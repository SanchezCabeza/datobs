#' Flag and interpolate time series data at regular intervals
#'
#' This function resamples a time series dataset to a regular interval (e.g.,
#' 30 minutes, 1 hour), interpolates gaps up to the interval size, and applies
#' quality control flags to numeric variables. Flags are based on plausible
#' ranges, z-score thresholds, and derivative thresholds.
#'
#' @param df Data frame with at least a `dateutc` column (POSIXct) and one or more numeric variables.
#' @param interval Character. Resampling interval (passed to [base::seq.POSIXt], e.g. `"30 min"`, `"1 hour"`).
#' @param ranges Named list of numeric vectors giving plausible min/max ranges
#'   for variables (e.g., `list(oxy = c(0, 20), tem = c(5, 40))`).
#'   Variables not listed will not be range-checked.
#' @param z_threshold Numeric. Z-score threshold for flagging questionable data (default: `3`).
#' @param derivative_thresholds Named list of numeric values giving derivative
#'   thresholds (units per second) for each variable.
#'
#' @return A data frame with `dateutc`, interpolated variables, and a `.flag`
#'   column for each variable.
#'   Flags:
#'   - `1 = Good`
#'   - `3 = Questionable/Suspect`
#'   - `4 = Bad`
#'   - `9 = Missing`
#'
#' @examples
#' \dontrun{
#' flagged <- flag_timeseries(
#'   df = combined,
#'   interval = "1 hour",
#'   ranges = list(oxy = c(0, 400), tem = c(5, 40)),
#'   derivative_thresholds = list(oxy = 0.2, tem = 0.1)
#' )
#' head(flagged)
#' }
#'
#' @importFrom dplyr arrange left_join mutate select
#' @importFrom lubridate floor_date ceiling_date
#' @importFrom stats approx sd
#' @export
flag_timeseries <- function(df,
                            interval = "30 min",
                            ranges = list(oxy = c(0, 400), tem = c(5, 40)),
                            z_threshold = 3,
                            derivative_thresholds = list(oxy = 0.2, tem = 0.1)) {
  if (!inherits(df$dateutc, "POSIXct")) df$dateutc <- lubridate::ymd_hms(df$dateutc)
  df <- dplyr::arrange(df, dateutc)

  # Build full continuous timeline
  start_time <- lubridate::floor_date(min(df$dateutc), unit = interval)
  end_time   <- lubridate::ceiling_date(max(df$dateutc), unit = interval)
  full_times <- seq.POSIXt(from = start_time, to = end_time, by = interval)
  result <- data.frame(dateutc = full_times)

  # Identify numeric variables to process
  vars <- names(df)[sapply(df, is.numeric)]
  vars <- setdiff(vars, "dateutc")

  # Interpolation helper
  interpolate_var <- function(var) {
    non_na_idx <- !is.na(df[[var]])
    if (sum(non_na_idx) >= 2) {
      interpolated <- stats::approx(
        x = df$dateutc[non_na_idx],
        y = df[[var]][non_na_idx],
        xout = full_times,
        method = "linear",
        rule = 2
      )
      out <- data.frame(dateutc = interpolated$x, value = interpolated$y)
    } else {
      out <- data.frame(dateutc = full_times, value = NA_real_)
    }
    names(out)[2] <- var
    return(out)
  }

  # Apply interpolation for each variable
  for (var in vars) {
    iv <- interpolate_var(var)
    result <- dplyr::left_join(result, iv, by = "dateutc")
  }

  # Apply QC checks
  apply_quality_checks <- function(df, var) {
    flag_col <- paste0(var, ".flag")
    df[[flag_col]] <- ifelse(is.na(df[[var]]), 9, 1)

    # Range checks
    if (var %in% names(ranges)) {
      range <- ranges[[var]]
      df[[flag_col]] <- ifelse(
        df[[flag_col]] == 1 & (df[[var]] < range[1] | df[[var]] > range[2]),
        4, df[[flag_col]]
      )
    }

    # Z-score check
    z_score <- (df[[var]] - mean(df[[var]], na.rm = TRUE)) /
      stats::sd(df[[var]], na.rm = TRUE)
    df[[flag_col]] <- ifelse(
      df[[flag_col]] == 1 & abs(z_score) > z_threshold,
      3, df[[flag_col]]
    )

    # Derivative check
    if (var %in% names(derivative_thresholds)) {
      derivative <- c(0, diff(df[[var]]) / as.numeric(diff(df$dateutc)))
      df[[flag_col]] <- ifelse(
        df[[flag_col]] == 1 & abs(derivative) > derivative_thresholds[[var]],
        3, df[[flag_col]]
      )
    }

    return(df)
  }

  for (var in vars) {
    result <- apply_quality_checks(result, var)
  }

  return(result)
}

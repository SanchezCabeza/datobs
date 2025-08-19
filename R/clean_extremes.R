#' Clean extreme values from the start and end of a time series for multiple variables iteratively
#'
#' Removes unrealistic extreme values from the first and last `n_check` records of
#' a time series for specified columns, based on a mean ± (`factor` × standard deviation)
#' threshold computed separately for each segment and column. Removes entire rows if
#' any specified column has an extreme value. Iterates until no more rows are removed
#' (convergence). Useful for eliminating spurious measurements when sensors are out of
#' the water at the start or end of a deployment.
#'
#' Data can be a CSV file or data frame. The first column is standardized to `dateutc`
#' (date-time in "yyyy/mm/dd hh:mm:ss" format). All columns in the input are retained
#' in the output.
#'
#' @param data A data frame or a character string with the path to a CSV file.
#' @param columns A vector of names or indices of columns to check for extremes
#'        (default: c("tem")). Must be numeric columns.
#' @param n_check Integer. Number of records to check at both the start and end
#'        (default: 48, representing 24 hours at 30-minute intervals).
#' @param factor Numeric. Number of standard deviations from the mean to define
#'        extremes (default: 3).
#'
#' @return A cleaned data frame with all original columns, with rows removed where
#'         any specified column has extreme values in the first or last `n_check` records.
#' @details
#' The algorithm:
#' 1. Reads the file (if a path is given) and renames the first column to `dateutc`.
#' 2. Validates that specified columns exist and contain numeric data.
#' 3. Removes rows with NA in any of the specified columns.
#' 4. Iteratively:
#'    a. Calculates mean and standard deviation for the first `n_check` values of each
#'       specified column and removes rows where any column's value is outside
#'       mean ± (`factor` × sd).
#'    b. Repeats for the last `n_check` values.
#'    c. Continues until no rows are removed in an iteration (convergence).
#' 5. Only data in the first and last `n_check` records can be removed.
#' 6. Retains all original columns in the output.
#'
#' @examples
#' \dontrun{
#' # Clean a CSV file with multiple variables
#' cleaned <- clean_extremes("mzt.1.1m.ot.20200101.20200202.csv", columns = c("tem", "oxy"))
#'
#' # Clean a data frame with multiple variables
#' df <- read.csv("mzt.1.1m.ot.20200101.20200202.csv")
#' cleaned <- clean_extremes(df, columns = c("tem", "oxy"), n_check = 48, factor = 3)
#' }
#'
#' @export
clean_extremes <- function(data, columns = c("tem"), n_check = 48, factor = 3) {
  # If a file name is given, read it
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }

  # Standardize first column to "dateutc"
  if (ncol(data) < 1) stop("Data must have at least one column (dateutc).")
  names(data)[1] <- "dateutc"

  # Validate columns
  for (col in columns) {
    if (is.numeric(col)) {
      if (col > ncol(data) || col <= 0) stop("Invalid column index: ", col)
      col_name <- names(data)[col]
    } else if (is.character(col)) {
      if (!(col %in% names(data))) stop("Invalid column name: ", col)
      col_name <- col
    } else {
      stop("Columns must be indices (numeric) or names (character)")
    }
    # Check if column is numeric
    if (!is.numeric(data[[col_name]])) stop(paste("Column", col_name, "must be numeric"))
  }

  # Get initial row count
  data_initial <- nrow(data)
  if (n_check * 2 > data_initial) {
    warning("n_check is too large for dataset size. No rows will be removed.")
    return(data)
  }

  # Remove rows with NA in any specified column
  na_rows <- rowSums(is.na(data[, columns, drop = FALSE])) > 0
  data <- data[!na_rows, , drop = FALSE]
  data_nomarks <- nrow(data)
  total_extremes_removed <- 0

  # If no data remains after removing NAs, return empty data frame
  if (nrow(data) == 0) {
    cat("No valid data after removing NAs.\n")
    return(data)
  }

  # Iterative cleaning until convergence
  iteration <- 0
  repeat {
    iteration <- iteration + 1
    cat(sprintf("Iteration %d: Starting with %d rows\n", iteration, nrow(data)))

    # Initialize indices to remove for this iteration
    remove_idx <- c()

    # Check extremes for each column
    for (col in columns) {
      col_data <- data[[col]]

      # Start indices
      idx_start <- 1:min(n_check, nrow(data))
      mean_start <- mean(col_data[idx_start], na.rm = TRUE)
      sd_start <- sd(col_data[idx_start], na.rm = TRUE)
      if (is.na(sd_start) || sd_start == 0) {
        warning(paste("No variability in", col, "for start segment in iteration", iteration, ". Skipping extreme check."))
        next
      }
      lower_start <- mean_start - factor * sd_start
      upper_start <- mean_start + factor * sd_start
      remove_start <- idx_start[col_data[idx_start] < lower_start | col_data[idx_start] > upper_start]

      # End indices
      idx_end <- (nrow(data) - min(n_check, nrow(data)) + 1):nrow(data)
      mean_end <- mean(col_data[idx_end], na.rm = TRUE)
      sd_end <- sd(col_data[idx_end], na.rm = TRUE)
      if (is.na(sd_end) || sd_end == 0) {
        warning(paste("No variability in", col, "for end segment in iteration", iteration, ". Skipping extreme check."))
        next
      }
      lower_end <- mean_end - factor * sd_end
      upper_end <- mean_end + factor * sd_end
      remove_end <- idx_end[col_data[idx_end] < lower_end | col_data[idx_end] > upper_end]

      # Combine indices to remove for this column
      remove_idx <- unique(c(remove_idx, remove_start, remove_end))
    }

    # Check for convergence
    if (length(remove_idx) == 0) {
      cat(sprintf("Iteration %d: No extremes found. Convergence reached.\n", iteration))
      break
    }

    # Remove rows where any column has extreme values
    data <- data[-remove_idx, , drop = FALSE]
    total_extremes_removed <- total_extremes_removed + length(remove_idx)
    cat(sprintf("Iteration %d: Removed %d extreme rows, %d rows remain\n", iteration, length(remove_idx), nrow(data)))

    # If no data remains, break
    if (nrow(data) == 0) {
      cat("No valid data after removing extremes.\n")
      break
    }
  }

  # Print final summary
  cat(sprintf(
    "%d initial rows, %d rows with NAs removed, %d total extremes removed across %d iterations\n",
    data_initial,
    data_initial - data_nomarks,
    total_extremes_removed,
    iteration
  ))

  return(data)
}

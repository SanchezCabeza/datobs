#' Combine time series data with arbitrary variables from multiple CSV files
#'
#' Reads all CSV files matching a pattern, combines into a single time series ordered by dateutc,
#' and removes duplicate dateutc records (keeping the first occurrence). Assumes the first column
#' is a datetime field (`dateutc`, format: %Y/%m/%d %H:%M:%S or similar). Remaining columns are
#' variables of interest (e.g., oxy, tem, sal, etc.).
#'
#' @param pattern File name pattern to match (default: ".csv" for all CSV files).
#' @param date_col Name of the date column (default: "dateutc").
#' @return A cleaned data frame with unique, ordered `dateutc` records and all variables.
#' @export
#' @examples
#' \dontrun{
#' cleaned <- combine_timeseries(pattern = "samo.*\\.csv$")
#' write.csv(cleaned, "samo_combined_cleaned.csv", row.names = FALSE)
#' }
combine_timeseries <- function(pattern = ".csv",
                               date_col = "dateutc") {
  # # test
  # pattern = "54m"
  # date_col = "dateutc"
  # i=1

  # List CSV files
  files <- list.files(pattern = pattern)
  if (length(files) == 0) stop("No files found matching pattern")

  data_list <- lapply(files, function(f) {
    cat("File:", f)
    data <- read.csv(f, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    cat(", columns: ", paste(names(data), collapse = ", "), "\n")

    # Validate columns
    if (!(date_col %in% names(data))) stop(paste("Date column", date_col, "not found in", f))

    # Parse date
    data[[date_col]] <- lubridate::ymd_hms(data[[date_col]])
    #data <- data[!is.na(data[[date_col]]), , drop = FALSE]
    # Rename first column explicitly to "dateutc"
    #names(data)[names(data) == date_col] <- "dateutc"
    data
  })

  # Ensure all files have the same columns
  col_sets <- lapply(data_list, names)
  if (length(unique(sapply(col_sets, paste, collapse = "|"))) > 1) {
    stop("Not all CSV files have the same column names in the same order")
  }

  # Combine into one data frame
  combined <- do.call(rbind, data_list)
  if (nrow(combined) == 0) stop("No valid data after combining files")

  # Sort by dateutc
  combined <- combined[order(combined$dateutc), ]

  # Remove duplicates by dateutc
  dup_count <- sum(duplicated(combined$dateutc))
  combined <- combined[!duplicated(combined$dateutc), ]

  cat(sprintf("Combined %d files, %d unique rows after removing %d duplicates\n",
              length(files), nrow(combined), dup_count))

  return(combined)
}


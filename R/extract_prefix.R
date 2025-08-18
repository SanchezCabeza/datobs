#' Extract temperature Prefix from a File Name
#'
#' Extracts the prefix ("t" or "t2") from the beginning of a given file name,
#' normalizing to lowercase. The function assumes the file name starts with "t",
#' "T", "t2", or "T2" and returns the lowercase equivalent. If no valid prefix is
#' found, it returns NA.
#'
#' @param file.name Character string specifying the file name or path from which to
#'   extract the prefix (e.g., "T2_data_2m.csv").
#'
#' @return A character string containing the matched prefix ("t" or "t2") in
#'   lowercase, or \code{NA} if the file name does not start with a valid prefix.
#'
#' @examples
#' # Example usage
#' extract_prefix("T2_temperature_2m_2023.csv")  # Returns "t2"
#' extract_prefix("t_data_6m.csv")               # Returns "t"
#' extract_prefix("no_prefix.csv")               # Returns NA
#'
#' @export
extract_prefix <- function(file.name) {
  # Define prefix pattern (matches t, T, t2, or T2 at the start)
  prefix_pattern <- "^([tT]|[tT]2)"

  # Extract base file name
  file_name <- basename(file.name)

  # Extract prefix
  prefix_match <- regmatches(file.name, regexpr(prefix_pattern, file.name))

  # Return lowercase prefix or NA
  if (length(prefix_match) > 0) {
    return(tolower(prefix_match[1]))
  } else {
    return(NA)
  }
}

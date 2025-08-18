#' Extract Depth from a File Name
#'
#' Extracts a depth substring (e.g., 1m, 2m, 6m, 8m, 14m, 20m, 23m, 30m, 40m, 54m)
#' from a given file name. The function searches for the first occurrence of a
#' predefined depth pattern in the file name and returns it. If no depth is found,
#' it returns NA.
#'
#' @param file.name Character string specifying the file name or path from which to
#'   extract the depth (e.g., "T2_data_2m.csv").
#'
#' @return A character string containing the first matched depth (e.g., "2m") or
#'   \code{NA} if no depth pattern is found in the file name.
#'
#' @examples
#' # Example usage
#' extract_depth("T2_temperature_2m_2023.csv")  # Returns "2m"
#' extract_depth("t2_no_depth.csv")             # Returns NA
#'
#' @export
extract_depth <- function(file.name) {
  # Define possible depth patterns (exact matches to the specified substrings)
  depth_pattern <- "(1m|2m|6m|8m|10m|14m|20m|23m|30m|40m|54m)"
  file_name <- basename(file.name)
  matches <- regmatches(file.name, gregexpr(depth_pattern, file.name))
  # Take the first match if multiple (assuming one primary depth per file)
  if (length(matches[[1]]) > 0) {
    return(depth <- matches[[1]][1])
  } else {
    return(NA)
  }
}


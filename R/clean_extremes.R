#' Clean extreme values only in the original start/end windows
#'
#' Removes rows *only* from the first and last `n_check` records (frozen windows),
#' if any specified column lies outside mean±factor*sd (or median±factor*MAD if robust).
#' Iterates until no rows are removed or `max_iter` reached. Middle rows are never touched.
#'
#' @param data data.frame or CSV path
#' @param columns character or integer vector of columns to check (numeric columns)
#' @param n_check integer, rows in each edge window (default 48)
#' @param factor numeric multiplier for threshold (default 3)
#' @param robust logical; if TRUE use median/MAD (default TRUE)
#' @param min_points integer; require ≥ this many points in a window to compute stats (default 8)
#' @param max_iter integer; safety cap on iterations (default 50)
#' @return cleaned data.frame with same columns
#' @export
clean_extremes <- function(data,
                           columns = c("tem"),
                           n_check = 48,
                           factor = 3,
                           robust = TRUE,
                           min_points = 8,
                           max_iter = 50) {
  # Read if path
  if (is.character(data)) {
    data <- read.csv(data, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  }
  stopifnot(is.data.frame(data))
  if (ncol(data) < 1) stop("Data must have at least one column.")

  # Standardize first column name
  names(data)[1] <- "dateutc"

  # Resolve columns to names and validate numeric
  col_names <- vapply(columns, function(col) {
    if (is.numeric(col)) {
      if (col <= 0 || col > ncol(data)) stop("Invalid column index: ", col)
      names(data)[col]
    } else if (is.character(col)) {
      if (!col %in% names(data)) stop("Invalid column name: ", col)
      col
    } else stop("`columns` must be names or indices.")
  }, character(1))
  for (cn in col_names) if (!is.numeric(data[[cn]])) stop("Column ", cn, " must be numeric.")

  n0 <- nrow(data)
  if (n0 == 0) return(data)

  # Remove rows with NA in *any* specified column (marks)
  keep_na <- rowSums(is.na(data[, col_names, drop = FALSE])) == 0
  removed_na <- sum(!keep_na)
  data <- data[keep_na, , drop = FALSE]
  if (nrow(data) == 0) {
    cat("All rows removed due to NA in selected columns.\n")
    return(data)
  }

  # Freeze original windows after NA removal
  N <- nrow(data)
  if (2 * n_check > N) {
    warning("n_check too large for dataset; reducing to floor(N/2).")
    n_check <- floor(N / 2)
  }
  start_idx_orig <- seq_len(n_check)
  end_idx_orig   <- seq.int(from = N - n_check + 1, to = N)

  # Alive mask to avoid index drift, windows stay fixed to original indices
  alive <- rep(TRUE, N)

  # Helpers for thresholds
  mad_sd <- function(x) stats::mad(x, constant = 1.4826, na.rm = TRUE)
  compute_thr <- function(x) {
    if (robust) {
      m <- median(x, na.rm = TRUE); s <- mad_sd(x)
    } else {
      m <- mean(x, na.rm = TRUE);   s <- stats::sd(x, na.rm = TRUE)
    }
    list(m = m, s = s)
  }

  iter <- 0
  total_ext <- 0
  repeat {
    iter <- iter + 1
    if (iter > max_iter) {
      warning("Reached max_iter; stopping.")
      break
    }

    to_remove <- integer(0)

    for (cn in col_names) {
      v <- data[[cn]]

      # START window (fixed original indices, current alive subset)
      win_start <- alive & (seq_len(N) %in% start_idx_orig)
      x <- v[win_start]
      if (sum(!is.na(x)) >= min_points) {
        th <- compute_thr(x)
        if (!is.na(th$s) && th$s > 0) {
          low <- th$m - factor * th$s
          high <- th$m + factor * th$s
          idx_flag <- which(win_start & (v < low | v > high))
          to_remove <- union(to_remove, idx_flag)
        }
      }

      # END window
      win_end <- alive & (seq_len(N) %in% end_idx_orig)
      x <- v[win_end]
      if (sum(!is.na(x)) >= min_points) {
        th <- compute_thr(x)
        if (!is.na(th$s) && th$s > 0) {
          low <- th$m - factor * th$s
          high <- th$m + factor * th$s
          idx_flag <- which(win_end & (v < low | v > high))
          to_remove <- union(to_remove, idx_flag)
        }
      }
    }

    if (length(to_remove) == 0) break
    alive[to_remove] <- FALSE
    total_ext <- total_ext + length(to_remove)

    # Stop early if the windows have become empty (no more alive rows in them)
    if (!any(alive & (seq_len(N) %in% start_idx_orig)) &&
        !any(alive & (seq_len(N) %in% end_idx_orig))) break
  }

  out <- data[alive, , drop = FALSE]
  cat(sprintf("%d initial rows, %d NA rows removed, %d extreme rows removed in %d iteration(s).\n",
              n0, removed_na, total_ext, iter))
  out
}


#' Calculate GC skew of the DNA sequences across a sliding window
#'
#' This function calculates the GC skew \eqn{(G - C) / (G + C)} of DNA sequences
#' across sliding windows of a specified size. The GC skew is computed for each
#' window, and the results are stored in a new list-column in the input data frame.
#'
#' @name gc_skew
#'
#' @param sequences_df A data frame with a column named `sequences` that contains
#'   DNA sequences as strings.
#' @param window An integer specifying the size of the sliding window for GC skew
#'   calculation. Default is 100.
#'
#' @return A data frame with an additional list-column named `gc_skew_windows`
#'   containing the GC skew value for each window of the corresponding sequence.
#' @export
#'
#' @examples
#' # Example Data Frame with DNA Sequences
#' sequences_df <- data.frame(
#'   sequences = c(
#'     "AGCTGCGCGTATCGTACGCGATCGTATCGCGATCGTATCGCG",
#'     "GGCGCGCTAGCTCGAGTCGCGCGGCTCGATAGCTCGTACGTAG"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate GC skew with a sliding window size of 10
#' result <- gc_skew(sequences_df, window = 10)
#'
#' # Print the result
#' print(result)
#'
utils::globalVariables(c("sequences", "gc_skew_windows"))
gc_skew <- function(sequences_df, window = 100) {
  if (!(is.numeric(window) && floor(window) == window) || window < 1) {
    stop("Kindly provide whole number as window.")
  }

  # Add headers if identifiers are not provided
  if (!"headers" %in% colnames(sequences_df)) {
    sequences_df$headers <- paste0("seq", seq_len(nrow(sequences_df)))
  }

  # To calculate GC skew of a window: (G - C) / (G + C)
  skew_calculate <- function(sequence_window) {
    g_count <- stringr::str_count(sequence_window, "G")
    c_count <- stringr::str_count(sequence_window, "C")
    gc_count <- g_count + c_count
    if (gc_count == 0) {
      return(0)
    }
    return((g_count - c_count) / gc_count)
  }

  # To calculate GC skew across the sliding window
  skew_window <- function(sequence, window) {
    sequence_length <- stringr::str_length(sequence)

    if (sequence_length < window) {
      stop("Sequence are shorter than window size specified")
    }

    windows <- seq(1, sequence_length - window + 1, by = 1)

    skew_values <- purrr::map_dbl(windows, function(start) {
      window_seq <- stringr::str_sub(sequence, start, start + window - 1)
      skew_calculate(window_seq)
    })

    return(skew_values)
  }

  sequences_df |>
    dplyr::mutate(gc_skew_windows = purrr::map(sequences, skew_window, window = window))
}

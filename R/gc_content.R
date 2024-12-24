#' Calculate GC content of the DNA sequences across a sliding window
#'
#' This function calculates the GC content of DNA sequences across sliding windows of a specified size.
#' The GC content is computed for each window, and the results are stored in a new column in the input data frame.
#'
#' @name gc_content
#'
#' @param sequences_df A data frame with the column named `sequences` that contains DNA sequences as strings.
#' @param window An integer specifying the the size of sliding window for GC content calculation
#'
#' @return A data frame with additional column named `gc_content_windows` that contains the GC content values
#' for each window in each sequence. The column is a list where each element contains the GC content for each
#' window of the corresponding sequence.
#' @export
#'
#' @examples
#' # Example Data Frame with DNA Sequences
#' sequences_df <- data.frame(
#'   sequences = c(
#'     "AGCTGCGCGTATCGTACGCGATCGTATCGCGATCGTATCGCG",
#'     "GGCGCGCTAGCTCGAGTCGCGCGGCTCGATAGCTCGTACGTAG"
#'   ),
#'   stringsAsFactors = FALSE  # Ensures strings are not converted to factors
#' )
#'
#' # Calculate GC content with a sliding window size of 10
#' result <- gc_content(sequences_df, window = 10)
#'
#' # Print the result
#' print(result)
#'
utils::globalVariables("sequences")
gc_content <- function(sequences_df, window = 100) {
  if((!(is.numeric(window) && floor(window) == window)) || window < 1){
    stop("Kindly provide whole number as window.")
  }
  # To calculate GC content of a window
  gc_calculate <- function(sequence_window) {
    gc_count <- stringr::str_count(sequence_window, "[GC]")
    total_bases <- stringr::str_length((sequence_window))
    return(gc_count / total_bases)
  }

  # To calculate GC content across sliding window
  gc_window <- function(sequence, window) {
    sequence_length <- stringr::str_length(sequence)

    if (sequence_length < window) {
      stop("Sequence are shorter than window size specified")
    }

    windows <- seq(1, sequence_length - window + 1, by = 1)

    gc_values <- purrr::map_dbl(windows, function(start) {
      window_seq <- stringr::str_sub(sequence, start, start + window - 1)
      gc_calculate(window_seq)
    })

    return(gc_values)
  }

  sequences_df |>
    dplyr::mutate(gc_content_windows = purrr::map(sequences, gc_window, window = window))
}



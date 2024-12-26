#' Detect CpG Islands in DNA Sequences
#'
#' This function identifies CpG islands in a set of DNA sequences by calculating the observed-to-expected CpG ratio and GC content within sliding windows. A CpG island is defined based on a threshold for both the GC content and the CpG ratio.
#'
#' @name detect_cpg_islands
#'
#' @param sequences_df A data frame containing the sequences to be analyzed. It must have a column named `sequences` containing the DNA sequences. Optionally, a `headers` column can be included for sequence identifiers.
#' @param window An integer specifying the window size (in base pairs) for sliding analysis. Default is 100.
#' @param gc_threshold A numeric value representing the threshold for GC content to classify a window as a potential CpG island. Default is 0.5.
#' @param cpg_threshold A numeric value representing the threshold for the observed-to-expected CpG ratio to classify a window as a CpG island. Default is 0.6.
#'
#' @return A ggplot object visualizing the GC content and CpG islands across the sequences. The plot will show lines for GC content with shaded regions indicating the detected CpG islands.
#' @importFrom purrr map2_dfr
#' @importFrom stringr str_count str_length str_sub
#' @importFrom ggplot2 ggplot geom_line geom_rect scale_fill_manual labs facet_wrap
#' @importFrom dplyr filter
#'
#' @export
#'
#' @examples
#' # Example usage:
#' sequences_df <- data.frame(
#'  headers = c("seq1", "seq2"),
#'  sequences = c("ATCGCGATCGCGATCG", "CGATCGCGATCG"),
#'  stringsAsFactors = FALSE
#' )
#'
#' # Plot CpG islands
#' detect_cpg_islands(sequences_df, window = 10, gc_threshold = 0.4, cpg_threshold = 0.5)
#'
utils::globalVariables(c("midpoint", "header", "start", "is_cpg_island", "end"))
detect_cpg_islands <- function(sequences_df, window = 100, gc_threshold = 0.5, cpg_threshold = 0.6) {
  # Validate the window size
  if (!(is.numeric(window) && floor(window) == window) || window < 1) {
    stop("Kindly provide a whole number as the window size.")
  }

  # Add headers if identifiers are not provided
  if (!"headers" %in% colnames(sequences_df)) {
    sequences_df$headers <- paste0("seq", seq_len(nrow(sequences_df)))
  }

  # Function to calculate observed-to-expected CpG ratio
  cpg_calculate <- function(sequence_window) {
    observed_cpg <- stringr::str_count(sequence_window, "CG")
    c_count <- stringr::str_count(sequence_window, "C")
    g_count <- stringr::str_count(sequence_window, "G")
    total_bases <- stringr::str_length(sequence_window)
    expected_cpg <- g_count * c_count / total_bases
    return(observed_cpg / expected_cpg)
  }

  # Function to calculate GC content
  gc_calculate <- function(sequence_window) {
    gc_count <- stringr::str_count(sequence_window, "[GC]")
    total_bases <- stringr::str_length(sequence_window)
    return(gc_count / total_bases)
  }

  # Function to process each sequence
  process_sequence <- function(sequence, header, window) {
    sequence_length <- stringr::str_length(sequence)

    if (sequence_length < window) {
      return(data.frame(
        header = header,
        start = integer(0),
        end = integer(0),
        gc_content = numeric(0),
        cpg_ratio = numeric(0),
        is_cpg_island = logical(0)
      ))
    }

    windows <- seq(1, sequence_length - window + 1, by = 1)
    data <- purrr::map_dfr(windows, function(start) {
      end <- start + window - 1
      window_seq <- stringr::str_sub(sequence, start, end)
      gc_content <- gc_calculate(window_seq)
      cpg_ratio <- cpg_calculate(window_seq)
      is_cpg_island <- gc_content >= gc_threshold && cpg_ratio >= cpg_threshold
      data.frame(
        header = header,
        start = start,
        end = end,
        midpoint = start + (window / 2),
        gc_content = gc_content,
        is_cpg_island = is_cpg_island
      )
    })
    return(data)
  }

  # Apply processing to all sequences
  results <- purrr::map2_dfr(sequences_df$sequences, sequences_df$headers, process_sequence, window = window)

  # Generate plot containing GC Content and CpG Islands
  plot <- ggplot2::ggplot(results, ggplot2::aes(x = midpoint, y = gc_content, group = header)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::geom_rect(
      data = results |>
        dplyr::filter(is_cpg_island),
      ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = Inf, fill = is_cpg_island),
      inherit.aes = FALSE,
      alpha = 0.2,
      color = NA
    ) +
    ggplot2::facet_wrap(~header, scales = "free_x") +
    ggplot2::scale_fill_manual(values = c("TRUE" = "red"), name = "CpG Island") +
    ggplot2::labs(
      title = "GC Content and CpG Islands",
      x = "Position in Sequence",
      y = "GC Content"
    )

  return(plot)
}

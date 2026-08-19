#' Visualize GC Skew Across Sequences
#'
#' This function creates a line plot to visualize GC skew across DNA sequences.
#' The GC skew is plotted over sliding windows, either in a combined layout with
#' a legend or in a facet-wrapped layout where each sequence is plotted separately.
#'
#' @name gc_skew_visualize
#'
#' @param sequences_df A data frame containing the sequences and their
#'   corresponding GC skew windows. It is expected that the data frame has at
#'   least two columns: `headers` (sequence identifiers) and `gc_skew_windows`
#'   (list-column of GC skew values).
#' @param combined A logical value. If `TRUE`, all sequences will be plotted on
#'   the same graph with a legend indicating the sequence name. If `FALSE`, each
#'   sequence will be plotted in its own facet (subplot), and the legend removed.
#'
#' @return A ggplot2 object showing GC skew across sequences.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sequences_df <- data.frame(
#'   headers = c("seq1", "seq2"),
#'   sequences = c("AGCTGCGCGTATCGTACGCG", "GGCGCGCTAGCTCGAGTCG"),
#'   stringsAsFactors = FALSE
#' )
#' skew_df <- gc_skew(sequences_df, window = 5)
#' gc_skew_visualize(skew_df, combined = TRUE)
#' gc_skew_visualize(skew_df, combined = FALSE)
#' }
#'
utils::globalVariables(c("gc_skew_windows", "headers", "position"))
gc_skew_visualize <- function(sequences_df, combined = FALSE) {

  df_long <- sequences_df |>
    tidyr::unnest_longer(gc_skew_windows) |>
    dplyr::group_by(headers) |>
    dplyr::mutate(position = dplyr::row_number()) |>
    dplyr::ungroup()

  plot <- ggplot2::ggplot(df_long, ggplot2::aes(x = position, y = gc_skew_windows)) +
    ggplot2::geom_line(ggplot2::aes(color = headers), show.legend = combined) +
    ggplot2::labs(title = "GC Skew Across Sequences", x = "Position", y = "GC Skew") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Sequences")) +
    ggplot2::theme_minimal()

  if (!combined) {
    plot <- plot + ggplot2::facet_wrap(~headers, scales = "free_x")
  }

  return(plot)
}

#' Visualize GC Content Across Sequences
#'
#' This function creates a line plot to visualize GC content across DNA sequences. The GC content is plotted over
#' sliding windows, either in a combined layout with a legend or in a facet-wrapped layout where each sequence is
#' plotted separately.
#'
#' @name gc_visualize
#'
#' @param sequences_df A data frame containing the sequences and their corresponding GC content windows. It is expected
#' that the data frame has at least two columns: `headers` (sequence identifiers) and `gc_content_windows` (list of GC content values).
#' @param combined A logical value. If `TRUE`, all sequences will be plotted on the same graph with a legend indicating the sequence
#' name. If `FALSE`, each sequence will be plotted in its own facet (subplot), and the legend will be removed.
#'
#' @return A ggplot2 object showing GC content across sequences. The plot will either display sequences in a combined
#' layout with a legend or in separate facets.
#'
#' @export
#'
#' @examples
#' # Example usage:
#' sequences_df <- data.frame(
#' headers = c("seq1", "seq2"),
#'   gc_content_windows = I(list(
#'     c(0.4, 0.45, 0.5, 0.42),
#'     c(0.35, 0.38, 0.4, 0.37)
#'   ))
#' )
#' # Plot with all sequences combined in a single plot
#' gc_visualize(sequences_df, combined = TRUE)
#'
#' # Plot with each sequence in a separate facet
#' gc_visualize(sequences_df, combined = FALSE)
#'
utils::globalVariables(c("gc_content_windows", "headers", "position"))
gc_visualize <- function(sequences_df, combined = FALSE) {

  df_long <- sequences_df |>
    tidyr::unnest_longer(gc_content_windows) |>
    dplyr::group_by(headers) |>
    dplyr::mutate(position = dplyr::row_number()) |>
    dplyr::ungroup()

    plot <- ggplot2::ggplot(df_long, ggplot2::aes(x = position, y = gc_content_windows)) +
      ggplot2::geom_line(ggplot2::aes(color = headers), show.legend = combined) +
      ggplot2::labs(title = "GC Content Across Sequences", x = "Position", y = "GC Content") +
      ggplot2::guides(color = ggplot2::guide_legend(title = "Seqeunces")) +
      ggplot2::theme_minimal()

    if (!combined) {
      plot <- plot + ggplot2::facet_wrap(~headers, scales = "free_x")
    }

    return(plot)
}



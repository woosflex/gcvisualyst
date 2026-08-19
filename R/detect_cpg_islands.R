#' Detect or compute CpG Islands in DNA Sequences
#'
#' These functions identify CpG islands in a set of DNA sequences by calculating
#' the observed-to-expected CpG ratio and GC content within sliding windows. A
#' window is flagged as a CpG island candidate when both its GC content and its
#' observed-to-expected CpG ratio meet user-supplied thresholds. Consecutive
#' (adjacent or overlapping) positive windows are then merged into a single
#' continuous island, and only islands whose merged length is at least
#' \code{min_length} base pairs are reported.
#'
#' @name detect_cpg_islands
#'
#' @param sequences_df A data frame containing the sequences to be analyzed. It
#'   must have a column named `sequences` containing the DNA sequences.
#'   Optionally, a `headers` column can be included for sequence identifiers.
#' @param window An integer specifying the window size (in base pairs) for the
#'   sliding analysis. Default is 100.
#' @param gc_threshold A numeric value representing the threshold for GC content
#'   to classify a window as a potential CpG island. Default is 0.5.
#' @param cpg_threshold A numeric value representing the threshold for the
#'   observed-to-expected CpG ratio to classify a window as a candidate CpG
#'   island. Default is 0.6.
#' @param min_length An integer specifying the minimum length (in base pairs) a
#'   merged island must have to be reported. Standard is 200 bp. Default is 200.
#' @param return_type A character string, one of \code{"plot"} (default),
#'   \code{"table"}, or \code{"list"}. See \strong{Value}.
#'
#' @return For \code{detect_cpg_islands()}: depending on \code{return_type},
#'   either a ggplot object visualizing the GC content and detected CpG islands
#'   ("plot"), a data.frame of detected islands ("table"), or a list containing
#'   both ("list"). The plot shows lines for GC content with shaded regions
#'   indicating the detected (merged, length-filtered) CpG islands.
#'
#' @return For \code{cpg_islands()}: a data.frame with one row per detected
#'   island and columns \code{header}, \code{start}, \code{end},
#'   \code{length}, and \code{mean_gc}.
#'
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
#'   headers = c("seq1", "seq2"),
#'   sequences = c("ATCGCGATCGCGATCG", "CGATCGCGATCG"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Plot CpG islands
#' detect_cpg_islands(sequences_df, window = 10, gc_threshold = 0.4, cpg_threshold = 0.5)
#'
#' # Get the detected islands as a data.frame
#' cpg_islands(sequences_df, window = 10, gc_threshold = 0.4, cpg_threshold = 0.5)
#'
utils::globalVariables(c("midpoint", "header", "start", "is_cpg_island", "end"))

#' Compute CpG islands as a data.frame
#'
#' @rdname detect_cpg_islands
#' @export
cpg_islands <- function(sequences_df, window = 100, gc_threshold = 0.5,
                        cpg_threshold = 0.6, min_length = 200) {
  windows <- cpg_islands_windows(sequences_df, window, gc_threshold, cpg_threshold)
  merge_islands(windows, min_length)
}

#' @rdname detect_cpg_islands
#' @export
detect_cpg_islands <- function(sequences_df, window = 100, gc_threshold = 0.5,
                               cpg_threshold = 0.6, min_length = 200,
                               return_type = c("plot", "table", "list")) {
  return_type <- match.arg(return_type)

  windows <- cpg_islands_windows(sequences_df, window, gc_threshold, cpg_threshold)
  islands <- merge_islands(windows, min_length)

  if (return_type == "table") {
    return(islands)
  }
  if (return_type == "list") {
    return(list(plot = build_cpg_plot(windows, islands), islands_df = islands))
  }

  build_cpg_plot(windows, islands)
}

# --- internal helpers -------------------------------------------------------

# Compute per-window GC content and CpG-island flag for every sequence.
cpg_islands_windows <- function(sequences_df, window, gc_threshold, cpg_threshold) {
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
        header = character(0),
        start = integer(0),
        end = integer(0),
        midpoint = numeric(0),
        gc_content = numeric(0),
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
  results <- purrr::map2_dfr(sequences_df$sequences, sequences_df$headers,
                             process_sequence, window = window)
  return(results)
}

# Merge consecutive / overlapping positive windows into continuous islands and
# keep only those at least min_length bp long.
merge_islands <- function(windows, min_length) {
  empty <- data.frame(
    header = character(0), start = integer(0), end = integer(0),
    length = integer(0), mean_gc = numeric(0), stringsAsFactors = FALSE
  )

  if (is.null(windows) || nrow(windows) == 0) {
    return(empty)
  }

  pos <- windows[windows$is_cpg_island, , drop = FALSE]
  if (nrow(pos) == 0) {
    return(empty)
  }

  island_accum <- list()

  for (h in unique(pos$header)) {
    sub <- pos[pos$header == h, ]
    sub <- sub[order(sub$start), ]

    cur_start <- sub$start[1]
    cur_end <- sub$end[1]
    gc_vals <- sub$gc_content[1]

    idx <- seq_len(nrow(sub))
    for (i in if (length(idx) > 1) idx[-1] else idx) {
      if (sub$start[i] <= cur_end + 1) {
        # Adjacent or overlapping -> merge into current island
        cur_end <- max(cur_end, sub$end[i])
        gc_vals <- c(gc_vals, sub$gc_content[i])
      } else {
        # Gap -> close the current island and start a fresh one
        island_accum[[length(island_accum) + 1]] <- data.frame(
          header = h, start = cur_start, end = cur_end,
          length = cur_end - cur_start + 1,
          mean_gc = mean(gc_vals), stringsAsFactors = FALSE
        )
        cur_start <- sub$start[i]
        cur_end <- sub$end[i]
        gc_vals <- sub$gc_content[i]
      }
    }

    # Close the final island for this header
    island_accum[[length(island_accum) + 1]] <- data.frame(
      header = h, start = cur_start, end = cur_end,
      length = cur_end - cur_start + 1,
      mean_gc = mean(gc_vals), stringsAsFactors = FALSE
    )
  }

  islands <- do.call(rbind, island_accum)
  islands <- islands[islands$length >= min_length, , drop = FALSE]
  rownames(islands) <- NULL
  return(islands)
}

# Build the GC content + CpG island plot.
build_cpg_plot <- function(windows, islands) {
  plot <- ggplot2::ggplot(windows, ggplot2::aes(x = midpoint, y = gc_content, group = header)) +
    ggplot2::geom_line(color = "blue") +
    ggplot2::facet_wrap(~header, scales = "free_x") +
    ggplot2::labs(
      title = "GC Content and CpG Islands",
      x = "Position in Sequence",
      y = "GC Content"
    )

  if (nrow(islands) > 0) {
    plot <- plot +
      ggplot2::geom_rect(
        data = islands,
        ggplot2::aes(xmin = start, xmax = end, ymin = 0, ymax = Inf),
        inherit.aes = FALSE,
        fill = "red",
        alpha = 0.2,
        color = NA
      )
  }

  return(plot)
}

#' Export CpG islands to a BED file
#'
#' Write a data.frame of CpG islands (as returned by \code{\link{cpg_islands}()}
#' or \code{\link{detect_cpg_islands}(..., return_type = "table")}) to a
#' Browser Extensible Data (BED) file.
#'
#' BED files use 0-based, half-open coordinates, whereas gcvisualyst reports
#' 1-based, closed coordinates. This function therefore converts the 1-based
#' \code{start} to a 0-based BED \code{chromStart} by writing \code{start - 1},
#' while keeping \code{end} unchanged, so that the BED span equals
#' \code{end - (start - 1)} and matches the original island \code{length}.
#'
#' @param islands A data.frame containing at least the columns \code{header},
#'   \code{start} and \code{end} (e.g. the output of \code{\link{cpg_islands}()}).
#'   If a \code{mean_gc} column is present it is emitted as an optional 4th BED
#'   column (rounded to 3 decimal places).
#' @param path A character string giving the file path to write to.
#' @param track A logical value. If \code{TRUE} (the default), a
#'   \code{track name=... type=bed} line is written as the first line of the file.
#'   This is required for direct upload to the UCSC Genome Browser.
#'
#' @return The input \code{path}, returned invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' islands <- cpg_islands(
#'   data.frame(headers = "s1", sequences = strrep("CG", 300)),
#'   window = 100, min_length = 200
#' )
#' write_bed(islands, path = "islands.bed")
#' }
write_bed <- function(islands, path, track = TRUE) {
  required <- c("header", "start", "end")
  missing <- setdiff(required, colnames(islands))
  if (length(missing) > 0) {
    stop(
      "Missing required column(s) in `islands`: ",
      paste(missing, collapse = ", "),
      ". `islands` must contain columns: ", paste(required, collapse = ", "),
      " (optionally `mean_gc`)."
    )
  }

  if (!is.numeric(islands$start) || !is.numeric(islands$end)) {
    stop("Columns `start` and `end` in `islands` must be numeric.")
  }
  if (any(islands$start < 1)) {
    stop("All `start` coordinates in `islands` must be >= 1 (1-based, closed).")
  }
  if (any(islands$end < islands$start)) {
    stop("All `end` coordinates in `islands` must be >= the corresponding `start`.")
  }

  # Name the track from the output file (URL-safe for the UCSC browser).
  track_name <- gsub("[^A-Za-z0-9_.-]", "_", tools::file_path_sans_ext(basename(path)))
  if (!nzchar(track_name)) {
    track_name <- "gcvisualyst_cpg_islands"
  }
  track_line <- paste0('track name="', track_name, '" type=bed')

  lines <- character(0)
  if (isTRUE(track)) {
    lines <- c(lines, track_line)
  }

  if (nrow(islands) > 0) {
    block <- data.frame(
      chrom     = islands$header,
      chromStart = islands$start - 1,
      chromEnd   = islands$end,
      stringsAsFactors = FALSE
    )
    if ("mean_gc" %in% colnames(islands)) {
      block$score <- round(islands$mean_gc, 3)
    }
    lines <- c(lines, apply(block, 1, paste, collapse = "\t"))
  }

  writeLines(lines, path)
  invisible(path)
}

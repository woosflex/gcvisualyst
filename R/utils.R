# This file contains helper functions for the main_function

# Helper functions for read_fasta() main function

read_sequences <- function(path) {
  if (is.na(path)) {
    stop("Fasta file path not provided.")
  }
  file <- readr::read_file(path)

  # Split the file into records on the ">" marker, discarding any leading
  # text (everything before the first ">" is ignored).
  records <- stringr::str_split_1(file, ">")
  records <- records[records != ""]

  headers <- character(length(records))
  sequences <- character(length(records))

  for (i in seq_along(records)) {
    lines <- stringr::str_split_1(records[i], "\n")
    # Header is the first line of the record, trimmed (preserving ':' and spaces).
    headers[i] <- stringr::str_trim(lines[1])
    # Sequence is the concatenation of all remaining lines with ALL whitespace
    # removed. This correctly reassembles multi-line FASTA (two or more lines of
    # sequence) without inserting spaces/garbage between fragments.
    if (length(lines) > 1) {
      seq_raw <- paste(lines[-1], collapse = "")
      sequences[i] <- stringr::str_remove_all(seq_raw, "\\s")
    } else {
      sequences[i] <- ""
    }
    sequences[i] <- stringr::str_to_upper(sequences[i])
  }

  return(data.frame(headers = headers, sequences = sequences, stringsAsFactors = FALSE))
}

validate_sequences <- function(sequences) {
  nt_chars <- c("A", "C", "G", "T")
  for (seq in sequences) {
    chars <- unlist(stringr::str_split(seq, ""))
    if (!all(chars %in% nt_chars)) {
      return(TRUE)
    }
  }
  return(FALSE)
}

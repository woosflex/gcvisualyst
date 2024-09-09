# This file contains helper functions for the main_function

# Helper functions for read_fasta() main function

read_sequences <-function(path) {
  if (is.na(path)) {
    stop("Fasta file path not provided.")
  }
  file <- readr::read_file(path)
  file <- file |>
    stringr::str_trim() |>
    stringr::str_split_1(">")
  file <- file[file != ""]
  sequence_df <- data.frame(Header = character(),
                            Sequence = character(),
                            stringsAsFactors = FALSE
  )
  for (seq in file) {
    header_and_seq <- unlist(stringr::str_split(seq, "\n"), 2)
    header <- header_and_seq[1]
    sequence <- stringr::str_to_upper(stringr::str_squish(header_and_seq[2]))
    sequence_df <- rbind(sequence_df, list(header, sequence))
  }
  colnames(sequence_df) <- c("headers", "sequences")
  return(sequence_df)
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

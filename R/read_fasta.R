read_fasta <- function(path = NA) {
  sequences <- read_sequences(path)
  if(validate_sequences(sequences$sequences)) {
    stop("Non-DNA nucleotide or Ambigous nucleotide character present in sequence.")
  }
  return(sequences)
}

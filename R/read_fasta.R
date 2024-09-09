#' Read FASTA files
#'
#' @param path Path to the FASTA file with DNA sequences
#'
#' @return A data frame with sequences
#' @export
#'
#' @examples
read_fasta <- function(path = NA) {
  sequences <- read_sequences(path)
  if(validate_sequences(sequences$sequences)) {
    stop("Non-DNA nucleotide or Ambigous nucleotide character present in sequence.")
  }
  return(sequences)
}

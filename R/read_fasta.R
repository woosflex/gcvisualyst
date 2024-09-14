#' Read FASTA files and validate DNA sequences
#'
#' This function reads a FASTA file containing DNA sequences and returns a data frame with the sequences.
#' It also validates that the sequences only contain valid DNA nucleotides (A, T, G, C). If any non-DNA
#' or ambiguous nucleotide characters are found, an error is raised.
#'
#' @param path A character string specifying the path to the FASTA file containing DNA sequences.
#' The file should be in standard FASTA format.
#'
#' @return A data frame with a column `sequences` containing the DNA sequences and column `headers` containing FASTA headers read from the FASTA file.
#' If the file contains any invalid or ambiguous nucleotide characters, the function will throw an error.
#' @export
#'
#' @examples
#' # Example usage:
#' # Assuming `example.fasta` is a valid FASTA file located in your working directory
#' fasta_path <- system.file("extdata", "example.fasta", package = "gcvisualyst")
#' sequences_df <- read_fasta(fasta_path)
#'
#' # View the sequences
#' print(sequences_df)
#'
read_fasta <- function(path = NA) {
  sequences <- read_sequences(path)
  if (nrow(sequences) == 0) {
    stop("Provided FASTA file is empty.")
  }
  if (validate_sequences(sequences$sequences)) {
    stop("Non-DNA nucleotide or Ambigous nucleotide character present in sequence.")
  }
  return(sequences)
}

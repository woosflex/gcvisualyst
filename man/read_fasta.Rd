% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fasta.R
\name{read_fasta}
\alias{read_fasta}
\title{Read FASTA files and validate DNA sequences}
\usage{
read_fasta(path = NA)
}
\arguments{
\item{path}{A character string specifying the path to the FASTA file containing DNA sequences.
The file should be in standard FASTA format.}
}
\value{
A data frame with a column \code{sequences} containing the DNA sequences and column \code{headers} containing FASTA headers read from the FASTA file.
If the file contains any invalid or ambiguous nucleotide characters, the function will throw an error.
}
\description{
This function reads a FASTA file containing DNA sequences and returns a data frame with the sequences.
It also validates that the sequences only contain valid DNA nucleotides (A, T, G, C). If any non-DNA
or ambiguous nucleotide characters are found, an error is raised.
}
\examples{
# Example usage:
# Assuming `example.fasta` is a valid FASTA file located in your working directory
fasta_path <- system.file("extdata", "example.fasta", package = "gcvisualyst")
sequences_df <- read_fasta(fasta_path)

# View the sequences
print(sequences_df)

}


<!-- README.md is generated from README.Rmd. Please edit that file -->

# gcvisualyst

<!-- badges: start -->
<!-- badges: end -->

The goal of gcvisualyst is to analyze and visualize the GC content of
DNA sequences across sliding windows and detect CpG islands within these
sequences. This tool provides a simple and effective way to calculate GC
content, identify CpG islands, and generate informative plots
illustrating variations in GC content and CpG islands.

Features:

- **GC Content Calculation**: Computes the GC content of DNA sequences
  across sliding windows of user-defined size.
- **CpG Island Detection**: Identifies regions with high GC content and
  CpG dinucleotide enrichment based on customizable thresholds.
- **Data Visualization**: Generates visualizations of GC content and CpG
  islands for multiple sequences, either combined in a single plot or as
  separate facets for each sequence.
- **Customizable Plots**: Provides options for combined or facet-wrapped
  layouts for easy comparison of multiple sequences.
- **Efficient Processing**: Utilizes `dplyr`, `ggplot2`, `stringr`, and
  `purrr` for efficient data manipulation and plotting.

## Installation

You can install the development version of gcvisualyst like so:

``` r
# install.packages("devtools")
devtools::install_github("woosflex/gcvisualyst")
```

## Usage

### Detecting CpG Islands

gcvisualyst can identify CpG islands within DNA sequences using the
`detect_cpg_islands()` function. This function calculates GC content and
the observed-to-expected CpG ratio within sliding windows. Regions
meeting user-defined thresholds for both metrics are identified as CpG
islands.

``` r
library(gcvisualyst)

# Demo DNA sequences
sequences_df <- data.frame(
  headers = c("seq1", "seq2"),
  sequences = c("ATCGCGATCGCGATCG", "CGATCGATCGCGCGAT"),
  stringsAsFactors = FALSE
)

# Detect CpG islands and generate a plot
cpg_plot <- detect_cpg_islands(sequences_df, window = 10, gc_threshold = 0.5, cpg_threshold = 0.6)
print(cpg_plot)
```

<img src="man/figures/README-example_cpg-1.png" width="100%" />

### Visualizing GC Content

gcvisualyst can be used to generate combined plots where all sequences
are plotted in a single graph by using the `combined = TRUE` argument in
the `gc_visualize()` function:

``` r
# Demo DNA sequences
sequences_df <- data.frame(
  headers = c("seq1", "seq2"),
  sequences = c(
    "AGCTGCGCGTATCGTACGCGATCGTATCGCGATCGTATCGCG",
    "GGCGCGCTAGCTCGAGTCGCGCGGCTCGATAGCTCGTACGTAG"
  ),
  stringsAsFactors = FALSE
)

# Calculate GC content with a sliding window of 10
gc_content_df <- gc_content(sequences_df, window = 10)

# Visualize GC content
gc_visualize(gc_content_df, combined = TRUE)
```

<img src="man/figures/README-example_gc-1.png" width="100%" />

Alternatively, you can generate separate plots for all sequences by
using the default `combined = FALSE` parameter of the `gc_visualize()`
function:

``` r
# Visualize GC content for each sequence
gc_visualize(gc_content_df)
```

<img src="man/figures/README-example_gc_individual-1.png" width="100%" />

test_that("gc_visualize returns a ggplot object with combined layout", {
  sequences_df <- data.frame(
    headers = c("seq1", "seq2"),
    gc_content_windows = I(list(
      c(0.4, 0.45, 0.5, 0.42),
      c(0.35, 0.38, 0.4, 0.37)
    )),
    stringsAsFactors = FALSE
  )

  plot_combined <- gc_visualize(sequences_df, combined = TRUE)

  expect_s3_class(plot_combined, "ggplot")
  }
)

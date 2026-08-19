test_that("gc_skew() returns 0 for an equal G/C window", {
  df <- data.frame(sequences = "GGCC", stringsAsFactors = FALSE)
  result <- gc_skew(df, window = 4)
  expect_true("gc_skew_windows" %in% colnames(result))
  expect_equal(result$gc_skew_windows[[1]], 0)
})

test_that("gc_skew() returns 1 for a pure G window", {
  df <- data.frame(sequences = "GG", stringsAsFactors = FALSE)
  result <- gc_skew(df, window = 2)
  expect_equal(result$gc_skew_windows[[1]], 1)
})

test_that("gc_skew() returns -1 for a pure C window", {
  df <- data.frame(sequences = "CC", stringsAsFactors = FALSE)
  result <- gc_skew(df, window = 2)
  expect_equal(result$gc_skew_windows[[1]], -1)
})

test_that("gc_skew() matches a hand-computed value", {
  # "GGCGCGCG": G=5, C=3 -> skew = (5-3)/(5+3) = 2/8 = 0.25
  df <- data.frame(sequences = "GGCGCGCG", stringsAsFactors = FALSE)
  result <- gc_skew(df, window = 8)
  expect_equal(result$gc_skew_windows[[1]], 0.25)
})

test_that("gc_skew() returns a value per sliding window", {
  # "GGC" with window = 2 gives windows "GG" (=1) and "GC" (=0)
  df <- data.frame(sequences = "GGC", stringsAsFactors = FALSE)
  result <- gc_skew(df, window = 2)
  expect_equal(result$gc_skew_windows[[1]], c(1, 0))
  expect_length(result$gc_skew_windows[[1]], 2)
})

test_that("gc_skew() handles sequences shorter than the window", {
  df <- data.frame(sequences = "AG", stringsAsFactors = FALSE)
  expect_error(gc_skew(df, 10))
})

test_that("gc_skew_visualize() returns a ggplot object", {
  df <- data.frame(
    headers = c("seq1", "seq2"),
    gc_skew_windows = I(list(c(0.5, 0.2, 0.1), c(-0.2, -0.4, -0.1))),
    stringsAsFactors = FALSE
  )
  plot_combined <- gc_skew_visualize(df, combined = TRUE)
  expect_s3_class(plot_combined, "ggplot")

  plot_facet <- gc_skew_visualize(df, combined = FALSE)
  expect_s3_class(plot_facet, "ggplot")
})

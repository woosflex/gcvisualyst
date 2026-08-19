test_that("cpg_islands() reports nothing for a short high-GC region below min_length", {
  # A 10 bp all-CG region is high-GC/enriched but, with the default window=100,
  # the resulting island (10 bp) is far below min_length = 200.
  df <- data.frame(
    headers = "s1",
    sequences = "CGCGCGCGCG",
    stringsAsFactors = FALSE
  )
  res <- cpg_islands(df, window = 10, min_length = 200)
  expect_equal(nrow(res), 0)
})

test_that("cpg_islands() merges adjacent positive windows into a single island", {
  # 'CG' repeated 5 times = "CGCGCGCGCG", window = 2. Every "CG" window is
  # flagged and the windows are adjacent, so they all merge into ONE island
  # spanning the whole sequence (start 1, end 10).
  df <- data.frame(
    headers = "s1",
    sequences = "CGCGCGCGCG",
    stringsAsFactors = FALSE
  )
  res <- cpg_islands(df, window = 2, min_length = 5)
  expect_equal(nrow(res), 1)
  expect_equal(res$header, "s1")
  expect_equal(res$start, 1)
  expect_equal(res$end, 10)
  expect_equal(res$length, 10)
  expect_true(res$mean_gc >= 0.99)
})

test_that("cpg_islands() returns the expected data.frame columns", {
  df <- data.frame(
    headers = "s1",
    sequences = strrep("CG", 300), # 600 bp of high-GC content
    stringsAsFactors = FALSE
  )
  res <- cpg_islands(df, window = 100, min_length = 200)
  expect_true(is.data.frame(res))
  expect_equal(colnames(res), c("header", "start", "end", "length", "mean_gc"))
  expect_equal(nrow(res), 1)
  expect_equal(res$length, 600)
  expect_equal(res$start, 1)
  expect_equal(res$end, 600)
})

test_that("cpg_islands() respects min_length filtering on a longer region", {
  df <- data.frame(
    headers = "s1",
    sequences = strrep("CG", 300), # 600 bp
    stringsAsFactors = FALSE
  )
  # With a strict min_length the island is not reported...
  expect_equal(nrow(cpg_islands(df, window = 100, min_length = 1000)), 0)
  # ...but with a lenient min_length it is.
  expect_equal(nrow(cpg_islands(df, window = 100, min_length = 100)), 1)
})

test_that("detect_cpg_islands() handles return_type = 'table'", {
  df <- data.frame(
    headers = "s1",
    sequences = strrep("CG", 300),
    stringsAsFactors = FALSE
  )
  res <- detect_cpg_islands(df, window = 100, min_length = 200, return_type = "table")
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), 1)
  expect_true(all(c("header", "start", "end", "length", "mean_gc") %in% colnames(res)))
})

test_that("detect_cpg_islands() handles return_type = 'list'", {
  df <- data.frame(
    headers = "s1",
    sequences = strrep("CG", 300),
    stringsAsFactors = FALSE
  )
  res <- detect_cpg_islands(df, window = 100, min_length = 200, return_type = "list")
  expect_type(res, "list")
  expect_s3_class(res$plot, "ggplot")
  expect_true(is.data.frame(res$islands_df))
  expect_equal(nrow(res$islands_df), 1)
})

test_that("detect_cpg_islands() defaults to a plot", {
  df <- data.frame(
    headers = "s1",
    sequences = strrep("CG", 300),
    stringsAsFactors = FALSE
  )
  res <- detect_cpg_islands(df, window = 100)
  expect_s3_class(res, "ggplot")
})

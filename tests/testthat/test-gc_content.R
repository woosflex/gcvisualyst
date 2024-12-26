test_that("gc_content correctly calculates GC content for the sequences", {
  sequences_df <- data.frame(
    headers = c("seq1", "seq2"),
    sequences = c(
      "AGCTGCGCGTATCGTACGCGATCGTATCGCGATCGTATCGCG",
      "GGCGCGCTAGCTCGAGTCGCGCGGCTCGATAGCTCGTACGTAG"
    ),
    stringsAsFactors = FALSE
  )

  result_df <- gc_content(sequences_df, window = 10)

  expect_true(is.data.frame(result_df))
  expect_true("gc_content_windows" %in% colnames(result_df))

  expect_length(result_df$gc_content_windows[[1]], 33)
  expect_length(result_df$gc_content_windows[[2]], 34)

  expect_true(all(result_df$gc_content_windows[[1]] >= 0 & result_df$gc_content_windows[[1]] <= 1))
  expect_true(all(result_df$gc_content_windows[[2]] >= 0 & result_df$gc_content_windows[[2]] <= 1))
})

test_that("gc_content() handles sequences shorter than window", {

  sequences_df <- data.frame(
    sequences = c(
      "AGCTG"
    ),
    stringsAsFactors = FALSE
  )

  expect_error(gc_content(sequences_df, 10))
})

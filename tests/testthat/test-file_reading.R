test_that("read_fasta() returns correct output for valid sequences", {
  valid_fasta <- read_fasta("helper_files/test1.fasta")
  expect_true(is.data.frame(valid_fasta))
  expect_equal(colnames(valid_fasta), c("headers", "sequences"))
  expect_true(all(valid_fasta$sequences != ""))
  expect_true(all(valid_fasta$headers != ""))
})

test_that("read_fasta() shows error for incorrect sequence input", {
  expect_error(read_fasta("helper_files/test2.fasta"))
})

test_that("read_fasta() handles empty FASTA files", {
  expect_error(read_fasta("helper_files/test3.fasta"))
})

test_that("read_fasta() correctly parses multi-line FASTA sequences", {
  df <- read_fasta("helper_files/test_multiline.fasta")

  # Two records should be returned
  expect_equal(nrow(df), 2)
  expect_equal(colnames(df), c("headers", "sequences"))

  # Headers are preserved in full, including ':' and spaces
  expect_equal(df$headers[1], "NC_000001.1:1-60 Example multi-line sequence header")
  expect_equal(df$headers[2], "NC_000002.1:1-50 Another multi-line record")

  # Sequences are concatenated without any whitespace or garbage
  expect_equal(df$sequences[1], paste0(rep("C", 60), collapse = ""))
  expect_equal(df$sequences[2], paste0(rep("G", 50), collapse = ""))
  expect_false(grepl("\\s", df$sequences[1]))
  expect_false(grepl("\\s", df$sequences[2]))
})

test_that("read_fasta() concatenates sequences spread over multiple lines", {
  df <- read_fasta("helper_files/test_multiline.fasta")
  # Sequence 1 is 40 C's (line 1) + 20 C's (line 2) = 60 C's
  expect_equal(nchar(df$sequences[1]), 60)
  expect_equal(df$sequences[1], strrep("C", 60))
})

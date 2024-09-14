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

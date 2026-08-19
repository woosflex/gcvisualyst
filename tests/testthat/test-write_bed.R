test_that("write_bed() writes correct BED coordinates for a known island", {
  # 1-based closed island start=1, end=600 -> BED 0-based half-open 0..600
  islands <- data.frame(
    header = "chr1", start = 1, end = 600,
    length = 600, mean_gc = 0.62,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".bed")
  write_bed(islands, path)

  lines <- readLines(path)
  expect_match(lines[1], "^track ")
  # chrom, chromStart, chromEnd, mean_gc(rounded to 3 dp)
  expect_equal(lines[2], "chr1\t0\t600\t0.62")
})

test_that("write_bed() emits BED3 (no score) when mean_gc is absent", {
  islands <- data.frame(
    header = "chr2", start = 11, end = 250,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".bed")
  write_bed(islands, path, track = FALSE)

  expect_equal(readLines(path), "chr2\t10\t250")
})

test_that("write_bed() handles empty islands robustly", {
  empty <- data.frame(
    header = character(0), start = integer(0), end = integer(0),
    length = integer(0), mean_gc = numeric(0)
  )

  # With a track line, the file contains only the track line.
  path_track <- tempfile(fileext = ".bed")
  write_bed(empty, path_track, track = TRUE)
  expect_match(readLines(path_track), "^track ")

  # Without a track line, an empty (valid) file is produced.
  path_empty <- tempfile(fileext = ".bed")
  write_bed(empty, path_empty, track = FALSE)
  expect_equal(readLines(path_empty), character(0))
  expect_equal(file.info(path_empty)$size, 0)
})

test_that("write_bed() returns the path invisibly", {
  islands <- data.frame(
    header = "chr1", start = 1, end = 600,
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".bed")
  expect_invisible(write_bed(islands, path))
  expect_true(file.exists(path))
})

test_that("write_bed() errors when required columns are missing", {
  expect_error(
    write_bed(data.frame(header = "chr1", end = 600), tempfile()),
    "start"
  )
  expect_error(
    write_bed(data.frame(start = 1, end = 600), tempfile()),
    "header"
  )
  expect_error(
    write_bed(data.frame(header = "chr1", start = 1), tempfile()),
    "end"
  )
})

test_that("write_bed() validates coordinates", {
  expect_error(
    write_bed(data.frame(header = "chr1", start = 0, end = 10), tempfile()),
    "start"
  )
  expect_error(
    write_bed(data.frame(header = "chr1", start = 50, end = 10), tempfile()),
    "start.*end|end.*start"
  )
})

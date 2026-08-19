test_that("bundled Shiny app sources and builds its UI and server", {
  app_file <- system.file("shiny", "app.R", package = "gcvisualyst")
  expect_true(file.exists(app_file))

  # Source the app in an isolated environment. In a non-interactive session
  # shinyApp() returns the app object rather than trying to launch it.
  app_env <- new.env(parent = globalenv())
  app_val <- source(app_file, local = app_env)$value

  expect_s3_class(app_val, "shiny.appobj")
  expect_true(exists("ui", envir = app_env))
  expect_true(exists("server", envir = app_env))
  expect_true(is.function(app_env$server))

  # The UI should be a shiny tag tree with the expected output/input bindings.
  ui_html <- as.character(app_env$ui)
  for (needle in c("gc_plot", "skew_plot", "islands_table",
                   "download_csv", "download_bed", "gc_threshold",
                   "cpg_threshold", "min_length", "window", "analyze")) {
    expect_match(ui_html, needle, fixed = TRUE, info = paste("missing", needle))
  }
})

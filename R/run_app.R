#' Launch the GC Visualyst Shiny Application
#'
#' This function launches the interactive Shiny web application bundled with the
#' gcvisualyst package. The app lets users calculate and visualize GC content
#' either by typing in DNA sequences directly or by uploading a FASTA file.
#'
#' @param host The host address on which the app should run. Defaults to
#'   \code{"127.0.0.1"} so that the app is only reachable from the local machine.
#' @param port The port to use for the application. If \code{NULL} (the default),
#'   Shiny picks an available port automatically.
#' @param ... Additional arguments passed on to \code{\link[shiny]{runApp}}
#'   (e.g. \code{launch.browser}).
#'
#' @return This function is called for its side effect of launching the Shiny
#'   application. It blocks until the app is stopped.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch the app on the default local host and an automatically chosen port
#' run_app()
#'
#' # Launch the app on a specific port and open a browser automatically
#' run_app(port = 3838, launch.browser = TRUE)
#' }
run_app <- function(host = "127.0.0.1", port = NULL, ...) {
  app_dir <- system.file("shiny", package = "gcvisualyst")
  if (app_dir == "") {
    stop("Could not find the bundled Shiny application. ",
         "Please reinstall the gcvisualyst package.")
  }
  shiny::runApp(appDir = app_dir, host = host, port = port, ...)
}

#' Run the \code{behaviourchange} Shiny app
#' @author Ty Stanford and Dot Dumuid
#' @description Run the \code{behaviourchange} Shiny app
#'
#' @example
#' behaviourchange::runBCS()
#'
#' @export
#'
runBCS <- function() {
  appDir <- system.file("app", package = "behaviourchange")
  if (appDir == "") {
    stop(
      paste(
        "Could not find the app/ directory loaded from the inst/ directory.",
        "Try re-installing `behaviourchange`.",
        call. = FALSE
      )
    )
  }

  shiny::runApp(appDir, display.mode = "normal")
}

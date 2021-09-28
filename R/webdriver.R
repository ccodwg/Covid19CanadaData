#' Functions for RSelenium
#'
#' Navigate to a webpage using RSelenium server and tidy up afterwards.
#' @name webdriver
NULL

#' @param url The URL to navigate to.
#' @param headless Run in headless mode? Default: TRUE. Non-headless mode can
#' be useful for debugging.
#' @param check See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @param verbose See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @rdname webdriver
#' @export
webdriver_open <- function(url, headless = TRUE, check = FALSE, verbose = FALSE) {

  # start Firefox
  if (headless) {
    ec <- list(
      "moz:firefoxOptions" = list(
        args = list('--headless')))
  } else {
    ec <- list()
  }
  webdriver <- RSelenium::rsDriver(
    browser = "firefox",
    check = check,
    verbose = verbose,
    extraCapabilities = ec)

  # navigate to relevant content
  webdriver$client$navigate(url)

  # return webdriver
  return(webdriver)
}

#' @param webdriver Selenium server object.
#' @rdname webdriver
#' @export
webdriver_close <- function(webdriver) {
  webdriver$client$close()
  out <- utils::capture.output(webdriver$server$stop())
  if (!grepl("TRUE", out)) {
    warning("Something went wrong when closing the Selenium server.")
  }
}

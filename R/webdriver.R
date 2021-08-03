#' Functions for RSelenium
#'
#' Navigate to a webpage using RSelenium server and tidy up afterwards.
#' @name webdriver
NULL

#' @param url The URL to navigate to.
#' @param check See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @param verbose See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @rdname webdriver
#' @export
webdriver_open <- function(url, check = FALSE, verbose = FALSE) {

  # start headless Firefox
  webdriver <- RSelenium::rsDriver(
    browser = "firefox",
    check = check,
    verbose = verbose,
    extraCapabilities = list(
      "moz:firefoxOptions" = list(
        args = list('--headless'))))

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
    stop("Something went wrong when closing the Selenium server.")
  }
}

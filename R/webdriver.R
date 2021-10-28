#' Functions for RSelenium
#'
#' Navigate to a webpage using RSelenium server and tidy up afterwards.
#' The primary command is \code{webdriver_get}.
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

#' Helper functions for webdriver navigation
#'
#' @name webdriver
NULL

#' webdriver_wait_for_element: Wait until element is visible
#' @param webdriver Selenium server object.
#' @param By Type of selector to use.
#' @param value Value for selector.
#' @param timeout Timeout in seconds.
#' @rdname webdriver
#' @export
webdriver_wait_for_element <- function(webdriver, By, value, timeout) {
  # record start time
  start_time <- Sys.time()
  # check for element
  element <- NULL
  while (is.null(element)) {
    element <- suppressWarnings(
      tryCatch({webdriver$client$findElement(using = By, value = value)},
               error = function(e){NULL}))
    # check timeout
    if (as.numeric(Sys.time() - start_time) > timeout) {
      stop("Timeout exceeded while waiting for element to be visible.")
    }
  }
  # return element
  element
}

#' @param webdriver Selenium server object.
#' @param uuid The UUID of the dataset from datasets.json.
#' @rdname webdriver
#' @export
webdriver_commands <- function(webdriver, uuid) {
  switch(
    uuid,
    "66fbe91e-34c0-4f7f-aa94-cf6c14db0158" = {
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[2]/a",
        10)
      # wait for page to load before clicking tab
      Sys.sleep(5)
      elm$clickElement()
      # wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    },
    "454de458-f7b4-4814-96a6-5a426f8c8c60" = {
      # click on vaccine doses tab dropdown (when element is visible)
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/a",
        10)
      # wait for page to load before clicking tab
      Sys.sleep(5)
      elm$clickElement()
      # click tab
      webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/ul/li[2]/a",
        10)$clickElement()
      # wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    },
    {
      # by default, just wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    }
  )
}

#' @param uuid The UUID of the dataset from datasets.json.
#' @rdname webdriver
#' @return The function \code{webdriver_open} returns an HTML object created by \code{\link[xml2]{read_html}}.
#' @export
webdriver_get <- function(uuid) {
  # check that dataset really requires webdriver
  js <- get_dataset_arg(uuid, "js")
  if (is.na(js) | js == "False") {
    stop("This dataset does not need to be loaded using webdriver.")
  }
  # get URL of dataset
  url <- get_dataset_url(uuid)
  # open webdriver
  web <- webdriver_open(url)
  # run commands to drive webdriver
  webdriver_commands(web, uuid)
  # extract HTML
  ds <- web$client$getPageSource()
  # tidy up
  webdriver_close(web)
  # return HTML
  xml2::read_html(ds[[1]])
}

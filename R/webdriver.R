#' Functions for RSelenium
#'
#' Navigate to a webpage using RSelenium server and tidy up afterwards.
#' This is used for pages requiring JavaScript to load correctly.
#' The primary command is \code{webdriver_get}, which is called by
#' \code{\link[Covid19CanadaData]{dl_dataset}} when JavaScript is required. Note
#' that this function requires a recent version of the Chrome/Chromium
#' browser. If the latest version of Chrome/Chromium is not used, the version of
#' Chromedriver must be manually specified in \code{webdriver_options$chromever}.
#' Run \code{binman::list_versions("chromedriver")} for available versions.
#' @name webdriver
NULL

#' @param uuid The UUID of the dataset from datasets.json.
#' @param webdriver_options Optionally, arguments for \code{webdriver_open} provided
#' as a named list.
#' @rdname webdriver
#' @return The function \code{webdriver_open} returns an HTML object created by \code{\link[xml2]{read_html}}.
#' @export
webdriver_get <- function(uuid,
                          webdriver_options = list(
                            chromever = "latest",
                            check = TRUE,
                            verbose = FALSE)) {
  # check that dataset really requires webdriver
  js <- get_dataset_arg(uuid, "js")
  if (is.na(js) | js == "False") {
    stop("This dataset does not need to be loaded using webdriver.")
  }
  # get URL of dataset
  url <- get_dataset_url(uuid)
  # open webdriver with specified options
  if (is.list(webdriver_options)) {
    if (!webdriver_options$chromever %in% c("latest", unlist(binman::list_versions("chromedriver"))))
      webdriver_options$chromever <- "latest"
    if (!isFALSE(webdriver_options$check)) {
      webdriver_options$check <- TRUE
    }
    if (!isTRUE(webdriver_options$verbose)) {
      webdriver_options$verbose <- FALSE
    }
  } else {
    webdriver_options <- list(
      chromever = "latest",
      check = TRUE,
      verbose = FALSE)
  }
  web <- webdriver_open(url,
                        chromever = webdriver_options$chromever,
                        check = webdriver_options$check,
                        verbose = webdriver_options$verbose)
  # run commands to drive webdriver
  webdriver_commands(web, uuid)
  # extract HTML
  ds <- web$client$getPageSource()
  # tidy up
  webdriver_close(web)
  # return HTML
  xml2::read_html(ds[[1]])
}

#' @param url The URL to navigate to.
#' @param chromever See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @param check See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @param verbose See parameter in \code{\link[RSelenium]{rsDriver}}.
#' @rdname webdriver
#' @export
webdriver_open <- function(url,
                           chromever = "latest",
                           check = TRUE,
                           verbose = FALSE) {

  # start Chrome/Chromium
  ec <- list(chromeOptions = list(
      args = c('--headless', '--disable-gpu')))
  webdriver <- RSelenium::rsDriver(
    browser = "chrome",
    chromever = chromever,
    check = check,
    verbose = verbose,
    extraCapabilities = ec,
    geckover = NULL,
    iedrver = NULL,
    phantomver = NULL)

  # navigate to relevant content
  tryCatch(
    webdriver$client$navigate(url),
    error = function (e) {
      print(e)
      warning("RSelenium had an error. Make sure a recent version of Chrome/Chromium is installed. If it is installed but is not the latest version, the Chromedriver version must be specified. See `?webdriver_get` for more details.")
      webdriver$client$close() # try to close Selenium server
    }
  )

  # return webdriver
  return(webdriver)
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
#' @rdname webdriver
#' @export
webdriver_close <- function(webdriver) {
  webdriver$client$close()
  out <- utils::capture.output(webdriver$server$stop())
  if (!grepl("TRUE", out)) {
    warning("Something went wrong when closing the Selenium server.")
  }
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

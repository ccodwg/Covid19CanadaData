#' Functions for RSelenium webdriver
#'
#' Navigate to a webpage using RSelenium server and the Chrome browser running
#' in a Docker container, acquire the rendered HTML source and tidy up
#' afterwards. The primary command is \code{webdriver_get}. This command is used
#' by \code{\link[Covid19CanadaData]{dl_dataset}} for HTML pages requiring
#' JavaScript to render.
#' @name webdriver
NULL

#' @param uuid The UUID of the dataset from datasets.json.
#' @param host Optional. The URL of the Docker daemon. See parameter in
#' \code{\link[stevedore]{docker_client}} for details/defaults. The default for
#' Linux has been changed for this function: "unix:///run/user/1000/docker.sock"
#' (i.e., rootless Docker). Can be set in `options(docker_host = "path/to/host")`
#' (this will override the provided `host` parameter).
#' @param port Optional. The host port for Docker. If not provided, a random
#' open port will be selected using \code{\link[httpuv]{randomPort}}.
#' @param remoteServerAddr IP of the remote server. One of "localhost" (default)
#' or "host-gateway" (for Docker-in-Docker). See parameter in \code{\link[RSelenium]{remoteDriver}}.
#' Can be set in `options(docker_remoteServerAddr = c("localhost", "host-gateway"))`
#' (this will override the provided `remoteServerAddr` parameter).
#' @rdname webdriver
#' @return The function \code{webdriver_get} returns an HTML object created by \code{\link[xml2]{read_html}}.
#' @export
webdriver_get <- function(uuid, host, port, remoteServerAddr = "localhost") {

  # check that dataset really requires webdriver
  js <- get_dataset_arg(uuid, "js")
  if (is.na(js) | js == "False") {
    stop("This dataset does not need to be loaded using webdriver.")
  }
  # verify remoteServerAddr
  match.arg(remoteServerAddr, c("localhost", "host-gateway"), several.ok = FALSE)
  # get URL of dataset
  url <- get_dataset_url(uuid)
  # open webdriver
  if (!missing(host)) {
    if (!missing(port)) {
      web <- webdriver_open(url, host, port, remoteServerAddr = remoteServerAddr)
    } else {
      web <- webdriver_open(url, host, remoteServerAddr = remoteServerAddr)
    }
  } else {
    if (!missing(port)) {
      web <- webdriver_open(url, port = port, remoteServerAddr = remoteServerAddr)
    } else {
      web <- webdriver_open(url, remoteServerAddr = remoteServerAddr)
    }
  }
  # run commands to drive webdriver
  webdriver_commands(web, uuid)
  # extract HTML
  ds <- web$getPageSource()
  # tidy up
  if (!missing(host)) {
    webdriver_close(web, host)
  } else {
    webdriver_close(web)
  }
  # return HTML
  xml2::read_html(ds[[1]])
}

#' @param url The URL to navigate to.
#' @param host Optional. The URL of the Docker daemon. See parameter in
#' \code{\link[stevedore]{docker_client}} for details/defaults. The default for
#' Linux has been changed for this function: "unix:///run/user/1000/docker.sock"
#' (i.e., rootless Docker). Can be set in `options(docker_host = "path/to/host")`
#' (this will override the provided `host` parameter).
#' @param port Optional. The host port for Docker. If not provided, a random
#' open port will be selected using \code{\link[httpuv]{randomPort}}.
#' @param remoteServerAddr IP of the remote server. One of "localhost" (default)
#' or "host-gateway" (for Docker-in-Docker). See parameter in \code{\link[RSelenium]{remoteDriver}}.
#' Can be set in `options(docker_remoteServerAddr = c("localhost", "host-gateway"))`
#' (this will override the provided `remoteServerAddr` parameter).
#' @rdname webdriver
#' @export
webdriver_open <- function(url, host, port, remoteServerAddr = "localhost") {

  # check if docker_remoteServerAddr is set in options (this will override the provided remoteServerAddr parameter)
  docker_remoteServerAddr <- getOption("docker_remoteServerAddr")
  if (!is.null(docker_remoteServerAddr)) {
    remoteServerAddr <- docker_remoteServerAddr
  }

  # verify remoteServerAddr
  match.arg(remoteServerAddr, c("localhost", "host-gateway"), several.ok = FALSE)

  # connect to Docker
  if (missing(host)) {
    docker <- docker_connect()
  } else {
    docker <- docker_connect(host)
  }

  # select random open port if port is not provided
  if (missing(port)) {
    port <- httpuv::randomPort()
  }

  # select correct architecture for image
  if (Sys.info()["machine"] == "arm64") {
    # arm64
    img <- "seleniarm/standalone-chromium:latest"
    img_url <- "https://hub.docker.com/r/seleniarm/standalone-chromium"
  } else {
    # x86_64
    img <- "selenium/standalone-chrome:latest"
    img_url <- "https://hub.docker.com/r/selenium/standalone-chrome"
  }

  # check if image is available and if not, ask before downloading
  imgs <- unlist(docker$image$list()[["repo_tags"]])
  if (!img %in% imgs) {
    message(paste0("This dataset requires Selenium to download. Would you like to install Docker image '", img, "' from '", img_url, "'?"))
    resp <- readline(prompt = "Type 'Yes' without quotes to continue (any other action will abort the process): ")
    if (!resp == "Yes") stop("You must install the Docker image to download this dataset.")
  }

  # start Chrome
  cnt <- docker$container$run(img,
                              ports = paste0(port, ":4444"),
                              detach = TRUE)

  # add extra capabilities
  ec <- list(chromeOptions = list(
    args = c(
      "--headless",
      "--disable-dev-shm-usage"
      )
  ))

  # connect to headless Chrome
  if (remoteServerAddr == "host-gateway") {
    remoteServerAddr <- cnt$inspect()$network_settings$networks$bridge$gateway
  }
  webdriver <- RSelenium::remoteDriver(
    remoteServerAddr = remoteServerAddr,
    port = port,
    browserName = "chrome",
    extraCapabilities = ec)

  # try to connect to the server for 30 seconds
  webdriver_success <- FALSE
  start_time <- Sys.time()
  while(Sys.time() < start_time + 30 & !webdriver_success) {
    tryCatch(
      {
        sink <- utils::capture.output(webdriver$open())
        webdriver_success <- TRUE
        }, # don't print output
      error = function(e){})
  }
  if (!webdriver_success) {stop("Failed to connect to webdriver after 30 seconds.")}

  # navigate to relevant content
  webdriver$navigate(url)

  # return webdriver
  return(webdriver)
}

#' @param webdriver Selenium server object.
#' @param host Optional. The URL of the Docker daemon. See parameter in
#' \code{\link[stevedore]{docker_client}} for details/defaults. The default for
#' Linux has been changed for this function: "unix:///run/user/1000/docker.sock"
#' (i.e., rootless Docker). Can be set in `options(docker_host = "path/to/host")`
#' (this will override the provided `host` parameter).
#' @rdname webdriver
#' @export
webdriver_close <- function(webdriver, host) {
  # close connection to server
  webdriver$close()
  # get Docker host port
  port <- as.character(webdriver[["port"]])
  # connect to Docker
  if (missing(host)) {
    docker <- docker_connect()
  } else {
    docker <- docker_connect(host)
  }
  # close the running Docker container
  tryCatch(
    {
      docker_image_id <- docker$container$list(filter = c("publish" = port))$id
      docker$container$get(docker_image_id)$stop()
      docker$container$remove(docker_image_id)
    },
    error = function(e) {
      print(e)
      warning("Container failed to close.")
    }
  )
}

#' docker_connect: Connect to running Docker daemon
#' @param host Optional. The URL of the Docker daemon. See parameter in
#' \code{\link[stevedore]{docker_client}} for details/defaults. The default for
#' Linux has been changed for this function: "unix:///run/user/1000/docker.sock"
#' (i.e., rootless Docker). Can be set in `options(docker_host = "path/to/host")`
#' (this will override the provided `host` parameter).
#' @rdname webdriver
#' @export
docker_connect <- function(host) {
  # check if docker_host is set in options (this will override the provided host parameter)
  docker_host <- getOption("docker_host")
  if (!is.null(docker_host)) {
    host <- docker_host
  }
  # check Docker is available
  docker_instructions <- "Docker must be installed and the Docker daemon running and available. See install instructions for Docker Desktop on Windows and Mac: https://docs.docker.com/get-docker/"
  if (Sys.info()["sysname"] == "Linux") docker_instructions <- paste0(docker_instructions, "\n\nFor Linux, Docker must be available without sudo.\n\nThis may be achieved by installing rootless Docker (strongly recommended; run `curl -sSL https://get.docker.com/rootless | sh` and follow instructions) or creating a docker Unix group (security risk; https://docs.docker.com/engine/install/linux-postinstall/).\n\nThis function assumes rootless Docker and by default runs using host `unix:///run/user/1000/docker.sock`. This can be overridden using the 'host' argument or by setting options(docker_host = 'path/to/host').)")
  if (Sys.info()["sysname"] == "Windows") docker_instructions <- paste0(docker_instructions, "\n\nFor Windows, a Python installation with the packages `docker` and `pypiwin32` and the R package `reticulate` are further required; see: https://github.com/richfitz/stevedore#windows-support")
  if (missing(host)) {
    if (Sys.info()["sysname"] == "Linux") {
      if (!suppressMessages(stevedore::docker_available(host = "unix:///run/user/1000/docker.sock",
                                       verbose = TRUE))) {
        stop(docker_instructions)
      }} else {
        if (!suppressMessages(stevedore::docker_available(verbose = TRUE))) {
          stop(docker_instructions)
        }}
  } else {
    if (!suppressMessages(stevedore::docker_available(host = host, verbose = TRUE))) {
      stop(docker_instructions)
    }
  }
  # connect to Docker
  if (missing(host)) {
    if (Sys.info()["sysname"] == "Linux") {
      docker <- suppressMessages(stevedore::docker_client(host = "unix:///run/user/1000/docker.sock"))
    } else {
      docker <- suppressMessages(stevedore::docker_client())
    }
  } else {
    docker <- suppressMessages(stevedore::docker_client(host = host))
  }
}

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
    element <- suppressMessages(
      tryCatch({webdriver$findElement(using = By, value = value)},
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
    # NWT - communities tab
    "8814f932-33ec-49ef-896d-d1779b2abea7" = {
      # click on cases dropdown (when element is visible)
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[1]/a",
        10)
      # wait for page to load before clicking tab
      Sys.sleep(5)
      elm$clickElement()
      # click communities tab
      webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[1]/ul/li[2]/a",
        10)$clickElement()
      # wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    },
    # NWT - testing tab
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
    # NWT - vaccine coverage tab
    "effdfd82-7c59-4f49-8445-f1f8f73b6dc2" = {
      # click on vaccinations dropdown (when element is visible)
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/a",
        10)
      # wait for page to load before clicking tab
      Sys.sleep(5)
      elm$clickElement()
      # click vaccine coverage tab
      webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/ul/li[1]/a",
        10)$clickElement()
      # wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    },
    # NWT - vaccine doses tab
    "454de458-f7b4-4814-96a6-5a426f8c8c60" = {
      # click on vaccinations dropdown (when element is visible)
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/a",
        10)
      # wait for page to load before clicking tab
      Sys.sleep(5)
      elm$clickElement()
      # click vaccine doses tab
      webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/nav/div/ul/li[4]/ul/li[2]/a",
        10)$clickElement()
      # wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    },
    # Wellington Dufferin Guelph - cases tab
    "e00e2148-b0ea-458b-9f00-3533e0c5ae8e" = {
      # click on cases tab (when element is visible)
      elm <- webdriver_wait_for_element(
        webdriver,
        "xpath",
        "/html/body/div[1]/root/div/div/div[1]/div/div/div/exploration-container/div/div/div/exploration-host/div/div/exploration/div/explore-canvas/div/div[2]/div/div[2]/div[2]/visual-container-repeat/visual-container-group[2]/transform/div/div[2]/visual-container[3]/transform/div/div[3]/div/visual-modern",
        get_dataset_arg(uuid, "wait"))
      # click tab
      elm$clickElement()
      # wait for page to load
      Sys.sleep(5)
    },
    {
      # by default, just wait for page to load
      Sys.sleep(get_dataset_arg(uuid, "wait"))
    }
  )
}

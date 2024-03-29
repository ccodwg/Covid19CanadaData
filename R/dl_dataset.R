#' Download current version of a dataset catalogued in Covid19CanadaArchive
#'
#' Download the current version of an active dataset listed in datasets.json of
#' Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/datasets.json).
#' Data can either be imported into R (the default) or written to a file by
#' specifying the `file` argument. Currently, only CSV, JSON, XLSX, XLS, image
#' and HTML datasets are supported for reading into R. Websites requiring JavaScript
#' to render require `Docker` to be installed; see \code{\link[Covid19CanadaData]{webdriver}}.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param file A character string specifying the location to write the specified
#' dataset as a file (NULL by default, resulting in the dataset being returned
#' as an R object.
#' @param sep Optional. The separator to use when reading CSV files.
#' Defaults to ",".
#' @param sheet Optional. An integer or name specifying the sheet to return for
#' an XLSX or XLS file (by default, reads sheet 1 with a warning).
#' @param host Optional. The URL of the Docker daemon. See \code{\link[Covid19CanadaData]{webdriver}}.
#' @param port Optional. The host port for Docker. If not provided, a random
#' open port will be selected using \code{\link[httpuv]{randomPort}}.
#' @return The specified dataset either as an R object (the default) or
#' written to a file by (by specifying the argument `file`).
#' @examples
#' \dontrun{
#' # get PHAC epidemiology update CSV
#' dl_dataset("f7db31d0-6504-4a55-86f7-608664517bdb")
#'
#' # get Saskatchewan total cases CSV
#' dl_dataset("61cfdd06-7749-4ae6-9975-d8b4f10d5651")
#' }
#' @export
dl_dataset <- function(uuid,
                       sep,
                       sheet,
                       host,
                       port,
                       file = NULL) {

  # get list of datasets (datasets.json)
  ds <- get_datasets()

  # get list of uuids
  uuids <- names(ds)

  # try to load dataset by uuid
  if (uuid %in% uuids) {
    d <- ds[[uuid]]
    if (d$active != "True") {
      stop("Specified UUID exists but is flagged as inactive.")
    }
  } else {
    stop("Specified UUID '", uuid, "' does not exist in datasets.json.")
  }

  # if URL is not static, get URL
  url <- d$url
  if (is.null(url)) {
    url <- dl_dataset_dyn_url(uuid)
  }

  # add random number to url to prevent caching, if requested
  if (!is.null(d$args$rand_url) && d$args$rand_url == "True") {
    url <- paste0(url, "?randNum=", as.integer(lubridate::with_tz(Sys.time(), "America/Toronto")))
  }

  # download file or read into R
  if (!is.null(file)) {
    # check if file is a webpage that requires JS
    js <- d$args$js
    if (!is.null(js) && js) {
      # yes, use Selenium and write HTML
      html_out <- webdriver_get(d$uuid)
      xml2::write_html(html_out, file)
    } else {
      # no, just download file
      h <- create_curl_handle(d)
      curl::curl_download(url, file, handle = h)
    }
  } else {
    read_dataset(file = url,
                 d,
                 sep = if (missing(sep)) NULL else sep,
                 sheet = if (missing(sheet)) NULL else sheet,
                 port = if (missing(port)) NULL else port,
                 host = if (missing(host)) NULL else host)
  }
}

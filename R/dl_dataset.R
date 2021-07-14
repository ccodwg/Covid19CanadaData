#' Download current version of a dataset catalogued in Covid19CanadaArchive
#'
#' Download the current version of an active dataset listed in datasets.json of
#' Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/datasets.json).
#' Data can either be imported into R (the default) or written to a file by
#' specifying the `file` argument. Currently, only CSV, JSON, XLSX, XLS, image
#' and HTML datasets are supported for reading into R.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param file A character string specifying the location to write the specified
#' dataset as a file (NULL by default, resulting in the dataset being returned
#' as a data frame).
#' @param sep The separator to use when reading CSV files. Defaults to ",".
#' @param sheet An integer specifying the sheet to return for an XLSX or XLS
#' file (by default, reads sheet 1 with a warning).
#' @return The specified dataset either as a data frame in R (the default) or
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
                       file = NULL,
                       sep = NULL,
                       sheet = NULL){

  # get datasets.json
  ds_list <- get_dataset_list()

  # try to load dataset by uuid
  if (uuid %in% ds_list$uuid) {
    d <- ds_list[ds_list$uuid == uuid, ]
    if (d$active != "True") {
      stop("Specified UUID exists but is flagged as inactive.")
    }
  } else {
    stop("Specified UUID does not exist in datasets.json.")
  }

  # if URL is not static, get URL
  if (!is.na(d$url)) {
    url <- d$url
  } else {
    url <- dl_dataset_dyn_url(uuid)
  }

  # add no-cache header
  h <- curl::new_handle()
  curl::handle_setheaders(h, "Cache-Control" = "no-cache") # not always respected

  # add random number to url to prevent caching, if requested
  if (!is.na(d$args$rand_url) & d$args$rand_url == "True") {
    url <- paste0(url, "?randNum=", as.integer(Sys.time()))
  }

  # download file or read into R
  if (!is.null(file)) {
    curl::curl_download(url, file, handle = h)
  } else {
    if (d$file_ext == "csv") {
      if (is.null(sep)) {
        sep <- ","
      }
      dat <- utils::read.csv(url, stringsAsFactors = FALSE, sep = sep)
    } else if (d$file_ext == "json") {
      dat <- jsonlite::fromJSON(url)
    } else if (d$file_ext %in% c("xlsx", "xls")) {
      if (is.null(sheet)) {
        warning("Sheet not specified, reading sheet 1 by default.")
        sheet <- 1
      }
      if (d$file_ext == "xlsx") {
        tmp <- tempfile(fileext = ".xlsx")
      } else {
        tmp <- tempfile(fileext = ".xls")
      }
      utils::download.file(url, tmp)
      dat <- readxl::read_excel(tmp, sheet)
    } else if (d$file_ext %in% c("jpg", "jpeg", "png", "tiff")) {
      dat <- magick::image_read(url)
    } else if (d$file_ext == "html") {
      dat <- xml2::read_html(url)
    } else {
      stop("The file extension of this dataset is not supported for reading into R.")
    }
    return(dat)
  }
}

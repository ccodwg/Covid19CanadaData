#' Download archived versions of a dataset catalogued in Covid19CanadaArchive
#'
#' Download the archived version of a dataset listed in datasets.json of
#' Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/datasets.json).
#' Data can either be imported into R (the default) or written to a file by
#' specifying the `file` argument. Currently, only CSV, JSON, XLSX, XLS, image
#' and HTML datasets are supported for reading into R. Live versions of active
#' datasets may be optionally appended to the list of archived files. If no date
#' filtering is specified, the latest file is returned by default.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param date A character string in YYYY-MM-DD format specifying the date of
#' data to return. Also accepts "latest" (the default), "first" and "all".
#' @param after A character string in YYYY-MM-DD format specifying that data
#' from this date or later should be returned. Ignored if `date` is defined.
#' @param before A character string in YYYY-MM-DD format specifying that data
#' from this date or earlier should be returned. Ignored if `date` is defined.
#' @param add_live If the version of the dataset from today has not yet been
#' archived (and the dataset is still active), append the live version of the
#' file by calling \code{\link[Covid19CanadaData]{dl_dataset}}. Ignored when
#' `file` is specified.
#' @param sep Optional. The separator to use when reading CSV files.
#' Defaults to ",".
#' @param sheet Optional. An integer or name specifying the sheet to return for
#' an XLSX or XLS file (by default, reads sheet 1 with a warning).
#' @param path A character string specifying the file path to write the
#' specified dataset as a file/files. This value is NULL by default, resulting
#' in the dataset being returned as a list.
#' @return A list containing the specified dataset(s) as R objects (the default)
#' or written to a file/files by (by specifying the argument `path`).
#' @export
dl_archive <- function(uuid,
                       date,
                       after,
                       before,
                       sep,
                       sheet,
                       add_live = FALSE,
                       path = NULL) {

  # get dataset information
  d <- get_uuid(uuid)

  # read date filters
  date <- if (missing(date)) NULL else date
  after <- if (missing(after)) NULL else after
  before <- if (missing(before)) NULL else before

  # retrieve file index
  ind <- api_archive(uuid, date, after, before)

  # retrieve URLs
  urls <- ind$file_url

  # download files or read into R
  if (!is.null(path)) {
    # check add_live
    if (add_live) {
      warning("add_live is ignored when downloading files to disk.")
    }
    # create curl handle
    h <- curl::new_handle()
    # download files
    dat <- lapply(urls, FUN = function(x) {
      url <- x
      name <- basename(url)
      file <- file.path(path, name)
      cat("Downloading:", file, fill = TRUE)
      curl::curl_download(url, file, handle = h)
    })
  } else {
    # read files into R

    # read args
    sep <- if (missing(sep)) NULL else sep
    sheet <- if (missing(sheet)) NULL else sheet

    # ensure HTML files are correctly read in
    if (d$file_ext == "html") {
      d$file_ext <- "html_raw"
    }

    # read files
    dat <- lapply(urls, FUN = function(x) {
      url <- x
      name <- basename(url)
      cat("Reading:", name, fill = TRUE)
      Covid19CanadaData::read_dataset(
        url,
        d,
        sep = sep,
        sheet = sheet,
        port = NULL,
        host = NULL)
    })

    # add file dates to data
    names(dat) <- ind$file_date_true

    # add live file
    if (add_live) {
      if (d$active == "True") {
        # check if today's dataset is in the archive yet
        date_today <- lubridate::date(lubridate::with_tz(Sys.time(), "America/Toronto"))
        if (date_today %in% as.Date(ind$file_date)) {
          cat("Today's file is already in the archive. No need to append the live version.", fill = TRUE)
        } else {
          cat("Today's file is not yet in the archive. Appending the live file...", fill = TRUE)
          dat[[as.character(date_today)]] <- if (!missing(sheet)) {
            Covid19CanadaData::dl_dataset(uuid, sheet = sheet)
          } else if (!missing(sep)) {
            Covid19CanadaData::dl_dataset(uuid, sep = sep)
          } else {
            Covid19CanadaData::dl_dataset(uuid)
          }
        }
      } else {
        cat("This dataset is inactive. No need to append the live version.", fill = TRUE)
      }
    }
    # return data as list
    return(dat)
  }
}

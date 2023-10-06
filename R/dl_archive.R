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
#' @param remove_duplicates Remove duplicate files from the sample after date
#' filtering? Defaults to `TRUE`.
#' @param add_live If the version of the dataset from today has not yet been
#' archived (and the dataset is still active), append the live version of the
#' file by calling \code{\link[Covid19CanadaData]{dl_dataset}}. Ignored when
#' `file` is specified. Defaults to `FALSE`.
#' @param sep Optional. The separator to use when reading CSV files.
#' Defaults to ",".
#' @param sheet Optional. An integer or name specifying the sheet to return for
#' an XLSX or XLS file (by default, reads sheet 1 with a warning).
#' @param overwrite When saving to disk (by specifying the argument `path`),
#' should existing files be overwritten? If `FALSE` (the default), only new
#' files will be downloaded.
#' @param path A character string specifying the file path to write the
#' specified dataset as a file/files. This value is NULL by default, resulting
#' in the dataset being returned as a list.
#' @return A list containing the specified dataset(s) as R objects (the default)
#' or written to a file/files by (by specifying the argument `path`). In the
#' latter case, a vector of file paths is also returned invisibly.
#' @export
dl_archive <- function(uuid,
                       date,
                       after,
                       before,
                       sep,
                       sheet,
                       remove_duplicates = TRUE,
                       add_live = FALSE,
                       overwrite = FALSE,
                       path = NULL) {

  # get dataset information
  d <- get_uuid(uuid)

  # read date filters
  date <- if (missing(date)) NULL else date
  after <- if (missing(after)) NULL else after
  before <- if (missing(before)) NULL else before

  # retrieve file index
  ind <- api_archive(uuid, date, after, before, remove_duplicates)

  # retrieve URLs
  files <- ind$file_name
  urls <- ind$file_url

  # create curl handle
  h <- curl::new_handle()

  # download files or read into R
  if (!is.null(path)) {
    # check add_live
    if (add_live) {
      warning("add_live is ignored when downloading files to disk.")
    }
    # perform path expansion (so that existing files are properly recognized if using ~)
    path <- path.expand(path)
    # get file paths
    file_paths <- file.path(path, files)
    # if overwrite == FALSE, check for existing files
    if (!overwrite) {
      files <- list.files(path, full.names = TRUE)
      # download new files only
      file_n <- which(!file_paths %in% files)
      # check if any files are left
      if (length(file_n) == 0) {
        warning("Aborting download: all requested files already exist at the specified path. Use overwrite=TRUE to download anyway.")
        return(invisible(NULL))
      } else if (length(file_n) < length(urls)) {
        cat("Downloading new files only (use overwrite=TRUE to override this behaviour).", fill = TRUE)
      }
    } else {
      # download all files
      file_n <- seq_along(ind)
    }
    # download files
    dat <- lapply(file_n, FUN = function(x) {
      url <- urls[x]
      file <- file_paths[x]
      cat("Downloading:", file, fill = TRUE)
      curl::curl_download(url, file, handle = h)
    })
    # return file path invisibly
    return(invisible(unlist(dat)))
  } else {
    # read files into R

    # read args
    sep <- if (missing(sep)) NULL else sep
    sheet <- if (missing(sheet)) NULL else sheet

    # read files
    dat <- lapply(1:nrow(ind), FUN = function(x) {
      url <- urls[x]
      name <- files[x]
      cat("Reading:", name, fill = TRUE)
      tmp <- tempfile()
      curl::curl_download(url, tmp, handle = h)
      Covid19CanadaData::read_dataset(
        file = tmp,
        d,
        sep = sep,
        sheet = sheet,
        port = NULL,
        host = NULL)
    })

    # add file dates to data
    names(dat) <- ind$file_date

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

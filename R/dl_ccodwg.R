#' Download data from the COVID-19 Canada Open Data Working Group dataset
#'
#' Download data from the COVID-19 Canada Open Data Working Group dataset via
#' the JSON API. Pre-processing of the data may be done by specifying the
#' relevant arguments in the function which are then passed to the API call.
#' Data can either be imported into R (the default) or written to a CSV file by
#' specifying the `file` argument. Full documentation of the API is available
#' here: https://opencovid.ca/api/
#'
#' @param type One of "timeseries" (time series data), "summary" (summary data),
#' "individual" (individual-level data), "other" (supplementary files) or
#' "version" (date and time dataset was last updated).
#' @param stat Which statistic to return. See details.
#' @param loc The geographic level of the data. Required for types "timeseries",
#' "summary" and "individual". One of "default", "canada", "prov", "hr",
#' a two-letter province code or a health region code. Not all geographic levels
#' are available for all datasets. See API documentation for all possible
#' values. If NA, the default value for the specified dataset will be used.
#' @param date A character string specifying the date of data to return.
#' Use either YYYY-MM-DD or DD-MM-YYYY format.
#' @param after A character string specifying that data from this date or later
#' should be returned. Use either YYYY-MM-DD or DD-MM-YYYY format.
#' @param before A character string specifying that data from this date or
#' earlier should be returned. Use either YYYY-MM-DD or DD-MM-YYYY format.
#' @param ymd One of "true" or "false". Should dates be returned in YYYY-MM-DD
#' format? Default = "true". Otherwise, dates are returned in DD-MM-YYYY format.
#' @param missing How should missing values be returned? One of "null", "na",
#' "nan" or "empty". Default = "na". This reads into R as NA.
#' @param extra One of "true" or "false". For individual-level data, should
#' abbreviated columns be joined? (E.g., "case_source" for individual-level
#' case data) Default = "true".
#' @param dateonly One of "true" or "false". Should version return only the
#' update date or the full date and time of the update? Default = "false".
#' @param verbose Logical. Print debug messages?
#' @param file A character string specifying the location to write the specified
#' dataset as a CSV file (NULL by default, resulting in the dataset being
#' returned as a data frame).
#' @return The specified dataset either as a data frame in R (the default) or
#' written to a CSV file by (by specifying the argument `file`). If
#' type = "version", the date and time the dataset were last updated is returned
#' as a character string.
#' @details The values for stat depend on the type of data desired. Note that
#' only one value for stat are accepted per query:
#'
#' timeseries: "cases", "mortality", "recovered", "testing", "active",
#' "avaccine", "dvaccine", "cvaccine"
#'
#' Note that not all datasets are available at all geographic levels (loc).
#'
#' other: "prov", "hr", "age_cases", "age_mortality"
#' @examples
#' \dontrun{
#' # get case time series for Toronto during the first half of March 2020
#' dl_ccodwg("timeseries", "cases", loc = 3595, after = "2020-03-01", before = "2020-03-15")
#'
#' # get most recent Canada-wide summary
#' dl_ccodwg("summary", loc = "canada")
#'
#' # get list of province names and population values
#' dl_ccodwg("other", "prov")
#'
#' # get date the dataset was last updated
#' as.Date(dl_ccodwg("version"))
#' }
#' @export
dl_ccodwg <- function(type = c("timeseries", "individual", "summary",
                               "other", "version"),
                      stat = c(
                        "cases",
                        "mortality",
                        "recovered",
                        "testing",
                        "active",
                        "avaccine",
                        "dvaccine",
                        "cvaccine"
                      ),
                      loc = "default",
                      date = NA,
                      after = NA,
                      before = NA,
                      ymd = "true",
                      missing = "na",
                      extra = "true",
                      dateonly = "false",
                      verbose = FALSE,
                      file = NULL) {

  # verify type argument
  match.arg(type,
            choices = c("timeseries", "summary", "individual",
                        "other", "version"),
            several.ok = FALSE)
  # verify formatting arguments
  match.arg(ymd,
            choices = c("true", "false"),
            several.ok = FALSE)
  match.arg(missing,
            choices = c("null", "na", "nan", "empty"),
            several.ok = FALSE)

  # process arguments
  loc <- as.character(loc)

  # download data
  if (type == "timeseries") {
    ## verify arguments
    match.arg(stat,
              choices = c("cases", "mortality", "recovered", "testing",
                          "active", "avaccine", "dvaccine", "cvaccine"),
              several.ok = FALSE)
    if (length(stat) > 1) {
      stop("Only a single 'stat' may be specified.")
    }
    if (stat %in% c("cases", "mortality")) {
      match.arg(loc,
                choices = c("default", "canada", "prov", "hr",
                            get_prov_names(), get_hr_ids()),
                several.ok = FALSE)
    } else {
      match.arg(loc,
                choices = c("default", "canada", "prov",
                            get_prov_names()),
                several.ok = FALSE)
    }
    api_call <- api_ccodwg(type,
      c("stat", "loc", "date",
        "after", "before", "ymd",
        "missing"))
    dat <- jsonlite::fromJSON(api_call)[[1]]

  } else if (type == "summary") {
    ## verify arguments
    match.arg(loc,
              choices = c("default", "canada", "prov", "hr",
                          get_prov_names(), get_hr_ids()),
              several.ok = FALSE)
    api_call <- api_ccodwg(type,
                           c("loc", "date", "after",
                             "before", "ymd", "missing"))
    dat <- jsonlite::fromJSON(api_call)[[1]]

  } else if (type == "individual") {
    stop("Individual-level data not yet available in this package.")

  } else if (type == "other") {
    ## verify arguments
    match.arg(stat,
              choices = c("prov", "hr", "age_cases", "age_mortality"),
              several.ok = FALSE)
    if (length(stat) > 1) {
      stop("Only a single 'stat' may be specified.")
    }
    api_call <- api_ccodwg(type,
                           c("stat", "missing"))
    dat <- jsonlite::fromJSON(api_call)[[1]]

  } else if (type == "version") {
    ## verify arguments
    match.arg(dateonly,
              choices = c("true", "false"),
              several.ok = FALSE)
    api_call <- api_ccodwg(type,
                           c("dateonly"))
    dat <- jsonlite::fromJSON(api_call)[[1]]
  }

  # print API call
  if (verbose) {
    cat(api_call, fill = TRUE)
  }

  # write data (if file is specified) else return data
  if (!is.null(file)) {
    utils::write.csv(dat, file = file, row.names = FALSE)
  } else {
    return(dat)
  }

}

#' Get date the COVID-19 Canada Open Data Working Group dataset was last updated
#'
#' This is a simple convenience function to return the date portion of a
#' "version" call to the API.
#' @examples
#' \dontrun{
#' # get date the CCODWG dataset was last updated
#' ccodwg_update_date()
#' }
#' @export
ccodwg_update_date <- function(){
  as.Date(dl_ccodwg("version", dateonly = "true"))
}

#' Download current version of a dataset catalogued in Covid19CanadaArchive
#'
#' Download the current version of an active dataset listed in datasets.json of
#' Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/data/datasets.json).
#' Data can either be imported into R (the default) or written to a file by
#' specifying the `file` argument. Currently, only CSV datasets are supported.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param file A character string specifying the location to write the specified
#' dataset as a file (NULL by default, resulting in the dataset being returned
#' as a data frame).
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
                       file = NULL){

  # load datasets.json
  ds <- suppressWarnings(jsonlite::fromJSON("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/data/datasets.json")$active %>%
    dplyr::bind_rows())

  # try to load daset by uuid
  if (uuid %in% ds$uuid) {
    d <- ds[ds$uuid == uuid, ]
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

  # read dataset and return or write to file
  if (d$file_ext == "csv") {
    ## CSV
    dat <- utils::read.csv(url, stringsAsFactors = FALSE)
    if (!is.null(file)) {
      utils::write.csv(dat, file = file, row.names = FALSE)
    } else {
      return(dat)
    }
  }

}

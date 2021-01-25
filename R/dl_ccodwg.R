#' Download data from the COVID-19 Canada Open Data Working Group dataset
#'
#' Download data from the COVID-19 Canada Open Data Working Group dataset via
#' the JSON API. Pre-processing of the data may be done by specifying the
#' relevant arguments in the function which are then passed to the API call.
#'
#' Full documentation of the API is available here: https://opencovid.ca/api/
#'
#' Data can either be imported into R (the default) or written to a CSV file by
#' specifying the `file` argument.
#'
#' @param type One of "timeseries" (time series data), "summary" (summary data),
#' "individual" (individual-level data), "other" (supplementary files) or
#' "version" (date and time dataset was last updated).
#' @param stat Which statistic to return. One of "cases", "mortality",
#' "recovered", "testing", "active", "avaccine", "dvaccine" or "cavvacine".
#' Note that not all datasets are available at all geographic levels.
#' @param loc The geographic level of the data. One of "default", "canada",
#' "prov", "hr", a two-letter province code or a health region code. Not all
#' geographic levels are available for all datasets. See API documentation for
#' all possible values. If NA, the default value for the specified dataset will
#' be used.
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
#' @param verbose Logical. Print debug messages?
#' @param file A character string specifying the location to write the specified
#' dataset as a CSV file (NULL by default, resulting in the dataset being
#' returned as a data frame).
#' @return The specified dataset either as a data frame in R (the default) or
#' written to a CSV file by (by specifying the argument `file`). If
#' type = "version", the date and time the dataset were last updated is returned
#' as a character string.
#' @examples
#' # get case time series for Toronto during the first half of March 2020
#' dl_ccodwg("timeseries", "cases", loc = 3595, after = "2020-03-01", before = "2020-03-15")
#'
#' # get most recent Canada-wide summary
#' dl_ccodwg("summary", loc = "canada")
#'
#' # get date the dataset was last updated
#' as.Date(dl_ccodwg("version"))
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
    if (length(stat) > 1) {
      stop("Only a single 'stat' may be specified.")
    }
    match.arg(stat,
              choices = c("cases", "mortality", "recovered", "testing",
                          "active", "avaccine", "dvaccine", "cvaccine"),
              several.ok = FALSE)
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
    stop("Other files not yet available in this package.")

  } else if (type == "version") {
    api_call <- api_ccodwg(type)
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
#' # get date the CCODWG dataset was last updated
#' ccodwg_update_date()
#' @export
ccodwg_update_date <- function(){
  as.Date(dl_ccodwg("version"))
}

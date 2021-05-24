#' Process a dataset catalogued in Covid19CanadaArchive
#'
#' Process a dataset listed in datasets.json of Covid19CanadaArchive (https://github.com/ccodwg/Covid19CanadaArchive/blob/master/datasets.json)
#' into a standardized format for a specific value provided by that dataset
#' (e.g., cases, deaths, hospitalizations, etc.). If `ds` is provided, the
#' function will read the dataset from a named list indexed by UUID. Otherwise,
#' the active version of the dataset will be read using \code{\link{dl_dataset}}
#' (if available).
#'
#' The currently supported values are the following:
#' \itemize{
#'  \item cases
#'  \item mortality
#'  \item recovered
#'  \item testing
#'  \item vaccine_distribution
#'  \item vaccine_administration
#'  \item vaccine_completion
#' }
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param val The desired value to be extracted from the dataset. Valid
#' inputs, include "cases", "mortality", "testing", etc. See details.
#' @param fmt The format of the output data.
#' @param ds The downloaded dataset in the format returned by `dl_dataset`.
#' @param ... Additional arguments to be passed to the processing function.
#' @return A standardized version of the dataset for the desired value.
#'
#' @export
process_dataset <- function(uuid,
                            val = c(
                              "cases",
                              "mortality",
                              "recovered",
                              "testing",
                              "vaccine_distribution",
                              "vaccine_administration",
                              "vaccine_completion"
                            ),
                            fmt,
                            ds,
                            ...) {

  # get datasets.json
  ds_list <- get_dataset_list()

  # check UUID
  if (uuid %in% ds_list$uuid) {
    d <- ds_list[ds_list$uuid == uuid, ]
  } else {
    stop("Specified UUID does not exist in datasets.json.")
  }

  # check value
  match.arg(
    val,
    choices = c("cases",
                "mortality",
                "recovered",
                "testing",
                "vaccine_distribution",
                "vaccine_administration",
                "vaccine_completion"),
    several.ok = FALSE
  )

  # get processing function name based on province name identified in datasets.json
  prov <- gsub("/", "-", d$dir_parent)
  process_fun <- get(paste0("process_", prov))

  # unpack optional arguments
  dots <- list(...)
  if (methods::hasArg("prov")) {
    prov <- dots[["prov"]]
  } else {
    prov <- NULL
  }
  if (methods::hasArg("date_current")) {
    date_current <- dots[["date_current"]]
  } else {
    date_current <- as.Date(Sys.Date())
  }
  if (methods::hasArg("testing_type")) {
    testing_type <- dots[["testing_type"]]
  } else {
    testing_type <- NULL
  }

  # pass arguments to processing function
  dat_processed <- process_fun(uuid, val, fmt, ds,
                               prov, date_current, testing_type)

  # return processed data
  dat_processed

}

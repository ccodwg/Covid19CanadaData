#' Get datasets.json
#'
#' @return A data frame containing the information from datasets.json.
get_dataset_list <- function() {
  if (!exists("ds_list", envir = covid_ds_env)) {
    # download and cache datasets.json
    assign(
      "ds_list",
      suppressWarnings(
        jsonlite::fromJSON("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/datasets.json") %>%
          unlist(recursive = FALSE) %>%
          dplyr::bind_rows()
      ),
      envir = covid_ds_env
    )
  }
  # return datasets.json
  covid_ds_env$ds_list
}

#' Helper functions for process_dataset
#'
#' Helper functions for \code{\link{process_dataset}}
#'
#' @name process_dataset_helpers
NULL

#' process_dataset: Common processing for fmt = cum_current
#' @param .data The dataset to be processed.
#' @param loc One of "prov" or "hr", depending on the spatial resolution.
#' @param val The value.
#' @param prov The province.
#' @param date_current The date provided to cum_current (usually the current date).
#' @rdname process_dataset_helpers
helper_cum_current <- function(.data, loc = c("prov", "hr"),
                               val, prov, date_current) {
  match.arg(loc, choices = c("prov", "hr"), several.ok = FALSE)
  if (loc == "prov") {
    dplyr::mutate(
      .data,
      name = val,
      province = prov,
      date = date_current,
      value = as.integer(.data$value)
    ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$date,
        .data$value)
  } else {
    dplyr::mutate(
      .data,
      name = val,
      province = prov,
      date = date_current,
      value = as.integer(.data$value)
    ) %>%
      dplyr::select(
        .data$name,
        .data$province,
        .data$sub_region_1,
        .data$date,
        .data$value)
  }
}

#' Error functions for process_dataset
#'
#' Error functions for \code{\link{process_dataset}}
#'
#' @name process_dataset_e
NULL

#' process_dataset: Report no functions to process specified UUID
#' @rdname process_dataset_e
e_uuid <- function() stop("No functions exist to process this UUID.")

#' process_dataset: Report value cannot be extracted from specified UUID
#' @rdname process_dataset_e
e_val <- function() stop("The specified value cannot be extracted from this UUID.")

#' process_dataset: Report value cannot be extracted with specified output format
#' @rdname process_dataset_e
e_fmt <- function() stop("The specified output format is not available for this value.")

#' Get province name abbreviations
#'
#' @return A character vector of province name abbreviations.
get_prov_names <- function() {
  prov <- jsonlite::fromJSON("https://api.opencovid.ca/other?stat=prov")[[1]]
  prov[["province_short"]]
}

#' Get health region IDs
#'
#' @return A character vector of health region IDs.
get_hr_ids <- function() {
  hr <- jsonlite::fromJSON("https://api.opencovid.ca/other?stat=hr")[[1]]
  hr[["HR_UID"]]
}

#' Generate API call for the COVID-19 Canada Open Data Working Group dataset
#'
#' This function is used within dl_ccodwg() to generate the API call with the
#' specified arguments.
#'
#' @param type One of "timeseries" (time series data), "summary" (summary data),
#' "individual" (individual-level data), "other" (supplementary files) or
#' "version" (date and time dataset was last updated).
#' @param args_list A list of API arguments for the selected data type.
#' @return A character string representing the API call with the specified
#' arguments.
api_ccodwg <- function(type, args_list) {

  if (!missing(args_list)) {
    # get arguments
    args_vals <- unlist(mget(args_list, envir = parent.frame()))

    # process arguments
    if (type %in% c("timeseries", "summary", "individual")) {
      if (args_vals[["loc"]] == "default") {
        args_vals[["loc"]] <- NA
      }
    }

    # generate API call
    args_vals <- as.character(args_vals)
    args_names <- args_list[!is.na(args_vals)]
    args_vals <- args_vals[!is.na(args_vals)]
    args_string <- paste(
      paste(args_names, args_vals, sep = "="),
      collapse = "&")
    paste0(
      "https://api.opencovid.ca/", type, "?", args_string
    )
  } else {
    paste0("https://api.opencovid.ca/", type)
  }

}

#' Download current version of a dataset catalogued in Covid19CanadaArchive: Get dynamic URL
#'
#' Helper function for dl_dataset(): data-specific code to retrieve current URL
#' of a dataset with dynamic URLs. Replicates code included in the "url_fun_r"
#' field of datasets.json. This code is intentionally written to fit on a single
#' line.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return The current URL of the specified dataset.
#' @export
dl_dataset_dyn_url <- function(uuid) {
  switch(
    uuid,
    "61cfdd06-7749-4ae6-9975-d8b4f10d5651" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "c40d5b7c-f41c-4633-8bc1-a158dedcbf40" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-tests/tests'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "db9a7e2e-1a1f-4b98-a31a-24460910fc2d" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-cases/hospitalized'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "b575a747-e433-43f4-bd86-23c896df8de5" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/seven-day-average-of-new-covid-cases'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    stop("Specified UUID does not exist in dasets.json or does not have a dynamic URL.")
  )
}

#' Get datasets.json
#'
#' @return A data frame containing the information from datasets.json.
#' @export
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
    "5783f3d9-93b0-4aaa-ab97-e0e362a084cc" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-vaccines/vaccines'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "bd18a4e4-bc22-47c6-b601-1aae39667a03" = {
      paste0('https://www.gov.nu.ca', rvest::html_attr(rvest::html_elements(rvest::html_node(rvest::read_html('https://www.gov.nu.ca/health/information/covid-19-vaccination'), 'body'), 'img')[[2]], 'src'))
    },
    stop("Specified UUID does not exist in dasets.json or does not have a dynamic URL.")
  )
}

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

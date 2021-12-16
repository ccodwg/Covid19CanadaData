#' Get list of datasets
#'
#' Calls `api.opencovid.ca/datasets` to return information about all available datasets.
#'
#' @returns A named list of datasets and their corresponding information.
#' @export
get_datasets <- function() {
  jsonlite::fromJSON("https://api.opencovid.ca/datasets", simplifyVector = FALSE)
}

#' Get information on a specific dataset by UUID
#'
#' Calls `api.opencovid.ca/datasets` to return information about a specific dataset.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return A named list of available information on the dataset.
#' @export
get_uuid <- function(uuid) {
  # construct API call
  api_call <- paste0("https://api.opencovid.ca/datasets?uuid=", uuid)
  # get information
  tryCatch(
    resp <- jsonlite::fromJSON(api_call),
    error = function(e) stop("UUID not found.")
  )
  # return information
  return(resp[[1]])
}

#' Get URL of dataset by UUID
#'
#' If the URL is dynamic, the current URL will be calculated and returned.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return The URL of the specified dataset.
#' @export
get_dataset_url <- function(uuid) {
  resp <- get_uuid(uuid)
  url <- resp$url
  if (is.null(url)) {
    url <- dl_dataset_dyn_url(uuid)
  }
  return(url)
}

#' Get argument of dataset by UUID
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param arg The argument to return.
#' @return The named arg of the specified dataset.
#' @export
get_dataset_arg <- function(uuid, arg) {
  resp <- get_uuid(uuid)
  args <- resp$args
  tryCatch(
    {
      a <- args[[arg]]
      if (is.null(a)) {
        cat("Argument", arg, "not specified for this UUID. Returning NA.", fill = TRUE)
        return(NA)
      }
      return(a)
    }
  )
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
    "7b7be246-cd65-4f35-b354-faa705cacecc" = {
      sub('^\\.\\.', 'https://novascotia.ca/news', rvest::html_text2(rvest::html_elements(rvest::read_html('https://novascotia.ca/news/search/?dept=180'), xpath='//a[contains(text(), \"New Cases of COVID-19\")]/@href')[1]))
    },
    "2e7a5549-92ae-473d-a97a-7b8e0c1ddbbc" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://www.bchu.org/ServicesWeProvide/InfectiousDiseases/Pages/coronavirus.aspx'), 'iframe'), 'src')
    },
    "fe08035c-2c03-4960-a642-bde1fe18c857" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://ckphu.com/current-situation-in-chatham-kent/'), 'iframe'), 'src')
    },
    "83d1fa13-7fb3-4079-b3dc-5bc50c584fd3" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://www.kflaph.ca/en/healthy-living/status-of-cases-in-kfla.aspx'), 'iframe'), 'src')
    },
    stop("Specified UUID does not exist in datasets.json or does not have a dynamic URL.")
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

#' Get province name abbreviations
#'
#' @return A character vector of province name abbreviations.
get_prov_names <- function() {
  prov <- utils::read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/other/prov_map.csv",
                          stringsAsFactors = FALSE)[["province_short"]]
}

#' Get health region IDs
#'
#' @return A character vector of health region IDs.
get_hr_ids <- function() {
  hr <- utils::read.csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/other/hr_map.csv",
                        stringsAsFactors = FALSE)
  hr <- as.character(sort(unique(hr[["HR_UID"]])))
}

#' Generate API call for the COVID-19 Canada Open Data Working Group dataset
#'
#' This function is used within dl_ccodwg() to generate the API call with the
#' specified arguments.
#'
#' @param type One of "timeseries" (time series data), "summary" (summary data),
#' "individual" (individual-level data),  or "other" (supplementary files).
#' @param args_list A list of API arguments for the selected data type.
#' @return A character string representing the API call with the specified
#' arguments.
api_ccodwg <- function(type, args_list) {

  # get arguments
  args_vals <- unlist(mget(args_list, envir = parent.frame()))

  # process arguments
  if (args_vals[["loc"]] == "default") args_vals[["loc"]] <- NA

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
}

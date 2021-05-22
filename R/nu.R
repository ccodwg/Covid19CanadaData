#' Functions to process datasets: NU
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nu <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "NU"

  # process datasets
  switch(
    uuid,
    "04ab3773-f535-42ad-8ee4-4d584ec23523" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$`Total confirmed cases`) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$Deaths) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$`Total recovered cases`) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table(header = TRUE) %>%
                `[[`(1) %>%
                dplyr::select(.data$`Total tests`) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_distribution" = {
          switch(
            fmt,

            e_fmt()
          )
        },
        "vaccine_administration" = {
          switch(
            fmt,

            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,

            e_fmt()
          )
        },
        e_val()
      )
    },
    e_uuid()
  )
}

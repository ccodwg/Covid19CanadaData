#' Functions to process datasets: NT
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nt <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "NT"

  # process datasets
  switch(
    uuid,
    "e008ba6f-7b09-4af4-afc5-63ec3c3bbfbb" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements(".metric") %>%
                `[`(5) %>%
                rvest::html_text() %>%
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
              data.frame(
                value = 0 # deaths are not reported
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
                rvest::html_elements(".metric") %>%
                `[`(6) %>%
                rvest::html_text() %>%
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
                rvest::html_elements(".metric") %>%
                `[`(1) %>%
                rvest::html_text() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements(".metric") %>%
                `[`(7:8) %>%
                rvest::html_text() %>%
                sub("^.*)", "", .) %>% # remove "(Dose 1/2)"
                readr::parse_number() %>%
                sum() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements(".metric") %>%
                `[`(8) %>%
                rvest::html_text() %>%
                sub("^.*)", "", .) %>% # remove "(Dose 2)"
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    e_uuid()
  )
}

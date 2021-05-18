#' Functions to process datasets: YT
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_yt <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "YT"

  # process datasets
  switch(
    uuid,
    "4cdeff57-3cbd-4d58-b0a9-c66d8c0c197e" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                `[[`(1) %>%
                rvest::html_table() %>%
                dplyr::select(2) %>%
                dplyr::slice(1) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                `[[`(1) %>%
                rvest::html_table() %>%
                dplyr::select(2) %>%
                dplyr::slice(4) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                `[[`(1) %>%
                rvest::html_table() %>%
                dplyr::select(2) %>%
                dplyr::slice(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          match.arg(testing_type, c("n_tests_completed", "n_people_tested"), several.ok = FALSE)
          switch(
            fmt,
            "prov_cum_current" = {
              if (testing_type == "n_tests_completed") {
                ds %>%
                  rvest::html_elements("table") %>%
                  `[[`(2) %>%
                  rvest::html_table() %>%
                  dplyr::select(2) %>%
                  dplyr::slice(1) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", prov, val, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_elements("table") %>%
                  `[[`(2) %>%
                  rvest::html_table() %>%
                  dplyr::select(2) %>%
                  dplyr::slice(2) %>%
                  as.character() %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", prov, val, date_current)
              }
            },
            e_fmt()
          )
        },
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                `[[`(3) %>%
                rvest::html_table() %>%
                dplyr::select(2) %>%
                dplyr::slice(3) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("table") %>%
                `[[`(3) %>%
                rvest::html_table() %>%
                dplyr::select(2) %>%
                dplyr::slice(2) %>%
                as.character() %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
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

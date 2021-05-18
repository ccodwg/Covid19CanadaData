#' Functions to process datasets: BC
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_bc <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "BC"

  # process datasets
  switch(
    uuid,
    "91367e1d-8b79-422c-b314-9b3441ba4f42" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$Cases) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$Cases
                  ) %>%
                helper_cum_current(loc = "hr", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$Deaths) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$Deaths
                ) %>%
              helper_cum_current(loc = "hr", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$HA_Name, .data$Recovered) %>%
                dplyr::rename(
                  sub_region_1 = .data$HA_Name,
                  value = .data$Recovered
                ) %>%
                helper_cum_current(loc = "hr", prov, val, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "f9a8dea5-1eed-447b-a9a0-be2a4b62d6a6" = {
      switch(
        val,
        "testing" = {
          switch(
            "prov_cum_current" = {
              ds %>%
                dplyr::filter(.data$Region == "BC") %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::select(.data$Total_Tests) %>%
                dplyr::rename(value = .data$Total_Tests) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "9d940861-0252-4d33-b6e8-23a2eeb105bf" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Tot_Doses_Received) %>%
                dplyr::rename(value = .data$Tot_Doses_Received) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Tot_Doses_Admin) %>%
                dplyr::rename(value = .data$Tot_Doses_Admin) %>%
                helper_cum_current(loc = "prov", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$HA_Name == "BC") %>%
                dplyr::select(.data$Two_Dose_Admin) %>%
                dplyr::rename(value = .data$Two_Dose_Admin) %>%
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

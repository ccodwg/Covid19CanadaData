#' Functions to process datasets: ON
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_on <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "ON"

  # process datasets
  switch(
    uuid,
    "921649fa-c6c0-43af-a112-23760da4d622" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::select(.data$Reporting_PHU) %>%
                dplyr::count(.data$Reporting_PHU) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$Outcome1 == "Fatal") %>%
                dplyr::select(.data$Reporting_PHU) %>%
                dplyr::count(.data$Reporting_PHU) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$Outcome1 == "Resolved") %>%
                dplyr::select(.data$Reporting_PHU) %>%
                dplyr::count(.data$Reporting_PHU) %>%
                dplyr::rename(
                  sub_region_1 = .data$Reporting_PHU,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "a8b1be1a-561a-47f5-9456-c553ea5b2279" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::select(.data$Total.patients.approved.for.testing.as.of.Reporting.Date) %>%
                # this is called "Tests completed" on the Ontario testing data website
                # https://covid-19.ontario.ca/data/testing-volumes-and-results
                dplyr::rename(value = .data$Total.patients.approved.for.testing.as.of.Reporting.Date) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "170057c4-3231-4f15-9438-2165c5438dda" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                dplyr::slice_tail(n = 1) %>%
                dplyr::transmute(value = .data$total_doses_administered) %>%
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
                dplyr::slice_tail(n = 1) %>%
                dplyr::transmute(value = .data$total_individuals_fully_vaccinated) %>%
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

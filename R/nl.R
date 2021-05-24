#' Functions to process datasets: NL
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nl <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {

  # set defaults
  prov <- "NL"

  # process datasets
  switch(
    uuid,
    "f0e10f54-a4db-48d8-9c4e-8571e663ca28" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_of_cases) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_of_cases
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
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_of_deaths) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_of_deaths
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
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_number_recovered) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_number_recovered
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "testing" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$name, .data$total_people_tested) %>%
                dplyr::rename(
                  sub_region_1 = .data$name,
                  value = .data$total_people_tested
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "6d1b0f2e-0ac0-4ad8-a383-619065ec5a52" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$DosesReceived) %>%
                dplyr::rename(value = .data$DosesReceived) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "6a7a6197-a10d-42a3-b0f2-0e4e78113539" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$TotalAdmin) %>%
                dplyr::rename(value = .data$TotalAdmin) %>%
                helper_cum_current(loc = "prov", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "vaccine_completion" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$TotalAdmin, .data$OneDose) %>%
                dplyr::transmute(value = .data$TotalAdmin - .data$OneDose) %>%
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

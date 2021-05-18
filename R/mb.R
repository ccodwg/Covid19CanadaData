#' Functions to process datasets: MB
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_mb <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "MB"

  # definitions
  mb_rha <- c(
    "Interlake-Eastern",
    "Northern",
    "Prairie Mountain Health",
    "Southern Health-Sant\u00E9 Sud", # unicode
    "Winnipeg"
  )

  # process datasets
  switch(
    uuid,
    "0261e07b-85ce-4952-99d1-6e1e9a440291" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::select(
                  .data$RHA,
                  .data$Total_Cases
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
                  value = .data$Total_Cases
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
                dplyr::filter(.data$Area %in% mb_rha) %>%
                dplyr::select(
                  .data$RHA,
                  .data$Deaths
                ) %>%
                dplyr::rename(
                  sub_region_1 = .data$RHA,
                  value = .data$Deaths
                ) %>%
                helper_cum_current(loc = "hr", val, prov, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::filter(.data$Area == "All") %>%
                dplyr::select(
                  .data$Recovered
                ) %>%
                dplyr::rename(
                  value = .data$Recovered
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
              ds$features$attributes %>%
                dplyr::filter(.data$Area == "All") %>%
                dplyr::select(
                  .data$Total_Tests
                ) %>%
                dplyr::rename(
                  value = .data$Total_Tests
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

#' Functions to process datasets: NS
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_ns <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "NS"

  # definitions
  ns_zones <- c(
    "Zone 1 - Western",
    "Zone 2 - Northern",
    "Zone 3 - Eastern",
    "Zone 4 - Central"
  )

  # process datasets
  switch(
    uuid,
    "d0f05ef1-419f-4f4c-bc2d-17446c10059f" = {
      switch(
       val,
       "cases" = {
         switch(
           fmt,
           "hr_cum_current" = {
             ds$features$attributes %>%
               dplyr::select(.data$ZONE_LABEL, .data$cumu_cases) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$cumu_cases
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
               dplyr::select(.data$ZONE_LABEL, .data$tot_deaths) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$tot_deaths
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
               dplyr::select(.data$ZONE_LABEL, .data$res_cases) %>%
               dplyr::filter(.data$ZONE_LABEL %in% ns_zones) %>%
               dplyr::rename(
                 sub_region_1 = .data$ZONE_LABEL,
                 value = .data$res_cases
               ) %>%
               helper_cum_current(loc = "hr", val, prov, date_current)
           },
           e_fmt()
         )
       },
       e_val()
      )
    },
    "0e7a1f46-5d31-4267-be97-831172fa7081" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds$features$attributes %>%
                dplyr::select(.data$tests_cumu) %>%
                dplyr::rename(value = .data$tests_cumu) %>%
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

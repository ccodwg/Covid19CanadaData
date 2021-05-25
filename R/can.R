#' Functions to process datasets: CAN
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_can <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {
  # function: province abbreviation to province name (PHAC)
  phac_prov <- function(p) {
    dplyr::case_when(
      # p == "CAN" ~ "Canada", # to use, must strip beginning of phrase
      p == "NL" ~ "Newfoundland and Labrador",
      p == "PE" ~ "Prince Edward Island",
      p == "NS" ~ "Nova Scotia",
      p == "NB" ~ "New Brunswick",
      p == "QC" ~ "Quebec",
      p == "ON" ~ "Ontario",
      p == "MB" ~ "Manitoba",
      p == "SK" ~ "Saskatchewan",
      p == "AB" ~ "Alberta",
      p == "BC" ~ "British Columbia",
      p == "YT" ~ "Yukon",
      p == "NT" ~ "Northwest Territories",
      p == "NU" ~ "Nunavut"#,
      # p == "" ~ "Federal allocation", # to use, must strip anything after (e.g., footnote)
      )
  }
  # process datasets
  switch(
    uuid,
    "fa3f2917-6553-438c-9a6f-2af8d077f47f" = {
      switch(
        val,
        "vaccine_distribution" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_table() %>%
                `[[`(1) %>%
                dplyr::filter(.data$`Vaccine distribution` == phac_prov(prov)) %>%
                dplyr::pull(.data$Total) %>%
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

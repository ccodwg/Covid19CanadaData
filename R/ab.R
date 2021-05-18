#' Functions to process datasets: AB
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_ab <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {
  # set defaults
  prov <- "AB"

  # process datasets
  switch(
    uuid,
    "59da1de8-3b4e-429a-9e18-b67ba3834002" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::select(.data$Alberta.Health.Services.Zone) %>%
                dplyr::count(.data$Alberta.Health.Services.Zone) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  value = .data$n
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
              ds %>%
                dplyr::filter(.data$Case.status == "Died") %>%
                dplyr::count(.data$Alberta.Health.Services.Zone) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", prov, val, date_current)
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              ds %>%
                dplyr::filter(.data$Case.status == "Recovered") %>%
                dplyr::count(.data$Alberta.Health.Services.Zone) %>%
                dplyr::rename(
                  sub_region_1 = .data$Alberta.Health.Services.Zone,
                  value = .data$n
                ) %>%
                helper_cum_current(loc = "hr", prov, val, date_current)
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "ec1acea4-8b85-4c04-b905-f075de040493" = {
      switch(
        val,
        "recovered" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("li") %>%
                grep("Recovered cases", ., value = TRUE) %>%
                readr::parse_number() %>%
                data.frame(
                  value = .
                ) %>%
                dplyr::mutate(
                  name = val,
                  province = prov,
                  date = date_current,
                  value = as.integer(.data$value)
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
                  rvest::html_elements("li") %>%
                  grep("Total tests completed", ., value = TRUE) %>%
                  readr::parse_number() %>%
                  data.frame(
                    value = .
                  ) %>%
                  helper_cum_current(loc = "prov", prov, val, date_current)
              } else if (testing_type == "n_people_tested") {
                ds %>%
                  rvest::html_elements("li") %>%
                  grep("People tested", ., value = TRUE) %>%
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
        e_val()
      )
    },
    "5c34c720-1a89-4486-8217-5f33545af9cb" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                rvest::html_elements("li") %>%
                grep("doses administered", ., value = TRUE) %>%
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
                rvest::html_elements("li") %>%
                grep("Albertans fully immunized", ., value = TRUE) %>%
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

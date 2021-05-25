#' Functions to process datasets: NU
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_nu <- function(uuid, val, fmt, ds,
                       prov, date_current, testing_type) {

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
        e_val()
      )
    },
    "bd18a4e4-bc22-47c6-b601-1aae39667a03" = {
      switch(
        val,
        "vaccine_administration" = {
          switch(
            fmt,
            "prov_cum_current" = {
              ds %>%
                # chop off population table
                magick::image_flop() %>%
                magick::image_crop(geometry = "50%x100%+0+0") %>%
                magick::image_flop() %>%
                # crop to Nunavut summary
                magick::image_crop(geometry = "100%x3%+0+122") %>%
                magick::image_transparent("white", fuzz = 50) %>%
                magick::image_background("white") %>%
                # read data
                tesseract::ocr() %>%
                gsub("\n", "", .) %>%
                stringr::str_split(" ", simplify = TRUE) %>%
                `[`(c(1, 3)) %>%
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
                # chop off population table
                magick::image_flop() %>%
                magick::image_crop(geometry = "50%x100%+0+0") %>%
                magick::image_flop() %>%
                # crop to Nunavut summary
                magick::image_crop(geometry = "100%x3%+0+122") %>%
                magick::image_transparent("white", fuzz = 50) %>%
                magick::image_background("white") %>%
                # read data
                tesseract::ocr() %>%
                gsub("\n", "", .) %>%
                stringr::str_split(" ", simplify = TRUE) %>%
                `[`(3) %>%
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

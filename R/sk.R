#' Functions to process datasets: SK
#' @inherit process_dataset
#' @keywords internal
#' @importFrom rlang .data
process_sk <- function(uuid, val, fmt, ds,
                       date_current, testing_type) {

  # set defaults
  prov <- "SK"

  # process datasets
  switch(
    uuid,
    "95de79d5-5e5c-45c2-bbab-41daf3dbee5d" = {
      switch(
        val,
        "cases" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 2))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                  ) %>%
              dplyr::mutate(value = as.integer(value))
            # append missing HR data (diff between SK total and sum of HRs)
            dat <- dat %>%
              dplyr::add_row(
                sub_region_1 = "Not Reported",
                value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][2]) -
                  sum(dat$value)
            )
            dat
            },
            e_fmt()
          )
        },
        "mortality" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 7))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][7]) -
                    sum(dat$value)
                )
              dat
            },
            e_fmt()
          )
        },
        "recovered" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
              dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                lapply(FUN = function(x) {
                  x %>%
                    dplyr::select(1) %>%
                    dplyr::slice(c(1, 6))
                })
              dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                dplyr::rename(
                  sub_region_1 = .data$X1,
                  value = .data$X2
                ) %>%
                dplyr::mutate(value = as.integer(value))
              # append missing HR data (diff between SK total and sum of HRs)
              dat <- dat %>%
                dplyr::add_row(
                  sub_region_1 = "Not Reported",
                  value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][6]) -
                    sum(dat$value)
                )
              dat
            },
            e_fmt()
          )
        },
        e_val()
      )
    },
    "9736bff9-4bd3-4c04-b9d9-87f60b3d5eb5" = {
      switch(
        val,
        "testing" = {
          switch(
            fmt,
            "hr_cum_current" = {
              # get HR data
                dat <- ds$tabs$tables[[1]]$body[[2]]$cells[1:13] %>%
                  lapply(FUN = function(x) {
                    x %>%
                      dplyr::select(1) %>%
                      dplyr::slice(c(1, 2))
                  })
                dat <- data.frame(matrix(unlist(dat), ncol = 2, byrow = TRUE)) %>%
                  dplyr::rename(
                    sub_region_1 = .data$X1,
                    value = .data$X2
                  ) %>%
                  dplyr::mutate(value = as.integer(value))
                # append missing HR data (diff between SK total and sum of HRs)
                dat <- dat %>%
                  dplyr::add_row(
                    sub_region_1 = "Not Reported",
                    value = as.integer(ds$tabs$tables[[1]]$body[[2]]$cells[[14]][[1]][2]) -
                      sum(dat$value)
                  )
                dat
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

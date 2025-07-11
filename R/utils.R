#' Get list of datasets
#'
#' @returns A named list of datasets and their corresponding information.
#' @export
get_datasets <- function() {
  # check if parsed results are cached
  if (httpcache::hitCache("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/datasets.json")) {
    # return parsed results
    return(httpcache::getCache("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/datasets.json"))
  } else {
    # get dataset list and parse
    resp <- jsonlite::fromJSON(httr::content(
      httpcache::GET("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/datasets.json"),
      as = "text", encoding = "utf-8"), simplifyVector = FALSE)
    # combine active and inactive datasets into single list
    resp <- unlist(unlist(resp, recursive = FALSE), recursive = FALSE)
    names(resp) <- sapply(resp, function(x) x$uuid)
    # cache parsed results
    httpcache::setCache("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive/master/datasets.json", resp)
    # return parsed results
    return(resp)
  }
}

#' Get information on a specific dataset by UUID
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return A named list of available information on the dataset.
#' @export
get_uuid <- function(uuid) {
  tryCatch(
    {
      # get parsed datasets list
      ds <- get_datasets()
      # get UUID from parsed datasets list
      if (uuid %in% names(ds)) {
        resp <- ds[[uuid]]
      } else {
        stop("UUID not found.")
      }
    },
    error = function(e) {
      print(e)
    }
  )
  # return information
  return(resp)
}

#' Get URL of dataset by UUID
#'
#' If the URL is dynamic, the current URL will be calculated and returned.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return The URL of the specified dataset.
#' @export
get_dataset_url <- function(uuid) {
  resp <- get_uuid(uuid)
  url <- resp$url
  if (is.null(url)) {
    url <- dl_dataset_dyn_url(uuid)
  }
  return(url)
}

#' Get argument of dataset by UUID
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param arg The argument to return.
#' @return The named arg of the specified dataset.
#' @export
get_dataset_arg <- function(uuid, arg) {
  resp <- get_uuid(uuid)
  args <- resp$args
  tryCatch(
    {
      a <- args[[arg]]
      if (is.null(a)) {
        cat("Argument", arg, "not specified for this UUID. Returning NA.", fill = TRUE)
        return(NA)
      }
      return(a)
    }
  )
}

#' Download current version of a dataset catalogued in Covid19CanadaArchive: Get dynamic URL
#'
#' Helper function for \code{\link[Covid19CanadaData]{dl_dataset}}:
#' data-specific code to retrieve current URL of a dataset with dynamic URLs.
#' Replicates code included in the "url_fun_r" field of datasets.json.
#' This code is intentionally written to fit on a single line.
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @return The current URL of the specified dataset.
#' @export
dl_dataset_dyn_url <- function(uuid) {
  switch(
    uuid,
    "61cfdd06-7749-4ae6-9975-d8b4f10d5651" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/cases'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "c40d5b7c-f41c-4633-8bc1-a158dedcbf40" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-tests/tests'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "db9a7e2e-1a1f-4b98-a31a-24460910fc2d" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-cases/hospitalized'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "b575a747-e433-43f4-bd86-23c896df8de5" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19/seven-day-average-of-new-covid-cases'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "5783f3d9-93b0-4aaa-ab97-e0e362a084cc" = {
      paste0('https://dashboard.saskatchewan.ca', stringr::str_extract(as.character(rvest::html_node(xml2::read_html('https://dashboard.saskatchewan.ca/health-wellness/covid-19-vaccines/vaccines'), 'body')), '(?<=href=\").*(?=\">CSV)'))
    },
    "bd18a4e4-bc22-47c6-b601-1aae39667a03" = {
      paste0('https://www.gov.nu.ca', rvest::html_attr(rvest::html_elements(rvest::html_node(rvest::read_html('https://www.gov.nu.ca/health/information/covid-19-vaccination'), 'body'), 'img')[[2]], 'src'))
    },
    "7b7be246-cd65-4f35-b354-faa705cacecc" = {
      d1 <- rvest::read_html('https://novascotia.ca/news/search/?dept=180'); d2 <- rvest::html_elements(d1, 'a'); sub('^\\.\\.', 'https://novascotia.ca/news', rvest::html_attr(d2[grep('New Cases of COVID-19|New Hospital Admissions|People in Hospital|COVID-19 Weekly Data Report', rvest::html_text2(d2))][1], 'href'))
    },
    "2e7a5549-92ae-473d-a97a-7b8e0c1ddbbc" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://www.bchu.org/ServicesWeProvide/InfectiousDiseases/Pages/coronavirus.aspx'), 'iframe'), 'src')
    },
    "fe08035c-2c03-4960-a642-bde1fe18c857" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://ckphu.com/current-situation-in-chatham-kent/'), 'iframe'), 'src')
    },
    "83d1fa13-7fb3-4079-b3dc-5bc50c584fd3" = {
      rvest::html_attr(rvest::html_element(rvest::read_html('https://www.kflaph.ca/en/healthy-living/status-of-cases-in-kfla.aspx'), 'iframe'), 'src')
    },
    "fff9248e-fa24-4efb-ae04-000f3e5c815f" = {
      links <- rvest::html_elements(rvest::read_html('https://www.princeedwardisland.ca/en/search/site?f%5B0%5D=type%3Anews&f%5B1%5D=field_news_type%3A22&f%5B2%5D=field_department%3A612'), 'a'); linkstext <- rvest::html_text2(links); paste0('https://www.princeedwardisland.ca', rvest::html_attr(links[grep('new cases|recoveries|additional deaths|in hospital|COVID-19 Update', linkstext)][1], 'href'))
    },
    "25086ee8-6b82-4132-940f-85f3ea1d09e1" = {
      grep('https://www.cihi.ca/sites/default/files/document/scan-data-tables-covid-19-intervention-update\\d*-en\\.xlsx', rvest::html_attr(rvest::html_elements(rvest::read_html('https://www.cihi.ca/en/covid-19-intervention-scan'), 'a'), 'href'), value = TRUE)[1]
    },
    "a2b4d3a5-2aae-41bc-be5f-423f11bb357a" = {
      grep('https://www.cihi.ca/sites/default/files/document/scan-data-tables-covid-19-intervention-update\\d*-fr\\.xlsx', rvest::html_attr(rvest::html_elements(rvest::read_html('https://www.cihi.ca/fr/analyse-des-interventions-liees-a-la-covid-19'), 'a'), 'href'), value = TRUE)[1]
    },
    "211a55f7-3050-48b6-9cf0-6a76595733c3" = {
      paste0('https://www.gov.mb.ca/health/publichealth/surveillance/influenza/', rvest::html_attr(rvest::html_element(rvest::read_html('https://www.gov.mb.ca/health/publichealth/surveillance/influenza/index.html'), 'a[href*=week]'), 'href'))
    },
    "b22d4896-160d-432b-b02a-ba933d14a58a" = {
      stringr::str_extract(as.character(rvest::html_node(rvest::read_html('https://www.saskatchewan.ca/government/health-care-administration-and-provider-resources/treatment-procedures-and-guidelines/emerging-public-health-issues/2019-novel-coronavirus/cases-and-risk-of-covid-19-in-saskatchewan'), 'body')), '(?<=href=\").*/download(?=\")')
    },
    stop("Specified UUID does not exist in datasets.json or does not have a dynamic URL.")
  )
}

#' Call file archive index
#'
#' @param uuid The UUID of the dataset from datasets.json.
#' @param date A character string in YYYY-MM-DD format specifying the date of
#' data to return. Also accepts "latest" (the default), "first" and "all".
#' @param after A character string in YYYY-MM-DD format specifying that data
#' from this date or later should be returned. Ignored if `date` is defined.
#' @param before A character string in YYYY-MM-DD format specifying that data
#' from this date or earlier should be returned. Ignored if `date` is defined.
#' @param remove_duplicates Remove duplicate files from the sample after date
#' filtering? Defaults to `TRUE`. Ignored if a single date is requested.
#' @return Archive file index matching specified UUID and date filters,
#' as a data frame.
#' @export
index_archive <- function(uuid,
                        date = NULL,
                        after = NULL,
                        before = NULL,
                        remove_duplicates = TRUE) {

  # check inputs
  if (!is.null(date)) {
    if (!date %in% c("latest", "first", "all") & is.na(lubridate::ymd(date, quiet = TRUE))) {
      stop("Check format of parameter 'date'.")}}
  if (!is.null(after)) {
    if (is.na(lubridate::ymd(after, quiet = TRUE))) {
      stop("Check format of parameter 'after'.")}}
  if (!is.null(before)) {
    if (is.na(lubridate::ymd(before, quiet = TRUE))) {
      stop("Check format of parameter 'before'.")}}
  match.arg(as.character(remove_duplicates), c(TRUE, FALSE), several.ok = FALSE)

  # get file index for UUID (from cache, if possible)
  ind_url <- paste0("https://raw.githubusercontent.com/ccodwg/Covid19CanadaArchive-index/main/uuid/csv/", uuid, ".csv")
  if (httpcache::hitCache(ind_url)) {
    ind_raw <- httpcache::getCache(ind_url)
  } else {
    ind_raw <- utils::read.csv(ind_url, stringsAsFactors = FALSE)
    ind_raw$file_date <- as.Date(ind_raw$file_date)
    httpcache::setCache(ind_url, ind_raw)
  }
  ind <- ind_raw
  # date and duplicate filtering
  if (all(is.null(date), is.null(after), is.null(before))) {
    cat("No date filters specified, returning latest file...", fill = TRUE)
    ind <- ind[ind$file_date == max(ind$file_date), ]
  } else if (!is.null(date)) {
    if (!is.null(after) | !is.null(before)) {
      warning("Parameter 'date' is defined, ignoring parameters 'after' and 'before'.")
    }
      switch(
        date,
        "latest" = {
          ind <- ind[ind$file_date == max(ind$file_date), ]
        },
        "first" = {
          ind <- ind[ind$file_date == min(ind$file_date), ]
        },
        "all" = {
          # remove duplicates
          if (remove_duplicates) {
            ind <- ind[ind$file_duplicate == 0, ]
          }
        },
        ind <- ind[ind$file_date == lubridate::ymd(date), ] # single date
      )
    } else {
    if (!is.null(after)) {
      ind <- ind[ind$file_date >= lubridate::ymd(after), ]
    }
    if (!is.null(before)) {
      ind <- ind[ind$file_date <= lubridate::ymd(before), ]
    }
      # remove duplicates
      if (remove_duplicates) {
        ind <- ind[ind$file_duplicate == 0, ]
      }
    }

  # return index
  return(ind)
}

#' Create curl handle with relevant options set
#'
#' Helper function for \code{\link[Covid19CanadaData]{dl_dataset}}.
#'
#' @param d Information on dataset from \code{\link[Covid19CanadaData]{get_uuid}}.
#' @return A `curl` handle with relevant options set.
#' @export
create_curl_handle <- function(d) {
  # create handle
  h <- curl::new_handle()
  # add no-cache headers (not always respected)
  curl::handle_setheaders(h,
                          "Cache-Control" = "no-cache",
                          "Pragma" = "no-cache")
  # add user-agent, if requested
  if (!is.null(d$args$user) && d$args$user == "True") {
    curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.14; rv:66.0) Gecko/20100101 Firefox/66.0")
  }
  # don't verify SSL certificate, if requested
  if (!is.null(d$args$verify) && d$args$verify == "False") {
    curl::handle_setopt(h, "ssl_verifypeer" = FALSE)}
  # return handle
  return(h)
}

#' Read dataset into R (based on file extension)
#'
#' Helper function for \code{\link[Covid19CanadaData]{dl_dataset}} and
#' \code{\link[Covid19CanadaData]{dl_archive}}: determine file extension and
#' fetch additional parameters (if necessary) in order to read the file into R.
#'
#' @param file The location of the file to read (may also be a URL).
#' @param d Information on dataset from \code{\link[Covid19CanadaData]{get_uuid}}.
#' @param sep See the parameter in \code{\link[Covid19CanadaData]{dl_dataset}}.
#' @param sheet See the parameter in \code{\link[Covid19CanadaData]{dl_dataset}}.
#' @param port See the parameter in \code{\link[Covid19CanadaData]{dl_dataset}}.
#' @param host See the parameter in \code{\link[Covid19CanadaData]{dl_dataset}}.
#' @return The specified dataset as an R object.
#' @export
read_dataset <- function(file,
                         d,
                         sep = NULL,
                         sheet = NULL,
                         port = NULL,
                         host = NULL) {
  # is file a URL or a file path?
  is_url <- tryCatch(
    {httr::HEAD(file, config = httr::config(ssl_verifypeer = FALSE)); is_url <- TRUE},
    error = function(e) {is_url <- FALSE}
  )
  # get file extension
  file_ext <- d$file_ext
  if (file_ext %in% c("xlsx", "xls")) {
    file_ext <- "excel"
  } else if (file_ext %in% c("jpg", "jpeg", "png", "tiff")) {
    file_ext <- "image"
  }
  # create curl handle and download content from URL
  # HTML content is handled elsewhere
  if (is_url & file_ext != "html") {
    url <- file
    h <- create_curl_handle(d)
    file <- tempfile()
    curl::curl_download(url, file, handle = h)
  }
  # read dataset
  switch(
    file_ext,
    "csv" = {
      if (is.null(sep)) {
        sep <- ","
      }
      dat <- utils::read.csv(file, stringsAsFactors = FALSE, sep = sep)
    },
    "json" = {
      dat <- jsonlite::fromJSON(file)
    },
    "excel" = {
      if (is.null(sheet)) {
        warning("Sheet not specified, reading sheet 1 by default.")
        sheet <- 1
      }
      dat <- readxl::read_excel(file, sheet)
    },
    "image" = {
      dat <- magick::image_read(file)
    },
    "html" = {
      if (!is_url) {
        # if HTML file, just read the HTML
        dat <- xml2::read_html(file)
      } else {
        if (!is.null(d$args$js) && d$args$js == "True") {
          if (is.null(host) & is.null(port)) {
            dat <- webdriver_get(d$uuid)
          } else if (!is.null(d$host) & is.null(port)) {
            dat <- webdriver_get(d$uuid, host = host)
          } else if (is.null(host) & !is.null(port)) {
            dat <- webdriver_get(d$uuid, port = port)
          } else {
            dat <- webdriver_get(d$uuid)
          }
        } else {
          url <- file
          h <- create_curl_handle(d)
          file <- tempfile()
          curl::curl_download(url, file, handle = h)
          dat <- xml2::read_html(file)
        }
      }
    },
    stop("The file extension of this dataset is not supported for reading into R.")
  )
  # return data
  return(dat)
}

#' Get sheet names of an Excel file
#'
#' @param file The location of the file to read (may also be a URL).
#' @return The sheet names of the Excel file as a character vector.
#' @export
get_sheet_names <- function(file) {
  readxl::excel_sheets(file)
}

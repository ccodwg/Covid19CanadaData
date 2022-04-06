#' Functions for scraping Tableau dashboards
#'
#' Functions for scraping Tableau dashboards that return their content as images
#' rather than HTML. Raw data may be gleaned through other methods, such as
#' parsing HTML tooltips.
#'
#' @name tableau
NULL

#' Get Tableau session ID
#'
#' @param url The URL of the Tableau dashboard.
#' @param req_root The root URL of the request to Tableau server.
#' @return The root URL where requests to Tableau server should be directed,
#' made of the URL of the dashboard plus a unique session ID.
#' @export
tableau_session_id <- function(url, req_root) {
  web <- rvest::read_html(url)
  s_id <- web %>%
    rvest::html_element("textarea#tsConfigContainer") %>%
    rvest::html_text2() %>%
    jsonlite::fromJSON() %>%
    `[[`("sessionid")
  return(paste(req_root, "sessions", s_id, sep = "/"))
}

#' Make a request to Tableau tooltip server
#'
#' The data identifying the specific tooltip to request should have `--[boundary]`
#' in place of the initial boundary value so that a random boundary value may
#' be substituted where appropriate.
#'
#' @param url The URL of the Tableau dashboard.
#' @param req_root The root URL of the request to Tableau server.
#' @param post Data to POST identifying the tooltip requested.
#' @return HTML tooltip text.
#' @export
tableau_tooltip <- function(url, req_root, post) {
  # get request URL for tooltip server
  url <- "https://public.tableau.com/views/LGLDHUCOVID-19CaseSummaryDashboard/1_CaseSummary?:embed=y&amp;:showVizHome=no&amp;:host_url=https%3A%2F%2Fpublic.tableau.com%2F&amp;:embed_code_version=3&amp;:tabs=yes&amp;:toolbar=yes&amp;:animate_transition=yes&amp;:display_static_image=yes&amp;:display_spinner=no&amp;:display_overlay=yes&amp;:display_count=yes&amp;:language=en-US&amp;publish=yes&amp;:loadOrderID=0"
  req_root <- "https://public.tableau.com/vizql/w/LGLDHUCOVID-19CaseSummaryDashboard/v/1_CaseSummary"
  post <- '--[boundary]\r\nContent-Disposition: form-data; name="worksheet"\r\n\r\nHospitalizations - Current\r\n--[boundary]\r\nContent-Disposition: form-data; name="dashboard"\r\n\r\n1. Case Summary\r\n--[boundary]\r\nContent-Disposition: form-data; name="tupleIds"\r\n\r\n[3]\r\n--[boundary]\r\nContent-Disposition: form-data; name="vizRegionRect"\r\n\r\n{"r":"viz","x":227,"y":19,"w":0,"h":0,"fieldVector":null}\r\n--[boundary]\r\nContent-Disposition: form-data; name="allowHoverActions"\r\n\r\nfalse\r\n--[boundary]\r\nContent-Disposition: form-data; name="allowPromptText"\r\n\r\ntrue\r\n--[boundary]\r\nContent-Disposition: form-data; name="allowWork"\r\n\r\nfalse\r\n--[boundary]\r\nContent-Disposition: form-data; name="useInlineImages"\r\n\r\ntrue\r\n--[boundary]--\r\n'

  req_url <- paste(tableau_session_id(url, req_root),
                   "commands/tabsrv/render-tooltip-server", sep = "/")
  h <- curl::new_handle()
  # generate 8-digit boundary value
  boundary <- paste(sample(c(letters, LETTERS, 0:9), size = 8, replace = TRUE), collapse = "")
  # add boundary value to data
  post <- gsub("\\[boundary\\]", boundary, post)
  # post <- gsub('\\"', '\"', post)
  # construct curl handle
  content_type <- paste0("multipart/form-data; boundary=", boundary)
  curl::handle_setform(h, curl::form_data(post, content_type))
  # curl::handle_setopt(h, copypostfields = curl::form_data(post, type = content_type))
  # curl::handle_setheaders(h, "content-type" = content_type)
  # curl::handle_setform(h,
  #   'worksheet' = 'Hospitalizations - Current',
  #   'dashboard' = '1. Case Summary',
  #   'tupleIds' = '[3]',
  #   'vizRegionRect' = '{"r":"viz","x":203,"y":4,"w":0,"h":0,"fieldVector":null}',
  #   'allowHoverActions' = 'false',
  #   'allowPromptText' = 'true',
  #   'allowWork' = 'false',
  #   'useInlineImages' = 'true'
  # )
  # resp <- httr::POST(
  #   url = req_url,
  #   config = httr::content_type(content_type),
  #   body = post,
  #   encode = "multipart",
  #   httr::verbose()
  # )
  # resp <- system2("curl", args = c(req_url, "-H", content_type, "--data-raw", paste0("$'", post, "'"), "--compressed"))
  # send request
  resp <- curl::curl_fetch_memory(req_url, handle = h)
  # read response
  jsonlite::prettify(rawToChar(resp$content))

}

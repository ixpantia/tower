#' @title Create a response builder
#' @description Creates a response builder
#' @return A response builder object
#' @export
response_builder <- function() {
  resp <- new.env(parent = emptyenv())
  resp$status <- 200
  resp$headers <- list()
  resp$content_type <- NULL
  resp$cookies <- list()
  resp$body <- NULL
  structure(
    resp,
    class = "response_builder"
  )
}

#' @title Set a header on a response
#' @description Sets or adds a header to a response
#' @param res A response builder object
#' @param name The name of the header
#' @param value The value of the header
#' @return The response builder object
#' @export
set_header <- function(res, name, value) {
  res$headers[[name]] <- value
  return(invisible(res))
}

#' @title Set the status of a response
#' @description Sets the status of a response
#' @param res A response builder object
#' @param status The status to set
#' @return The response builder object
#' @export
set_status <- function(res, status) {
  res$status <- status
  return(invisible(res))
}

#' @title Add a cookie to a response
#' @description Adds a cookie to a response
#' @param res A response builder object
#' @param name The name of the cookie
#' @param value The value of the cookie
#' @return The response builder object
#' @export
add_cookie <- function(res, name, value) {
  res$cookies[[name]] <- value
  return(invisible(res))
}

#' @title Set the content type of a response
#' @description Sets the content type of a response
#' @param res A response builder object
#' @param content_type The content type to set
#' @return The response builder object
#' @export
set_content_type <- function(res, content_type) {
  res$content_type <- content_type
  return(invisible(res))
}

#' @keywords internal
detect_content_type <- function(body) {
  UseMethod("detect_content_type")
}

#' @keywords internal
detect_content_type.default <- function(body) {
  return("application/json")
}

#' @keywords internal
detect_content_type.shiny.tag <- function(body) {
  return("text/html")
}

#' @keywords internal
detect_content_type.shiny.tag.list <- function(body) {
  return("text/html")
}

#' @keywords internal
detect_content_type.list <- function(body) {
  return("application/json")
}

#' @keywords internal
detect_content_type.raw <- function(body) {
  return("application/octet-stream")
}

#' @keywords internal
detect_content_type.character <- function(body) {
  return("text/plain")
}

#' @keywords internal
serialize_body <- function(body, content_type) {
  switch(
    content_type,
    "text/html" = as.character(body),
    "text/plain" = as.character(body),
    "application/json" = as.character(jsonlite::toJSON(body)),
    "application/octet-stream" = as.raw(body)
  )
}

#' @title Add a body to a response
#' @description Adds a body to a response, if no content type is set, it will be detected
#' @param res A response builder object
#' @param body The body to add
#' @return The response builder object
#' @export
add_body <- function(res, body) {
  if (is.null(res$content_type)) {
    set_content_type(res, detect_content_type(body))
  }
  res$body <- body
  return(invisible(res))
}

#' @title Add a body to a response as JSON
#' @description Adds a body to a response as JSON
#' @param res A response builder object
#' @param body The body to add
#' @return The response builder object
#' @export
add_body_json <- function(res, body) {
  set_content_type(res, "application/json")
  res$body <- body
  return(invisible(res))
}

#' @title Build a response
#' @description Builds a response
#' @param res A response builder object
#' @return A 'shiny' response object
#' @export
build_response <- function(res) {
  content_type <- ifelse(
    is.null(res$content_type),
    "text/plain",
    res$content_type
  )
  body <- serialize_body(res$body, content_type)
  status <- res$status
  headers <- res$headers
  cookies <- res$cookies |>
    purrr::imap(cookie_to_header) |>
    purrr::flatten()
  headers <- append(headers, cookies)

  shiny::httpResponse(
    status = status,
    headers = headers,
    content_type = content_type,
    content = body
  )
}

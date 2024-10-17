#' @title Add an HTTP layer to a tower
#' @description Adds an HTTP layer to a tower
#' @param tower A tower object
#' @param method A string containing the HTTP method to match
#' @param path A string containing the path to match
#' @param handler A function to call when the layer is matched
#' @return The tower with the added route
#' @export
add_route <- function(tower, method = "GET", path, handler) {
  handler <- compiler::cmpfun(handler)
  route_handler <- compiler::cmpfun(function(req) {
    if (req$REQUEST_METHOD == method && req$PATH_INFO == path) {
      return(handler(req))
    }
    req$NEXT(req)
  })
  return(add_http_layer(tower, route_handler))
}

#' @title Add a GET route
#' @description Adds a GET route to a tower
#' @param tower A tower object
#' @param path A string containing the path to match
#' @param handler A function to call when the route is matched
#' @return The tower with the added GET route
#' @export
add_get_route <- function(tower, path, handler) {
  add_route(tower, "GET", path, handler)
}

#' @title Add a POST route
#' @description Adds a POST route to a tower
#' @param tower A tower object
#' @param path A string containing the path to match
#' @param handler A function to call when the route is matched
#' @return The tower with the added POST route
#' @export
add_post_route <- function(tower, path, handler) {
  add_route(tower, "POST", path, handler)
}

#' @title Add a PUT route
#' @description Adds a PUT route to a tower
#' @param tower A tower object
#' @param path A string containing the path to match
#' @param handler A function to call when the route is matched
#' @return The tower with the added PUT route
#' @export
add_put_route <- function(tower, path, handler) {
  add_route(tower, "PUT", path, handler)
}

#' @title Add a DELETE route
#' @description Adds a DELETE route to a tower
#' @param tower A tower object
#' @param path A string containing the path to match
#' @param handler A function to call when the route is matched
#' @return The tower with the added DELETE route
#' @export
add_delete_route <- function(tower, path, handler) {
  add_route(tower, "DELETE", path, handler)
}

#' @title Add a PATCH route
#' @description Adds a PATCH route to a tower
#' @param tower A tower object
#' @param path A string containing the path to match
#' @param handler A function to call when the route is matched
#' @return The tower with the added PATCH route
#' @export
add_patch_route <- function(tower, path, handler) {
  add_route(tower, "PATCH", path, handler)
}

#' @title Extract the request body from a JSON request
#' @description Extracts the request body from a JSON request
#' @param req A request object
#' @param ... Additional arguments to pass to \code{\link[jsonlite]{fromJSON}}
#'   when parsing the request body. This will only be used the first time the
#'   request body is parsed. Subsequent calls will return the cached result.
#' @return The R object representation of the body's JSON content
#' @export
req_body_json <- function(req, ...) {
  if (!is.null(req[[".parsed.body.json"]])) {
    return(req[[".parsed.body.json"]])
  }
  body <- tryCatch(
    expr = jsonlite::fromJSON(
      req$rook.input$read_lines(),
      ...
    ),
    error = function(e) {
      list()
    }
  )
  req[[".parsed.body.json"]] <- body
  return(body)
}

#' @title Extract form data from a request
#' @description Extracts form data from a request
#' @param req A request object
#' @return A list containing the form data in the body
#' @export
req_body_form <- function(req) {
  if (!is.null(req[[".parsed.body.form"]])) {
    return(req[[".parsed.body.form"]])
  }
  form <- tryCatch(
    expr = shiny::parseQueryString(req[["rook.input"]]$read_lines()),
    error = function(e) {
      print(e)
      list()
    }
  )
  req[[".parsed.body.form"]] <- form
  return(form)
}

#' @title Extract query parameters from a request
#' @description Extracts query parameters from a request
#' @param req A request object
#' @return A list containing the query parameters
#' @export
req_query <- function(req) {
  if (!is.null(req[[".parsed.query"]])) {
    return(req[[".parsed.query"]])
  }
  query <- tryCatch(
    expr = shiny::parseQueryString(req$QUERY_STRING),
    error = function(e) {
      list()
    }
  )
  req[[".parsed.query"]] <- query
  return(query)
}

#' @keywords internal
split_cookie_pair <- function(.x) {
  stringr::str_split(.x, "=", n = 2)
}

#' @keywords internal
cookie_unescape <- function(.x) {
  .x[2] <- curl::curl_unescape(.x[2])
  stats::setNames(.x[2], .x[1])
}

#' @title Parse cookies
#' @description Parses cookies from a string
#'
#' @param x A string containing the cookies
#'
#' @return A list containing the HTTP cookies
#' @keywords internal
parse_cookies <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  cookie_pairs <- stringr::str_split(x, "; ")
  cookie_pairs <- purrr::map(cookie_pairs, split_cookie_pair)[[1]]
  cookie_pairs <- purrr::map(cookie_pairs, cookie_unescape)
  cookie_pairs <- purrr::flatten(cookie_pairs)
  return(cookie_pairs)
}

#' @keywords internal
cookie_to_header <- function(.x, .y) {
  list(
    "Set-Cookie" = build_http_cookie(.y, .x)
  )
}

#' @title Build a cookie
#' @description Builds an HttpOnly cookie from a key and value
#'
#' @param key A string containing the cookie key
#' @param value A string containing the cookie value
#'
#' @return A string containing the formated cookie
#' @export
build_http_cookie <- function(key, value) {
  glue::glue("{key}={value}; path=/; SameSite=Lax; HttpOnly")
}

#' @title Extract cookies from a request
#' @description Extracts cookies from a request
#' @param req A request object
#' @return A list containing the cookies
#' @export
req_cookies <- function(req) {
  if (!is.null(req[[".parsed.cookies"]])) {
    return(req[[".parsed.cookies"]])
  }
  cookies <- tryCatch(
    expr = parse_cookies(req$HTTP_COOKIE),
    error = function(e) {
      list()
    }
  )
  req[[".parsed.cookies"]] <- cookies
  return(cookies)
}

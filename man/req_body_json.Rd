% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/http_helpers.R
\name{req_body_json}
\alias{req_body_json}
\title{Extract the request body from a JSON request}
\usage{
req_body_json(req, ...)
}
\arguments{
\item{req}{A request object}

\item{...}{Additional arguments to pass to \code{\link[jsonlite]{fromJSON}}
when parsing the request body. This will only be used the first time the
request body is parsed. Subsequent calls will return the cached result.}
}
\value{
A list containing the request body
}
\description{
Extracts the request body from a JSON request
}

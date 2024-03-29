% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/http_helpers.R
\name{add_get_route}
\alias{add_get_route}
\title{Add a GET route}
\usage{
add_get_route(tower, path, handler)
}
\arguments{
\item{tower}{A tower object}

\item{path}{A string containing the path to match}

\item{handler}{A function to call when the route is matched}
}
\value{
A tower object with the route added
}
\description{
Adds a GET route to a tower
}

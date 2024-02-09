# tower

<!-- badges: start -->
[![R-CMD-check](https://github.com/ixpantia/tower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ixpantia/tower/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Dead simple middleware for R Shiny.

## Summary

`tower` is a simple library for adding middleware to Shiny applications.
It is inspired by the [tower](https://docs.rs/tower/latest/tower/) crate for Rust.
It is designed to enable package authors and Shiny developers to extend
Shiny a little bit more than what is usually possible.

You can use  `tower` to add middlewares that forward, modify, or intercept
requests in Shiny applications. This can be useful for adding logging, authentication,
caching, or routing to your Shiny applications.

## Installation

You can install the development version of `tower` from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("ixpantia/tower")
```

## Example

We may want to add a new route to our Shiny application that adds a count
to a counter every time a user visits the route. We can do this with `tower`
by adding a middleware that intercepts the request and increments the counter.

``` r
library(shiny)
library(tower)

# Counter environment
global_counter <- new.env()
global_counter$count <- 0

# Middleware to increment the counter
increment_counter <- function(req) {
  global_counter$count <- global_counter$count + 1
  response_builder() |>
    add_body(paste("Counter is now", global_counter$count)) |>
    build_response()
}

# A very empty Shiny app (not necesarry for the demo)
ui <- fluidPage()
server <- function(input, output, session) {}

shinyApp(ui, server) |>
  create_tower() |>
  add_get_route("/counter", increment_counter) |>
  build_tower()
```

If you run the code above and visit the route `/counter` in your browser,
you will see the counter increment every time you visit the route.

## How it works

Basically, `tower` adds layers to a Shiny application. A layer is a function
that takes a request and returns either a response or NULL. If a layer returns
a response, the response is sent to the client and the request is not forwarded
to the next layer. If a layer returns NULL, the request is forwarded to the next
layer.

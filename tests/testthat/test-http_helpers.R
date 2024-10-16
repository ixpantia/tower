test_that("can add an arbitraty route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_route("GET", "/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "GET"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("can add a get route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_get_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "GET"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("can add a post route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_post_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "POST"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("can add a put route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_put_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "PUT"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("can add a delete route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_delete_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "DELETE"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("can add a patch route", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_patch_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "text/plain",
        content = "Hello, world!"
      )
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/hello",
    REQUEST_METHOD = "PATCH"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

create_dummy_rook_input <- function(content) {
  list(
    read_lines = function() {
      return(content)
    }
  )
}

test_that("can extract req body as a json", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_post_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_body_json(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "POST"
  request$CONTENT_TYPE <- "application/json"
  request$rook.input <- create_dummy_rook_input('{"secret": "Hello, world!"}')

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")

})

test_that("can extract req body as a json on multiple layers", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_http_layer(function(req) {
      body <- tower::req_body_json(req)
      testthat::expect_equal(body$secret, "Hello, world!")
      req$NEXT(req)
    }) |>
    tower::add_post_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_body_json(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "POST"
  request$CONTENT_TYPE <- "application/json"
  request$rook.input <- create_dummy_rook_input('{"secret": "Hello, world!"}')

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")

})

test_that("can extract req body form", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_post_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_body_form(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "POST"
  request$CONTENT_TYPE <- "application/x-www-form-urlencoded"
  request$rook.input <- create_dummy_rook_input("say=Hello&to=World&secret=Hello%2C%20world%21")

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")

})

test_that("can extract req body form on many layers", {

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_http_layer(function(req) {
      body <- tower::req_body_form(req)
      testthat::expect_equal(body$secret, "Hello, world!")
      req$NEXT(req)
    }) |>
    tower::add_post_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_body_form(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "POST"
  request$CONTENT_TYPE <- "application/x-www-form-urlencoded"
  request$rook.input <- create_dummy_rook_input("say=Hello&to=World&secret=Hello%2C%20world%21")

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")

})

test_that("can extract req query", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_get_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_query(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "GET"
  request$CONTENT_TYPE <- "application/x-www-form-urlencoded"
  request$QUERY_STRING <- "say=Hello&to=World&secret=Hello%2C%20world%21"

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")


})

test_that("can extract req http cookies", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_get_route("/hello", function(req) {
      shiny::httpResponse(
        status = 200,
        content_type = "application/json",
        content = tower::req_cookies(req)$secret
      )
    }) |>
    tower::build_tower()

  request <- new.env()
  request$PATH_INFO <- "/hello"
  request$REQUEST_METHOD <- "GET"
  request$CONTENT_TYPE <- "application/x-www-form-urlencoded"
  request$HTTP_COOKIE <- "say=Hello; to=World; secret=Hello%2C%20world%21"

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "Hello, world!")

})

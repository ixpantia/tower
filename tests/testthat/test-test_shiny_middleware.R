test_that("can build tower with no layers", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )


  tower <- app |>
    tower::create_tower()

  expect_s3_class(tower, "tower")

  app <- tower |>
    tower::build_tower()

  expect_s3_class(app, "shiny.appobj")

})

test_that("http middleware can short-circuit", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_http_layer(function(req) {
      if (req$PATH_INFO == "/") {
        return(shiny::httpResponse(200, "text/plain", "Hello, world!"))
      }
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")

})

test_that("http middleware can be forwarded", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_http_layer(function(req) {
      if (req$PATH_INFO == "/about") {
        return(shiny::httpResponse(200, "text/plain", "About page"))
      }
      req$NEXT(req)
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/html; charset=UTF-8")
  expect_true(stringr::str_detect(response$content, "<!DOCTYPE html>"))

  request <- list(
    PATH_INFO = "/about",
    REQUEST_METHOD = "GET"
  )

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "About page")

})

test_that("http middleware can be forwarded with helpers", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_route("GET", "/about", function(req) {
      return(shiny::httpResponse(200, "text/plain", "About page"))
    }) |>
    tower::build_tower()

  request <- list(
    PATH_INFO = "/",
    REQUEST_METHOD = "GET"
  )

  parts <- app |>
    tower::app_into_parts()

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/html; charset=UTF-8")
  expect_true(stringr::str_detect(response$content, "<!DOCTYPE html>"))

  request <- list(
    PATH_INFO = "/about",
    REQUEST_METHOD = "GET"
  )

  response <- parts$ui(request)

  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "About page")

})

test_that("server middleware can run", {

  server <- function(input, output, session) {
    last_val <- session$userData$r1()
    session$userData$r1(last_val + 1)
  }

  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = server
  )

  app <- app |>
    tower::create_tower() |>
    tower::add_server_layer(function(input, output, session) {
      session$userData$r1 <- shiny::reactiveVal(0L)
    }) |>
    tower::add_server_layer(function(input, output, session) {
      last_val <- session$userData$r1()
      session$userData$r1(last_val + 1)
    }) |>
    tower::build_tower()

  parts <- app |>
    tower::app_into_parts()

  shiny::testServer(parts$server, {
    expect_equal(session$userData$r1(), 2)
  })

})

test_that("expect printable tower", {
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(),
    server = function(input, output, session) {}
  )

  tower <- app |>
    tower::create_tower() |>
    tower::add_http_layer(function(req) {
      if (req$PATH_INFO == "/") {
        return(shiny::httpResponse(200, "text/plain", "Hello, world!"))
      }
    }) |>
    tower::add_server_layer(function(input, output, session) {
      session$userData$r1 <- shiny::reactiveVal(0L)
    })

  expect_snapshot(print(tower))
})

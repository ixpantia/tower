test_that("can build an empty response", {
  response <- tower::response_builder() |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, character(0))
})

test_that("can build a response with a status", {
  response <- tower::response_builder() |>
    tower::set_status(404) |>
    tower::build_response()
  expect_equal(response$status, 404)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, character(0))
})

test_that("can build a response with a content type", {
  response <- tower::response_builder() |>
    tower::set_content_type("application/json") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "{}")
})

test_that("can build a response with a body", {
  response <- tower::response_builder() |>
    tower::add_body("Hello, world!") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")
})

test_that("can build a response with a body and content type", {
  response <- tower::response_builder() |>
    tower::set_content_type("text/html") |>
    tower::add_body("<h1>Hello, world!</h1>") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/html")
  expect_equal(response$content, "<h1>Hello, world!</h1>")
})

test_that("shiny::tags get converted to html", {
  response <- tower::response_builder() |>
    tower::set_content_type("text/html") |>
    tower::add_body(shiny::tags$h1("Hello, world!")) |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/html")
  expect_equal(response$content, "<h1>Hello, world!</h1>")
})

test_that("can build a response with a body as JSON", {
  response <- tower::response_builder() |>
    tower::add_body_json(list(a = 1, b = 2)) |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "{\"a\":[1],\"b\":[2]}")
})

test_that("can build a response with a body as JSON and content type", {
  response <- tower::response_builder() |>
    tower::set_content_type("application/json") |>
    tower::add_body_json(list(a = 1, b = 2)) |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/json")
  expect_equal(response$content, "{\"a\":[1],\"b\":[2]}")
})

test_that("can build a response with a raw body", {
  response <- tower::response_builder() |>
    tower::add_body(as.raw(0:255)) |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_equal(response$content, as.raw(0:255))
})

test_that("can build a response with a raw body and content type", {
  response <- tower::response_builder() |>
    tower::set_content_type("application/octet-stream") |>
    tower::add_body(as.raw(0:255)) |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "application/octet-stream")
  expect_equal(response$content, as.raw(0:255))
})

test_that("can build a response with cookies", {
  response <- tower::response_builder() |>
    tower::add_cookie("name", "value") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, character(0))
  expect_equal(response$headers$`Set-Cookie`, "name=value; path=/; SameSite=Lax; HttpOnly")
})

test_that("can build a response with many cookies", {
  response <- tower::response_builder() |>
    tower::add_cookie("name", "value") |>
    tower::add_cookie("name2", "value2") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, character(0))
  # Get all headers with name "Set-Cookie"
  cookies <- response$headers[names(response$headers) == "Set-Cookie"]
  expect_equal(cookies[[1]], "name=value; path=/; SameSite=Lax; HttpOnly")
  expect_equal(cookies[[2]], "name2=value2; path=/; SameSite=Lax; HttpOnly")
})

test_that("can build a response with a cookie and a body", {
  response <- tower::response_builder() |>
    tower::add_cookie("name", "value") |>
    tower::add_body("Hello, world!") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, "Hello, world!")
  expect_equal(response$headers$`Set-Cookie`, "name=value; path=/; SameSite=Lax; HttpOnly")
})

test_that("can add headers to a response", {
  response <- tower::response_builder() |>
    tower::set_header("X-Test", "test") |>
    tower::build_response()
  expect_equal(response$status, 200)
  expect_equal(response$content_type, "text/plain")
  expect_equal(response$content, character(0))
  expect_equal(response$headers$`X-Test`, "test")
})

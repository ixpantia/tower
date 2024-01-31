library(shiny)
library(tower)

# Counter environment
COUNTER <- new.env()
COUNTER$counter <- 0

# Middleware to increment the counter
increment_counter <- function(req) {
  if (req$PATH_INFO == "/increment") {
    COUNTER$counter <- COUNTER$counter + 1
    return(
      httpResponse(
        200,
        "text/plain",
        paste("Counter is now", COUNTER$counter)
      )
    )
  }
}

# A very empty Shiny app (not necesarry for the demo)
ui <- fluidPage()
server <- function(input, output, session) {}

shinyApp(ui, server) |>
  create_tower() |>
  add_http_layer(increment_counter) |>
  build_tower()

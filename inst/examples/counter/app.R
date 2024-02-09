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

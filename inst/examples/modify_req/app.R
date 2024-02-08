library(shiny)
library(tower)


ui <- fluidPage()
server <- function(input, output) { }

shinyApp(ui, server) |>
  create_tower() |>
  add_http_layer(function(req) {
    req$NEW_DATA <- "new data"
    return(NULL)
  }) |>
  add_http_layer(function(req) {
    req$NEW_DATA <- paste0(req$NEW_DATA, " and more data")
    return(NULL)
  }) |>
  add_http_layer(function(req) {
    print(req$NEW_DATA)
    return(NULL)
  }) |>
  build_tower()

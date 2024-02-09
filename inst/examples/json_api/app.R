library(tower)
library(shiny)
library(dplyr)

filter_iris <- function(req) {
  species <- req_query(req)$species
  if (is.null(species)) {
    return(
      response_builder() |>
        set_status(400) |>
        add_body("species parameter is required") |>
        build_response()
    )
  }
  response_data <- iris |>
    dplyr::filter(Species == species)
  response_builder() |>
    set_status(200) |>
    add_body_json(response_data) |>
    build_response()
}

ui <- fluidPage(
  titlePanel("Iris data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Species", choices = unique(iris$Species))
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  output$table <- renderTable({
    iris |>
      dplyr::filter(Species == input$species)
  })
}

shinyApp(ui, server) |>
  create_tower() |>
  add_get_route("/iris", filter_iris) |>
  build_tower()

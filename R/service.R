#' @keywords internal
compiler_options <- list(optimize = 3L)

#' @title Create a new tower
#' @description Create a new tower to build upon.
#' @param app A shiny app object
#' @return A new tower
#' @export
create_tower <- function(app) {
  structure(
    list(
      app = app,
      server_layers = list(),
      http_layers = list()
    ),
    class = c("tower")
  )
}

#' @title Print a tower
#' @description Print a tower
#' @param x A tower
#' @export
print.tower <- function(x, ...) {
  cat(
    "Tower:", length(x$server_layers), "server layers,",
    length(x$http_layers), "http layers\n"
  )
}

#' @title Add an HTTP layer to a tower
#' @description Add an HTTP layer to a tower. This layer
#'   will be called before the Shiny app's httpHandler.
#' @param tower A tower
#' @param layer A function that takes a request and returns either
#'   a response or NULL. NULL indicates that the layer did not
#'   short-circuit the request, therefore the next layer should be
#'   called. If a response is returned, the request is short-circuited
#'   and the response is returned to the client.
#' @return The tower with the layer added
#' @export
add_http_layer <- function(tower, layer) {
  tower$http_layers <- c(
    tower$http_layers,
    compiler::cmpfun(layer, options = compiler_options)
  )
  return(tower)
}

#' @title Add a server layer to a tower
#' @description Add a server layer to a tower. This layer
#'   will run before the Shiny app's server function. This
#'   is useful for adding custom logic to the server function
#'  without modifying the original server function.
#' @param tower A tower
#' @param layer A function that takes input, output, and session
#'   and has no return value. This function will be called before
#'   the original server function. If you want to short-circuit
#'   the server use an exception.
#' @return The tower with the layer added
#' @export
add_server_layer <- function(tower, layer) {
  tower$server_layers <- c(
    tower$server_layers,
    compiler::cmpfun(layer, options = compiler_options)
  )
  return(tower)
}

#' @keywords internal
build_http_handler <- function(tower) {
  app_handler <- compiler::cmpfun(
    tower$app$httpHandler,
    options = compiler_options
  )
  http_layers <- append(tower$http_layers, app_handler)
  handler <- function(req) {
    for (layer in http_layers) {
      response <- layer(req)
      if (!is.null(response)) {
        return(response)
      }
    }
  }
  return(compiler::cmpfun(handler, options = compiler_options))
}

#' @keywords internal
build_server <- function(tower) {

  server <- compiler::cmpfun(tower$app$serverFuncSource())
  layers <- append(tower$server_layers, list(server))

  if (length(layers) > 1) {
    server <- compiler::cmpfun(
      function(input, output, session) {
        for (layer in layers) {
          layer(input, output, session)
        }
      },
      options = compiler_options
    )
  }

  return(
    compiler::cmpfun(
      function() server,
      options = compiler_options
    )
  )

}

#' @title Build a Shiny app from a tower
#' @description Build a Shiny app from a tower. This will create
#'   a new Shiny app with the specified layers added.
#' @param tower A tower
#' @return A Shiny app object
#' @export
build_tower <- function(tower) {
  app <- tower$app
  app$httpHandler <- build_http_handler(tower)
  app$serverFuncSource <- build_server(tower)
  return(app)
}

#' @title Into parts
#' @description Splits a shiny.appobj into its parts, the ui and server
#' @param app A shiny.appobj
#' @return A list with the ui and server handlers
#' @export
app_into_parts <- function(app) {
  ui <- app$httpHandler
  server <- app$serverFuncSource()
  list(ui = ui, server = server)
}

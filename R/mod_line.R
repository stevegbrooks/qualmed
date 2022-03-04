
#' @export
mod_minimal <- function(dataset, module_id) {
  minimal_server <- function(id, ds) {
    module <- function(input, output, session) {
      ns <- session$ns
    }
    return(shiny::moduleServer(id, module))
  }
  mod <- list(
    ui = function(id) {
      ns <- shiny::NS(id)
      shiny::fluidPage()
    },
    server = rlang::expr(
      minimal_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import ggalluvial
#' @import viridis
#' @export

flowbar_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      position = "right",
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("group"),
          label = "Select a grouping variable",
          choices = "Coding_Level_1",
          selected = "Coding_Level_1",
          multiple = FALSE
        ),
        width = 2
      ),
      shiny::mainPanel(
        shiny::fillCol(
          shiny::plotOutput(
            ns("plot"),
            width = "100%",
            height = "auto"
          )
        ),
        width = 10
      )
    )
  )
}

#' @describeIn flowbar_UI
#'
#' Line Plot Server function
#'
#' @param ds A data.framish dataset. **Reactive**.
#' @return Nothing
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#' @import ggfittext
#' @export
flowbar_server <- function(id, ds) {
  module <- function(input, output, session) {
    ns <- session$ns
    StatStratum <- ggalluvial::StatStratum
    by_year_group <- shiny::reactive({
      shiny::req(input$group)
      ds() %>%
        dplyr::count(time = year, group = get(input$group)) %>%
        dplyr::filter(!is.na(group))
    }) %>% shiny::debounce(1500)

    shiny::observeEvent(ds(), {
      choices <- sort(names(ds()))
      shiny::updateSelectizeInput(inputId = "group",
                                  choices = choices,
                                  selected = input$group)
    })

    output$plot <- shiny::renderPlot({
      ggplot2::ggplot(
        data = by_year_group(),
        ggplot2::aes(
          x = time,
          y = n,
          stratum = group,
          alluvium = group,
          fill = group,
          label = group
        )
      ) +
        ggalluvial::geom_flow() +
        ggalluvial::geom_stratum(alpha = .5) +
        ggfittext::geom_fit_text(stat = "stratum", width = .8) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "none",
          axis.text = ggplot2::element_text(size = 16)
        ) +
        ggplot2::scale_color_viridis_d(drop = FALSE) +
        ggplot2::scale_fill_viridis_d(drop = FALSE)
    }, height = function() {
      plot_id <- paste0("output_", session$ns("plot"), "_width")
      session$clientData[[plot_id]] * .6
    }
    )
  }
  return(shiny::moduleServer(id, module))
}
#' @export
mod_flowbar <- function(dataset, module_id) {
  mod <- list(
    ui = flowbar_UI,
    server = rlang::expr(
      qualmed::flowbar_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

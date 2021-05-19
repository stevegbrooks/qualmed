#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @export

line_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("group"),
                       label = "Select a grouping variable",
                       choices = c("molecule", "TA", "ROPU"),
                       selected = "ROPU",
                       multiple = FALSE
        ),
        selectizeInput(ns("scales"),
                       label = "Select whether y-axis should be fixed",
                       choices = c("fixed", "free_y"),
                       selected = "fixed"
        ),
        width = 2
      ),
      mainPanel(
        plotly::plotlyOutput(
          ns("plot")
        ),
        width = 10
      )
    )
  )
}

#' @describeIn line_UI
#'
#' Line Plot Server function
#'
#' @param ds A data.framish dataset. **Reactive**.
#' @return Nothing
#'
#'
#' @export
#'
line_server <- function(id, ds) {

  module <- function(input, output, session) {

    ns <- session$ns

    by_source_group_year <- shiny::reactive({
      ds() %>%
        dplyr::count(source, group = get(input$group), year) %>%
        dplyr::filter(!is.na(group))
    }) %>% shiny::debounce(1000)

    output$plot <- plotly::renderPlotly({
      plot <- ggplot2::ggplot(
        by_source_group_year(),
        ggplot2::aes(
          x = year,
          y = n,
          color = group,
          text = paste("group:", group)
        )
      ) +
        ggplot2::theme_bw() +
        ggplot2::facet_wrap(~ source, scales = input$scales) +
        ggplot2::geom_line()

      plotly::ggplotly(
        plot,
        tooltip = c("y", "x", "text"),
        width = 1200,
        height = 650
      )
    })
  }
  return(shiny::moduleServer(id, module))
}

mod_line <- function(dataset, module_id) {
  mod <- list(
    ui = line_UI,
    server = rlang::expr(
      qualmed::line_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

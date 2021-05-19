#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#' @export

stratum_UI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("group"),
                       label = "Select a grouping variable",
                       choices = c("Classification", "Coding_Level_1"),
                       selected = "Classification",
                       multiple = FALSE
        ),
        width = 2
      ),
      mainPanel(
        plotOutput(
          ns("plot")
        ),
        width = 10
      )
    )
  )
}

#' @describeIn stratum_UI
#'
#' Line Plot Server function
#'
#' @param ds A data.framish dataset. **Reactive**.
#' @return Nothing
#'
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @export
stratum_server <- function(id, ds) {

  module <- function(input, output, session) {

    ns <- session$ns

    by_year_group <- shiny::reactive({
      ds() %>%
        dplyr::count(year, group = get(input$group)) %>%
        dplyr::filter(!is.na(group))
    }) %>% shiny::debounce(1000)

    output$plot <- shiny::renderPlot(
      width = 1200, height = 650, {
        ggplot2::ggplot(
          by_year_group(),
          ggplot2::aes(x = year,
                       stratum = group,
                       alluvium = group,
                       y = n,
                       fill = group,
                       label = group)) +
          ggplot2::theme_bw() +
          ggplot2::scale_x_continuous(breaks = seq(2015, 2020, 1)) +
          ggalluvial::geom_flow() +
          ggalluvial::geom_stratum(alpha = .5) +
          ggplot2::geom_text(stat = "stratum", size = 3) +
          ggplot2::theme(legend.position = "none")
      })
  }
  return(shiny::moduleServer(id, module))
}

mod_stratum <- function(dataset, module_id) {
  mod <- list(
    ui = stratum_UI,
    server = rlang::expr(
      qualmed::stratum_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

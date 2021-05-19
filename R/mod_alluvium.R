#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import ggalluvial
#' @export

alluvium_UI <- function(id) {
  ns <- NS(id)
  tagList(
    info_tag("alluvium"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(ns("group"),
                       label = "Select a grouping variable",
                       choices = c("func", "TA", "ROPU"),
                       selected = "func",
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

#' @describeIn alluvium_UI
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
alluvium_server <- function(id, ds) {
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
          data = by_year_group(),
          ggplot2::aes(x = year, y = n, alluvium = group)
        ) +
          ggalluvial::geom_alluvium(
            ggplot2::aes(fill = group, colour = group),
            alpha = .75,
            decreasing = FALSE
          ) +
          ggplot2::scale_x_continuous(
            breaks = seq(2014, 2020, 1)
          ) +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0))
      }
    )
  }
  return(shiny::moduleServer(id, module))
}

mod_alluvium <- function(dataset, module_id) {
  mod <- list(
    ui = alluvium_UI,
    server = rlang::expr(
      qualmed::alluvium_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

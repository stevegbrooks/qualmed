#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @export

line_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tagList(
      shiny::sidebarLayout(
        position = "right",
        shiny::sidebarPanel(
          shiny::selectizeInput(ns("group"),
                                label = "Group variable",
                                choices = "ROPU",
                                selected = "ROPU",
                                multiple = FALSE
          ),
          shiny::selectizeInput(ns("facet"),
                                label = "Facet variable",
                                choices = "source",
                                selected = "source",
                                multiple = FALSE
          ),
          shiny::selectizeInput(ns("scales"),
                                label = "Y-axis fixed or free",
                                choices = c("fixed", "free_y"),
                                selected = "fixed"
          ),
          width = 2
        ),
        shiny::mainPanel(
          shiny::fillCol(
            plotly::plotlyOutput(
              ns("plot"),
              width = "auto"
            )
          ),
          width = 10
        )
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
        dplyr::count(facet = get(input$facet), group = get(input$group), year) %>%
        dplyr::filter(!is.na(group))
    })

    observeEvent(ds(), {
      choices <- sort(names(ds()))
      updateSelectizeInput(inputId = "group",
                           choices = choices,
                           selected = input$group)
      updateSelectizeInput(inputId = "facet",
                           choices = choices,
                           selected = input$facet)
    })

    output$plot <- plotly::renderPlotly({
      req(by_source_group_year())
      plot <- ggplot2::ggplot(
        by_source_group_year(),
        ggplot2::aes(
          x = year,
          y = n,
          color = group,
          group = group,
          text = paste("group:", group)
        )
      ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0),
                       strip.text.x = ggplot2::element_text(size = 12)) +
        ggplot2::facet_wrap(~ facet, scales = input$scales) +
        ggplot2::geom_line()

      plotly::ggplotly(
        plot,
        tooltip = c("y", "x", "text"),
        height = 800
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

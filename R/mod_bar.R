#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#' @export

bar_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      position = "right",
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("group"),
          label = "Group variable",
          choices = "Classification",
          selected = "Classification",
          multiple = FALSE
        ),
        shiny::selectizeInput(
          ns("facet"),
          label = "Facet variable",
          choices = "ROPU",
          selected = "ROPU",
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

#' @describeIn bar_UI
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
bar_server <- function(id, ds) {

  module <- function(input, output, session) {

    ns <- session$ns

    by_year_group <- shiny::reactive({
      req(input$facet)
      req(input$group)
      ds() %>%
        dplyr::count(facet = get(input$facet), year, group = get(input$group)) %>%
        dplyr::filter(!is.na(group))
    })

    shiny::observeEvent(ds(), {
      choices <- sort(names(ds()))
      shiny::updateSelectizeInput(inputId = "group",
                                  choices = choices,
                                  selected = input$group)
      shiny::updateSelectizeInput(inputId = "facet",
                                  choices = choices,
                                  selected = input$facet)
    })

    output$plot <- shiny::renderPlot({
      ggplot2::ggplot(
        by_year_group(),
        ggplot2::aes(
          fill = group,
          label = group,
          y = n,
          x = year)
      ) +
        ggplot2::geom_col() +
        ggplot2::facet_grid(~ facet) +
        ggplot2::geom_text(
          size = 6,
          position = ggplot2::position_stack(vjust = 0.5)
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 18),
          axis.text = ggplot2::element_text(size = 16),
          strip.text.x = ggplot2::element_text(size = 16)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0))
    }, height = function() {
      plot_id <- paste0("output_", session$ns("plot"), "_width")
      session$clientData[[plot_id]] * .6
    })
  }
  return(shiny::moduleServer(id, module))
}

mod_bar <- function(dataset, module_id) {
  mod <- list(
    ui = bar_UI,
    server = rlang::expr(
      qualmed::bar_server(
        !!module_id,
        ds = shiny::reactive(filtered_datasets()[[!!dataset]])
      )
    ),
    module_id = module_id
  )
  return(mod)
}

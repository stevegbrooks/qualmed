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
  shiny::fluidPage(
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
          choices = "is_clinops",
          selected = "is_clinops",
          multiple = FALSE
        ),
        shiny::radioButtons(
          ns("time"),
          label = "By year or by month",
          choices = c("year", "year_month"),
          selected = "year"
        ),
        width = 2
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(
          ns("plot"),
          width = "auto"
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
      req(input$time)
      ds() %>%
        dplyr::count(facet = get(input$facet), time = get(input$time), group = get(input$group)) %>%
        dplyr::filter(!is.na(group)) %>%
        dplyr::filter(!is.na(facet))
    }) %>% shiny::debounce(1500)

    shiny::observeEvent(ds(), {
      choices <- sort(names(ds()))
      shiny::updateSelectizeInput(inputId = "group",
                                  choices = choices,
                                  selected = input$group)
      shiny::updateSelectizeInput(inputId = "facet",
                                  choices = choices,
                                  selected = input$facet)
    })

    output$plot <- plotly::renderPlotly({
      req(by_year_group())
      plot <- ggplot2::ggplot(
        by_year_group(),
        ggplot2::aes(
          fill = group,
          y = n,
          x = time,
          label = group,
          text = paste("group:", group))
      ) +
        ggplot2::geom_col() +
        ggplot2::facet_wrap(~ facet) +
        ggplot2::scale_fill_discrete(drop = FALSE) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 12),
          axis.text = ggplot2::element_text(size = 12)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0))
      if (length(dev.list()) > 0) {
        print(paste("mod_bar dev.list:", dev.list()))
        dev.off()
      }
      plotly::ggplotly(
        plot,
        tooltip = c("y", "x", "text"),
        height = 800
      )
    })
  }
  return(shiny::moduleServer(id, module))
}
#' @export
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

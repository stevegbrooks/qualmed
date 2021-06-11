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
    shiny::sidebarLayout(
      position = "right",
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("group"),
          label = "Group variable",
          choices = "ROPU",
          selected = "ROPU",
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
        shiny::sliderInput(
          ns("yslider"),
          label = "Y-axis limits",
          min = 0, max = 300, value = c(0, 200)
        ),
        width = 2
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(
          ns("plot"),
          width = "auto",
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

    by_facet_group_time <- shiny::reactive({
      shiny::req(input$facet)
      shiny::req(input$group)
      shiny::req(input$time)
      ds() %>%
        dplyr::count(facet = get(input$facet), group = get(input$group), time = get(input$time)) %>%
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

    shiny::observeEvent(shiny::req(by_facet_group_time()), {
      rng <- c(min(by_facet_group_time()[["n"]]),
               max(by_facet_group_time()[["n"]]))
      shiny::updateSliderInput(inputId = "yslider",
                               value = rng,
                               min = rng[1],
                               max = rng[2])
    })

    output$plot <- plotly::renderPlotly({
      req(by_facet_group_time())
      plot <- ggplot2::ggplot(
        by_facet_group_time(),
        ggplot2::aes(
          x = time,
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
        ggplot2::facet_wrap(~ facet) +
        ggplot2::geom_line() +
        ggplot2::ylim(input$yslider[1], input$yslider[2])
      if (length(dev.list()) > 0) {
        print(paste("mod_line dev.list:", dev.list()))
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

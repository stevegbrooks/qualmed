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
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      position = "right",
      shiny::sidebarPanel(
        shiny::selectizeInput(
          ns("group"),
          label = "Select a grouping variable",
          choices = "func",
          selected = "func",
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
      shiny::req(input$group)
      ds() %>%
        dplyr::count(year, group = get(input$group)) %>%
        dplyr::filter(!is.na(group))
    }) %>% shiny::debounce(5000)

    shiny::observeEvent(ds(), {
      choices <- names(ds())
      shiny::updateSelectizeInput(inputId = "group",
                                  choices = choices,
                                  selected = input$group)
    })

    output$plot <- shiny::renderPlot({
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
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 14)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0))
    }, height = function() {
      plot_id <- paste0("output_", session$ns("plot"), "_width")
      session$clientData[[plot_id]] * .6
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

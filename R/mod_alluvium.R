#' A module that produces a faceted line chart.
#'
#' @description This module presents a faceted line chart of the variables present in the dataset.
#' @import shiny
#' @import dplyr
#' @import plotly
#' @import ggplot2
#' @import ggalluvial
#' @import viridis
#' @import htmltools
#' @import sp
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
            height = "auto",
            hover = shiny::hoverOpts(id = ns("plot_hover"))
          ),
          shiny::htmlOutput(ns("tooltip"))
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
    })

    shiny::observeEvent(ds(), {
      choices <- sort(names(ds()))
      shiny::updateSelectizeInput(
        inputId = "group",
        choices = choices,
        selected = input$group
      )
    })

    shiny::observe({

      offset <- 5
      node_width <- 1 / 4
      alluvium_width <- 1 / 3

      pdf(file = NULL)

      p <- ggplot2::ggplot(
        data = by_year_group(),
        ggplot2::aes(x = year, y = n, alluvium = group)
      ) +
        ggalluvial::geom_alluvium(
          ggplot2::aes(fill = group, colour = group),
          alpha = .6,
          decreasing = FALSE
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(size = 18),
          axis.text = ggplot2::element_text(size = 16)
        ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -30, hjust = 0)) +
        ggplot2::scale_color_discrete(drop = FALSE) +
        ggplot2::scale_fill_discrete(drop = FALSE)

      pbuilt <- ggplot2::ggplot_build(p)

      data_draw <- transform(pbuilt$data[[1]], width = alluvium_width)
      groups_to_draw <- split(data_draw, data_draw$group)
      group_xsplines <- vector(mode = "list", length = length(groups_to_draw))
      group_xsplines <- lapply(groups_to_draw, function(x) {
        if (dim(x)[1] > 1) {
          ggalluvial:::data_to_xspline(x, knot.prop = TRUE)
        } else {
          temp <- ggalluvial:::data_to_xspline(rbind(x,x), knot.prop = TRUE)
        }
      })

      # Convert xspline coordinates to grid object.
      xspline_coords <- lapply(
        group_xsplines,
        function(coords) {
          grid::xsplineGrob(
            x = coords$x,
            y = coords$y,
            shape = coords$shape,
            open = FALSE
          )
        }
      )
      # Use grid::xsplinePoints to draw the curve for each polygon
      xspline_points <- lapply(xspline_coords, grid::xsplinePoints)
      # Define the x and y axis limits in grid coordinates (old) and plot
      # coordinates (new)
      xrange_old <- range(unlist(lapply(
        xspline_points,
        function(pts) as.numeric(pts$x)
      )))
      yrange_old <- range(unlist(lapply(
        xspline_points,
        function(pts) as.numeric(pts$y)
      )))
      xrange_new <- c(1 - alluvium_width / 2, max(pbuilt$data[[1]]$x) + alluvium_width / 2)
      yrange_new <- c(0, sum(pbuilt$data[[1]]$count[pbuilt$data[[1]]$x == 1]))
      # Define function to convert grid graphics coordinates to data coordinates
      new_range_transform <- function(x_old, range_old, range_new) {
        (x_old - range_old[1]) / (range_old[2] - range_old[1]) * (range_new[2] - range_new[1]) + range_new[1]
      }

      # Using the x and y limits, convert the grid coordinates into plot coordinates.
      polygon_coords <- lapply(xspline_points, function(pts) {
        x_trans <- new_range_transform(
          x_old = as.numeric(pts$x),
          range_old = xrange_old,
          range_new = xrange_new
        )
        y_trans <- new_range_transform(
          x_old = as.numeric(pts$y),
          range_old = yrange_old,
          range_new = yrange_new
        )
        list(x = x_trans, y = y_trans)
      })

      output$tooltip <- shiny::renderText(
        if (is.null(input$plot_hover)) {
          NULL
        } else {
          hover <- input$plot_hover
          x_coord <- round(hover$x)

          if (abs(hover$x - x_coord) < (node_width / 2)) {
            node_row <- pbuilt$data[[1]]$x == x_coord & hover$y > pbuilt$data[[1]]$ymin & hover$y < pbuilt$data[[1]]$ymax
            node_label <- pbuilt$data[[1]]$stratum[node_row]
            node_n <- pbuilt$data[[1]]$count[node_row]
            htmltools::renderTags(
              shiny::tags$div(
                node_label, shiny::tags$br(),
                "n =", node_n,
                style = paste0(
                  "position: absolute; ",
                  "top: ", hover$coords_css$y + offset, "px; ",
                  "left: ", hover$coords_css$x + offset, "px; ",
                  "background: gray; ",
                  "padding: 3px; ",
                  "color: white; "
                )
              )
            )$html
          } else {
            hover_within_flow <- sapply(
              polygon_coords,
              function(pol) {
                sp::point.in.polygon(
                  point.x = hover$x,
                  point.y = hover$y,
                  pol.x = pol$x,
                  pol.y = pol$y
                )
              }
            )
            if (any(hover_within_flow)) {
              coord_id <- rev(which(hover_within_flow == 1))[1]
              flow_label <- paste(unique(groups_to_draw[[coord_id]]$stratum), collapse = " -> ")
              flow_n <- groups_to_draw[[coord_id]]$count[1]
              htmltools::renderTags(
                shiny::tags$div(
                  flow_label, shiny::tags$br(),
                  "n =", flow_n,
                  style = paste0(
                    "position: absolute; ",
                    "top: ", hover$coords_css$y + offset, "px; ",
                    "left: ", hover$coords_css$x + offset, "px; ",
                    "background: gray; ",
                    "padding: 3px; ",
                    "color: white; "
                  )
                )
              )$html
            }
          }
        }
      )
      output$plot <- shiny::renderPlot(
        {
          p
        },
        height = function() {
          plot_id <- paste0("output_", session$ns("plot"), "_width")
          session$clientData[[plot_id]] * .6
        }
      )
    })
  }
  return(shiny::moduleServer(id, module))
}
#' @export
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

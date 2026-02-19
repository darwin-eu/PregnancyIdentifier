# Copyright 2024 DARWIN EU®
#
# This file is part of DarwinShinyModules
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' @title PlotPlotly Module Class
#'
#' @include Plot.R
#'
#' @description
#' Plotly module that handles `plotly` objects.
#'
#' @details
#' `Plotly` exposes bindings to interact with the plot programaticaly.
#' Currently, only the `plotly_selected` binding is supported in this module.
#'
#' @export
#'
#' @examples
#' library(DarwinShinyModules)
#'
#' plotlyFun <- function(data) {
#'   plotly::ggplotly(
#'     ggplot(data = data, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'       geom_point() +
#'       theme_bw()
#'   )
#' }
#'
#' plotlyModule <- PlotPlotly$new(fun = plotlyFun, args = list(data = iris))
#'
#' if (interactive()) {
#'   preview(plotlyModule)
#' }
PlotPlotly <- R6::R6Class(
  classname = "PlotPlotly",
  inherit = Plot,

  # Active ----
  active = list(
    #' @field plot (`plotly`) object.
    plot = function() return(private$.plot),

    #' @field source (`character`) Source label for the plotly plot.
    source = function() return(private$.source),

    #' @field bindings (`reactivevalues`) bindings from the plotly object.
    bindings = function() return(private$.bindings)
  ),

  # Private ----
  private = list(
    ## Fields ----
    .plot = NULL,
    .source = NULL,
    .bindings = shiny::reactiveValues(
      selected = NULL
    ),

    ## Methods ----
    .UI = function() {
      shiny::tagList(
        shiny::h3(private$.title),
        plotly::plotlyOutput(shiny::NS(private$.namespace, "plot"), height = private$.height) %>% shinycssloaders::withSpinner()
      )
    },

    .server = function(input, output, session) {
      super$.server(input, output, session)
      output$plot <- plotly::renderPlotly({
        plot <- do.call(what = private$.fun, args = self$args)
        plotly::event_register(p = private$.plot, event = "plotly_selected")
        private$updateBindings()
        return(plot)
      })
    },

    updateBindings = function() {
      shiny::observeEvent(plotly::event_data("plotly_selected", source = private$.source), {
        private$.bindings$selected <- plotly::event_data("plotly_selected", source = private$.source)
      })
    }
  )
)

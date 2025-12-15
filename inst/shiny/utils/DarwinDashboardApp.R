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

# Classes ----
DarwinDashboardApp <- R6::R6Class(
  classname = "DarwinDashboardApp",
  inherit = DarwinShinyModules:::ShinydashboardApp,

  ## Public ----
  public = list(
    launch = function() {
      shiny::addResourcePath(
        prefix = "www/img",
        directoryPath = system.file("www/img", package = "DarwinShinyModules")
      )
      super$launch()
    },

    UI = function() {
      shiny::tags$body(
        shiny::includeCSS(path = system.file(package = "DarwinShinyModules", "www", "theme.css")),
        darwinHeader(),
        shinydashboard::dashboardPage(
          header = shinydashboard::dashboardHeader(
            title = private$.title,
            titleWidth = 300
          ),
          sidebar = shinydashboard::dashboardSidebar(
            shinydashboard::sidebarMenu(
              private$modulesSidebar(private$.appStructure)
            ), width = 300
          ),
          body = shinydashboard::dashboardBody(private$modulesBody(private$.appStructure))
        ),
        customDarwinFooter()
      )
    }
  )
)

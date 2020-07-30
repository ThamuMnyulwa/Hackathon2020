# Author:  ----------------------------------------------------------------
# Description: ui script-----------------------------------------
# Date: -------------------------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  title = "Arcadis Gen",
  
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = div(img(src = "logo.png", width = 200)),
    titleWidth = 300,
    tags$li(
      a(
        strong("ABOUT V1.0"),
        height = 40,
        href = "",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),
  
  # SIDEBAR -----------------------------------------------------------------
  dashboardSidebar(
    width = 0,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    sidebarMenu(
      id = "sidebartabs",
      div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
      menuItem("Visualisations",
        tabName = "visualisations",
        icon = icon("hand-pointer")
      ),
      br(),
      menuItem("Maps",
               tabName = "maps",
               icon = icon("wrench")
     ),
     menuItem("Predictions",
              tabName = "predictions",
              icon = icon("wrench")
      )
    )
  ),
  
  # BODY --------------------------------------------------------------------
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "style.css"),
      
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "https://fonts.googleapis.com/css2?family=Overpass&display=swap"),
    ),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        4,
        bsButton(
          "visualisations.button",
          label = "VISUALISATIONS",
          icon = icon("hand-pointer"),
          style = "success",
          width = "100%"
        )
      ),
      column(
        4,
        bsButton(
          "maps.button",
          label = "MAP",
          icon = icon("wrench"),
          style = "success",
          width = "100%"
        )
      ),
      column(
        4,
        bsButton(
          "predictions.button",
          label = "MODEL DATA",
          icon = icon("check-circle"),
          style = "success",
          width = "100%"
        )
      )
    ),
    tabItems(
      tabItem("visualisations",
              fluidRow(
                tabBox(
                  width = 6,
                  tabPanel(
                    "Box PLot Output",
                    withSpinner(
                      plotOutput("boxplot"),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ),
                tabBox(
                  width = 6,
                  tabPanel(
                    "Summary",
                    tags$h3("Asset Data")
                  )
                ),
                fluidRow(tabBox(
                  width = 6, 
                  tabPanel(
                    "Asset Datatable",
                    withSpinner(
                      dataTableOutput(''),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ),
                tabBox(
                  width = 6,
                  tabPanel(
                    "Failure Datatable",
                    withSpinner(
                      dataTableOutput(''),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ))
              )),
      tabItem("maps",
              fluidRow(
                tabBox(
                  width = 12,
                  tabPanel(
                    "Map",
                    withSpinner(
                      leafletOutput("bbmap", height=1000),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                )
              )),
      tabItem("predictions",
              fluidRow(
                tabBox(
                  width = 6,
                  tabPanel(
                    "Asset Completeness",
                    withSpinner(
                      plotOutput(""),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ),
                tabBox(
                  width = 6,
                  tabPanel(
                    "Asset Summary Statistics",
                    withSpinner(
                      uiOutput(""),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ),
                fluidRow(
                  tabBox(
                  width = 6, 
                  tabPanel(
                    "Asset Datatable",
                    withSpinner(
                      dataTableOutput(''),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                ),
                tabBox(
                  width = 6, 
                  tabPanel(
                    "Failure Datatable",
                    withSpinner(
                      dataTableOutput(''),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    )
                  )
                )
                )
              ))
    )
  )
) # Last bracket

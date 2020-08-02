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
                sidebarPanel(width = 3,
                  stateSelectInput("merchants", "Merchants", levels(processed_data$Merchant_Id), levels(processed_data$Merchant_Id), TRUE),
                  stateSelectInput("towns", "Towns", levels(processed_data$Town), levels(processed_data$Town), TRUE),
                  stateSelectInput("avg_income", "Average Income", levels(processed_data$Avg_Income_3M), levels(processed_data$Avg_Income_3M), TRUE),
                  stateSelectInput("industry", "Industry", levels(processed_data$industry), levels(processed_data$industry), TRUE),
                  stateSelectInput("age_band", "Age", levels(processed_data$Age_Band), levels(processed_data$Age_Band), TRUE),
                  stateSelectInput("gender", "Gender", levels(processed_data$Gender_code), levels(processed_data$Gender_code), TRUE),
                  stateSelectInput("cap_client", "Capitec Client", levels(processed_data$capitec_client), levels(processed_data$capitec_client), TRUE),
                  mySliderInput("range", label = "Amount", min = min(processed_data$Amount), max = max(processed_data$Amount)),
                  uiOutput("ind_plots"),
                  dateRangeInput(
                    inputId = "transaction_date",
                    label = "Transaction Date",
                    start  = min(processed_data$date),
                    end    = max(processed_data$date)
                  ),
                  actionButton("filter_data", "Show Plots")
              ),
                tabBox(
                  width = 9,
                  tabPanel(
                    "Box PLot Output",
                    withSpinner(
                      plotOutput("boxplot"),
                      type = getOption("spinner.type", default = 1),
                      color = getOption("spinner.color", default = "#E4610F")
                    ))
                  ),
              tabBox(
                width = 9,
                tabPanel(
                  "Stacked Bar",
                  withSpinner(
                    plotOutput("stackedbar"),
                    type = getOption("spinner.type", default = 1),
                    color = getOption("spinner.color", default = "#E4610F")
                  ))
          ),
          tabBox(
            width = 12,
            tabPanel(
              "Total Transactions by Industry",
              withSpinner(
                plotOutput("total_transactions"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              )),
            tabPanel(
              "Daily Amount",
              withSpinner(
                plotOutput("daily_amount"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              )),
            tabPanel(
              "Gender Amount",
              withSpinner(
                plotOutput("gender_amount"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              ))
          ),
          tabBox(
            width = 12,
            tabPanel(
              "Complete Industry Spending",
              withSpinner(
                plotOutput("complete_industry_spending"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              )),
            tabPanel(
              "Three Month Transactions",
              withSpinner(
                plotOutput("complete_three_trans"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              )),
            tabPanel(
              "Density Industry",
              withSpinner(
                plotOutput("density_industry"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              )),
            tabPanel(
              "Capitect Clients",
              withSpinner(
                plotOutput("capitec_clients"),
                type = getOption("spinner.type", default = 1),
                color = getOption("spinner.color", default = "#E4610F")
              ))
            
          )
          
          
          
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

# Author:  ----------------------------------------------------------------
# Description: ui script-----------------------------------------
# Date: -------------------------------------------------------------------


navbarPage("Automatic Industry Report", id="main",
           tabPanel("Map", leafletOutput("bbmap", height=1000)),
           tabPanel("Data", DT::dataTableOutput("merchant_datatable"))
           #tabPanel("Read Me",includeMarkdown("readme.md"))
           )
# Author:  ----------------------------------------------------------------
# Description: ui script-----------------------------------------
# Date: 24/7/2020---------------------------------------------------------------

#renv::restore()
#
navbarPage("Automatic Industry Report", id="main",
           tabPanel("Map", leafletOutput("bbmap", height=1000)),
           tabPanel("Data", DT::dataTableOutput("merchant_datatable"))
           #tabPanel("Read Me",includeMarkdown("readme.md"))
           )
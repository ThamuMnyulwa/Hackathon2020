# Author:  ----------------------------------------------------------------
# Description: global script-----------------------------------------
# Date: -------------------------------------------------------------------

# read in packages --------------------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(leaflet)
library(DT)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(summarytools)
library(shinyalert)
library(ggplot2)
library(plotly)

# Data import -------------------------------------------------------------
merchant_data <- read_csv("./data/MerchantData.csv")

# Import external data sources from data file (Google)---------------------
transport_data <- read_csv("./data/Transport_data.csv")
placeofinterest_data <- read_csv("./data/Place_Of_Interest.csv")
competitor_data <- read_csv("./data/Competitor_data.csv")


factor_vec <- c("Merchant_Id ","Terminal_Id_Key","Merchant_Name","Town","Area_Code",
                "Age_Band","Gender_code","Avg_Income_3M",
                "capitec_client","time_of_day","day_of_month",
                "time_of_day","weekday","month","industry")

datatemp <- read_csv("./data/data1.csv") %>%
            select(-c("Card_Value_Spending","Card_Number_Spending","Client_ID"))

datatemp[is.na(datatemp)] <- 'Non-C'
datatemp$Gender_code <- addNA(datatemp$Gender_code)
datatemp$Age_Band <- addNA(datatemp$Age_Band)
datatemp$Avg_Income_3M <- addNA(datatemp$Avg_Income_3M)

processed_data <- datatemp %>% mutate_each_(list(factor), factor_vec)



stateSelectInput <- function(InputId_var, label_var, choices_var, selected_var, multiple_var){
  pickerInput(inputId = InputId_var,
              label = label_var,
              choices = choices_var,
              selected = selected_var,
              multiple = multiple_var,
              options = list(`actions-box` = TRUE))
}

mySliderInput <- function(id, label = id, min = 0, max = 1) {
  sliderInput(id, label, min = min, max = max, value = c(0, 3000), step = 500)
}

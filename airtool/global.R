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

# Improve handling NA's
# raw_data <- read_tsv("https://raw.githubusercontent.com/SUHackathon/data-science-challenge/master/data/business_data.txt", na=c("","NA","NULL",NULL)) %>% 
#             drop_na() %>% 
#             select(-c(Card_Amount_Paid, Military_Time, Province))



factor_vec <- c("Merchant_Id ","Terminal_Id_Key","Merchant_Name","Town","Area_Code",
                "Age_Band","Gender_code","Avg_Income_3M","Card_Value_Spending",
                "capitec_client","time_of_day","day_of_month",
                "time_of_day","weekday","month","industry")

processed_data <- read_csv("./data/data1.csv") %>% mutate_each_(list(factor), factor_vec)

stateSelectInput <- function(InputId_var, label_var, choices_var, selected_var, multiple_var){
  pickerInput(inputId = InputId_var,
              label = label_var,
              choices = choices_var,
              selected = selected_var,
              multiple = multiple_var)
}

mySliderInput <- function(id, label = id, min = 0, max = 1) {
  sliderInput(id, label, min = min, max = max, value = c(0, 3000), step = 500)
}


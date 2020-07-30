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


# Data import -------------------------------------------------------------
merchant_data <- read_csv("./data/MerchantData.csv")

# Improve handling NA's
raw_data <- read_tsv("https://raw.githubusercontent.com/SUHackathon/data-science-challenge/master/data/business_data.txt", na=c("","NA","NULL",NULL)) %>% 
            drop_na() %>% 
            select(-c(Card_Amount_Paid, Military_Time, Province))

# Author:  ----------------------------------------------------------------
# Description: global script-----------------------------------------
# Date: -------------------------------------------------------------------

# read in packages --------------------------------------------------------
library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(DT)

# Data import -------------------------------------------------------------
merchant_data <- read_csv("./data/MerchantData.csv")

raw_data <- read_tsv("https://raw.githubusercontent.com/SUHackathon/data-science-challenge/master/data/business_data.txt", na=c("","NA","NULL",NULL))

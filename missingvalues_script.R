# Improve handling NA's

library(readr)
library(curl)
library(tidyr)
library(dplyr)
library(stringr)


raw_data <- read_tsv("https://raw.githubusercontent.com/SUHackathon/data-science-challenge/master/data/business_data.txt", na=c("","NA","NULL",NULL))

# Count Na's column wise --------------------------------------------------------
na_count <-sapply(raw_data, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count


# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html

# Visualize columns with missing values -----------------------------------------

gg_miss_which(raw_data)

# Visualize overall missing -----------------------------------------------------

# Visualize missing data
# install.packages('naniar')
library(naniar)
vis_miss(raw_data,warn_large_data=FALSE)


# Exploring patterns with UpSetR -----------------------------------------------------
# install.packages('UpSetR')
library(UpSetR)
gg_miss_upset(raw_data)

n_var_miss(raw_data)

################################################################################

# Drop the single missing value in Capitec clients data

data1 = raw_data %>% drop_na("Town")

# Visualize columns with missing values -----------------------------------------
gg_miss_which(data1)

# Count Na's column wise --------------------------------------------------------
na_count <-sapply(data1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Explote pattern of missingness again ---------------------------------------
gg_miss_upset(data1)

n_var_miss(data1) # Count columns with no missing values

# Exploring Missingness Mechanisms ---------------------------------------------
library(ggplot2)

# using regular geom_point()
ggplot(data1,
       aes(x = factor(Town),
           y = Card_Amount_Paid)) +
  geom_point()


# Facets!
ggplot(data1,
       aes(x = factor(Merchant_Id),
           y = Card_Amount_Paid)) +
  geom_miss_point() + 
  facet_wrap(~factor(Town))

# Guys
ggplot(data1,
       aes(x = factor(Merchant_Id),
           y = Avg_Income_3M)) +
  geom_miss_point() + 
  facet_wrap(~factor(Town))

##################################################################

# data2 = data1 %>% select(-c(Card_Value_Spending, Client_ID, Military_Time, Province))

# Explote pattern of missingness again --------------------------------
vis_miss(data1,warn_large_data=FALSE)

# Count Na's column wise ----------------------------------------------
na_count <-sapply(data1, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

# Create a feature Capitec Clients
data1 <- data1 %>% mutate()

# Create feature showing month of transaction in order to use in visualization
# https://statistics.berkeley.edu/computing/r-dates-times

# dtparts = data1$Tran_Date
# #dtparts = format(dtparts,'%A, %B %d, %Y %H:%M:%S')
# data1$Month = format(dtparts,'%B')

# Create a feature for Transaction Industry
library(lubridate)

wday(data1$Tran_Date, label = TRUE)


data1 <- data1 %>% mutate(date = lubridate::date(Tran_Date),
                          day_of_month = lubridate::day(Tran_Date),
                          time_of_day = lubridate::hour(Tran_Date),
                          weekday = wday(Tran_Date, label = TRUE),
                          month = lubridate::month(Tran_Date),
                          capitec_client = if_else(is.na(Client_ID),"No", "Yes"),
                          industry = case_when(str_detect(Merchant_Name, "Liquor") ~ "Liquor",
                                               str_detect(Merchant_Name, "Market") ~ "MiniMarket",
                                               str_detect(Merchant_Name, "Rest") ~ "Restaurant",
                                               str_detect(Merchant_Name, "Coffee") ~ "CoffeeShop",
                                               str_detect(Merchant_Name, "Fast") ~ "FastFood",
                                               str_detect(Merchant_Name, "Beauty") ~ "Beauty",
                                       TRUE ~ "Unknown"))

# Create Industry Specific datasets --------------------

data_MiniMarket <- data1[data1$industry == "MiniMarket",]
data_Liquor <- data1[data1$industry == "Liquor",]
data_Restaurant <- data1[data1$industry == "Restaurant",]
data_CoffeeShop <- data1[data1$industry == "CoffeeShop",]
data_FastFood <- data1[data1$industry == "FastFood",]
data_Beauty <- data1[data1$industry == "Beauty",]  




# Improve handling NA's

library(readr)
#install.packages('curl')
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
       aes(x = factor(data1$Town),
           y = data1$Card_Amount_Paid)) +
  geom_point()


# Facets!
ggplot(data1,
       aes(x = factor(data1$Merchant_Id),
           y = data1$Card_Amount_Paid)) +
  geom_miss_point() + 
  facet_wrap(~factor(Town))

# Guys
ggplot(data1,
       aes(x = factor(data1$Merchant_Id),
           y = data1$Avg_Income_3M)) +
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
data1 <- data1 %>% mutate(Capitec_client = if_else(is.na(Client_ID),"No", "Yes"))

# Create a feature for Transaction Industry

data1 <- data1 %>% mutate( Industry = case_when(str_detect(Merchant_Name, "Liquor") ~ "Liquor",
                                               str_detect(Merchant_Name, "Market") ~ "MiniMarket",
                                               str_detect(Merchant_Name, "Rest") ~ "Restaurant",
                                               str_detect(Merchant_Name, "Coffee") ~ "CoffeeShop",
                                               str_detect(Merchant_Name, "Fast") ~ "FastFood",
                                               str_detect(Merchant_Name, "Beauty") ~ "Beauty",
                                       TRUE ~ "Unknown"))

                            
                                                                  


# Create Industry Specific datasets --------------------

data_MiniMarket <- data1[data1$Industry == "MiniMarket",]
data_Liquor <- data1[data1$Industry == "Liquor",]
data_Restaurant <- data1[data1$Industry == "Restaurant",]
data_CoffeeShop <- data1[data1$Industry == "CoffeeShop",]
data_FastFood <- data1[data1$Industry == "FastFood",]
data_Beauty <- data1[data1$Industry == "Beauty",]  


# Card_Value_Spending and Card_Number_Spending are not the same
all.equal(data1$Card_Value_Spending, data1$Card_Number_Spending)

# Amount and Card_Amount_Paid are not the same
all.equal(data1$Amount,data1$Card_Amount_Paid)
identical(data1$Amount,data1$Card_Amount_Paid)

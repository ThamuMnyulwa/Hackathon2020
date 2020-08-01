## Alcohol ###################################################

# Terminals--------------------------------------------------------------------------

# Individual terminals plot

ggplot(aes(x = factor(Merchant_Id), y = factor(Terminal_Id_Key)), data =
         data_Liquor) +
  geom_point() +
  ggtitle('Merchant_Id vs Terminal_Id_Key in Liquor Industry') +
  xlab('Merchant_Id') +
  ylab('Terminal_Id_Key')

# Terminals Bar chart

# Barchart showing number of terminals by unique merchant Id
count.df <-
  with(data_Liquor, aggregate(
    cbind(Terminal_Id_Key) ~ Merchant_Id,
    FUN = function(x) {
      length(unique(x))
    }
  ))

plot <-
  ggplot(count.df, aes(x = factor(Merchant_Id), y = Terminal_Id_Key))
plot + geom_bar(stat = "identity") +
  labs(
    title = "Number of terminals for each merchant in Liquor Industry",
    subtitle = "Counted over each Capitec Client",
    y = "Number of Terminals",
    x = "Merchant Id"
  ) +
  theme(legend.position = "top")


#----Total swipes---------------------------------------------------------------


# Count the total swipes by the Merchant Id and plot
count.df <-
  with(data_Liquor, aggregate(
    cbind(month, Amount) ~ Merchant_Id,
    FUN = function(x) {
      length(x)
    }
  ))

plot <- ggplot(count.df, aes(x = factor(Merchant_Id), y = Amount))
plot + geom_bar(stat = "identity") +
  labs(
    title = "Number of transactions for each merchant in Liquor Industry",
    subtitle = "Card swipes/taps counted over all Clients",
    y = "Number of Transactions",
    x = "Merchant Id"
  ) +
  theme(legend.position = "none")

# Take month into account as a variable
count.df <-
  data.frame(table(data_Liquor$Merchant_Id, data_Liquor$month))
colnames(count.df) <- c('Merchant_Id', 'Month', 'Freq')

#-- Create a new variable showing merchants proportion of sales (merchant that month)/(that months ammount)
count.df

plot <-
  ggplot(count.df, aes(x = factor(Month), y = Freq, fill = Merchant_Id))
plot + geom_bar(stat = "identity") +
  labs(
    title = "Number of transactions for each merchant in Liquor Industry",
    subtitle = "Card swipes/taps counted over all Clients",
    y = "Number of Transactions",
    x = "Merchant Id"
  )

#----Average amount---------------------------------------------------------------

# Count the average of the total swipes amount by the Merchant Id and plot
count.df <-
  with(data_Liquor, aggregate(
    cbind(Amount) ~ Merchant_Id,
    FUN = function(x) {
      length(x)
    }
  ))

plot <- ggplot(count.df, aes(x = factor(Merchant_Id), y = Amount))
plot + geom_bar(stat = "identity") +
  labs(
    title = "Average over three month period of transactions for each merchant in Liquor Industry",
    subtitle = "Card swipes/taps counted over all Clients",
    y = "Number of Transactions",
    x = "Merchant Id"
  ) +
  theme(legend.position = "none")


#Unique clients

plot(
  liquor.industry[, "Unique_clients"],
  main = "Number of unique clients",
  xaxt = "n",
  xlab = "Merchant",
  ylab = "Unique clients",
  pch = 20,
  type = "b"
)

axis(side = 1,
     at = c(1, 2, 3),
     labels = c(rownames(liquor.industry)))

#Income groups

#Merchant 1

plot(
  liquor.industry["Liquor 1", 6:11],
  main = c("Income group of clients", "Liquor 1"),
  xaxt = "n",
  xlab = "Merchant",
  ylab = "Proportion of clients",
  pch = 20,
  type = "b"
)

axis(
  side = 1,
  at = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "a.No inflows",
    " b.R1 - R2,999",
    "c.R3,000 - R7,499",
    "d.R7,500 - R14,999",
    "e.R15,000 - R29,999",
    "f.R30,000+"
  )
)

#Merchant 2

plot(
  liquor.industry["Liquor 2", 6:11],
  main = c("Income group of clients", "Liquor 2"),
  xaxt = "n",
  xlab = "Merchant",
  ylab = "Proportion of clients",
  pch = 20,
  type = "b"
)

axis(
  side = 1,
  at = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "a.No inflows",
    " b.R1 - R2,999",
    "c.R3,000 - R7,499",
    "d.R7,500 - R14,999",
    "e.R15,000 - R29,999",
    "f.R30,000+"
  )
)

#Merchant 3

plot(
  liquor.industry["Liquor 3", 6:11],
  main = c("Income group of clients", "Liquor 3"),
  xaxt = "n",
  xlab = "Merchant",
  ylab = "Proportion of clients",
  pch = 20,
  type = "b"
)

axis(
  side = 1,
  at = c(1, 2, 3, 4, 5, 6),
  labels = c(
    "a.No inflows",
    " b.R1 - R2,999",
    "c.R3,000 - R7,499",
    "d.R7,500 - R14,999",
    "e.R15,000 - R29,999",
    "f.R30,000+"
  )
)

ggplot(`rio::import('data1.csv')`) +
  aes(x = Card_Amount_Paid, y = Amount) +
  geom_point(size = 1L, colour = "#0c4c8a") +
  theme_minimal()


# code 1 ------------------------------------------------------------------

ggplot(data=data1 %>% filter(Amount<= 3000)) +
  aes(x = weekday, y = Amount, fill = NULL) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()


# Code 2 ------------------------------------------------------------------
ggplot(data=data1 %>% filter(Amount<= 3000)) +
  aes(x = capitec_client, y = Amount) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


ggplot(`rio::import('data1.csv')`) +
  aes(x = capitec_client, y = Amount) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()


# Code 3 ------------------------------------------------------------------

ggplot(data=data1%>% filter(Amount<= 3000)) +
  aes(x = capitec_client, y = Amount, fill = industry) +
  geom_boxplot() +
  scale_fill_hue() +
  theme_minimal()


ggplot(`rio::import('data1.csv')`) +
  aes(x = time_of_day, fill = Industry, size = Amount) +
  geom_histogram(bins = 30L) +
  scale_fill_hue() +
  theme_minimal()


# Export to ggplotbuilder2
data2 = processed_data[sample(nrow(processed_data),5000),]
dim(data2)

write_csv(data2,'data2.csv')

install.packages("esquisse")
library(esquisse)
esquisse::esquisser()

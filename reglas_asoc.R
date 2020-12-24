library(tidyverse)
library(arules)

groceries = read.csv("Groceries_dataset.csv")
head(groceries)


groceries$Date <- as.Date(groceries$Date, "%d-%m-%Y")
groceries$itemDescription <- as.factor(groceries$itemDescription)
groceries$Member_number <- as.factor(groceries$Member_number)

categories <- subset(groceries$itemDescription)


transactions <- groceries %>% group_by(Member_number, Date) %>% group_split()
head(transactions)
class(transactions)

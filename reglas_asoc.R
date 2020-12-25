library(tidyverse)
library(arules)

groceries = read.csv("Groceries_dataset.csv")
head(groceries)

groceries$Date = as.Date(groceries$Date, "%d-%m-%Y")
groceries$itemDescription = as.factor(groceries$itemDescription)
groceries$Member_number <- as.factor(groceries$Member_number)

str(groceries)

groupings <- groceries %>% group_by(Member_number, Date) %>% group_split()
head(groupings)

take_items = function (tib){
  a = tib[,3][[1]]
  return(a)
}

trans_itemsets = lapply(groupings, take_items)
  # we should give an index to every transaction in order to continue
names(trans_itemsets) = 1:length(trans_itemsets)
head(trans_itemsets)


transactions = as(trans_itemsets, "transactions")


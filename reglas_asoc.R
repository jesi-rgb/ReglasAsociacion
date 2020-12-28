#### CREDIT CARDS ####

# A manager at the bank is disturbed with more and more customers leaving their
# credit card services. They would really appreciate if one could predict for them
# who is gonna get churned so they can proactively go to the customer to provide 
# them better services and turn customers' decisions in the opposite direction
# I got this dataset from a website with the URL as https://leaps.analyttica.com/home.
# I have been using this for a while to get datasets and accordingly work on them 
# to produce fruitful results. The site explains how to solve a particular business 
# problem. Now, this dataset consists of 10,000 customers mentioning their age, 
# salary, marital_status, credit card limit, credit card category, etc. There are 
# nearly 18 features. We have only 16.07% of customers who have churned. Thus, it's 
# a bit difficult to train our model to predict churning customers.


library(tidyverse)
library(arules)
library (arulesViz)
cards = read.csv("BankChurners.csv")
head(cards)

# especificado en la documentacion que se eliminara
cards["Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1"] = NULL
cards["Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"] = NULL


str(cards)

groupings <- cards %>% group_by(Attrition_Flag) %>% group_split()
cards_trans = as(bind_rows(groupings), "transactions")

inspect(head(cards_trans, n=10))

aCards = apriori(cards_trans, parameter = list(support = 0.01, confidence = 0.8, minlen=2, maxlen=4, target = "rules"))
aCards = sort(aCards, by="confidence")
inspect(head(aCards, n=10))
barplot(table(size(aCards)), xlab="itemset size", ylab="count")

inspect(head(subset(aCards, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), n=10))
redundant <- is.redundant(x = aCards, measure = "confidence")
rulesPruned <- aCards[!redundant] 

aCards
rulesPruned # 80% of the rules out 73004 -> 19906
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage" ,"phi", "gini"), transactions=cards_trans)
head(mInteres)

plot(rulesPruned)
plot(subset(aCards, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), method="grouped")


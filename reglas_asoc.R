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
# eliminamos el id del cliente ya que no aporta ninguna información útil
cards["CLIENTNUM"] = NULL

str(cards)

# Attrition_Flag Gender Education_Level Marital_Status Income_Category Card_Category 
cards$Attrition_Flag = factor(cards$Attrition_Flag, levels = c("Attrited Customer", "Existing Customer"))
cards$Gender = factor(cards$Gender, levels = c("F", "M"))
cards$Education_Level = factor(cards$Education_Level, levels = c("Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate", "Unknown"))
cards$Marital_Status = factor(cards$Marital_Status, levels = c("Single", "Married", "Divorced", "Unknown"))
cards$Income_Category = factor(cards$Income_Category, levels = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown"))
cards$Card_Category = factor(cards$Card_Category, levels = c("Blue", "Silver", "Gold", "Platinum"))

head(cards)
str(cards)

# quick plot for all variables
cards %>%
  keep(is.numeric) %>% # keep only numeric values
  gather() %>% # gather into key value
  ggplot(aes(value)) + # put only value and then facet by every key
  facet_wrap(~ key, scales = "free") +
  geom_histogram(aes(fill=key)) + 
  ggtitle("Distribution for all numeric values") + theme(plot.title = element_text(face = "bold"))

head(cards)


cards %>%
  keep(is.character) %>% # keep only numeric values
  gather() %>% # gather into key value
  ggplot(aes(value)) + # put only value and then facet by every key
  facet_wrap(~ key, scales = "free") +
  geom_bar(aes(fill=key)) + 
  ggtitle("Distribution for all categoric values") + theme(plot.title = element_text(face = "bold"))


# preprocess para hacer transactions
groupings <- cards %>% group_by(Attrition_Flag) %>% group_split()
cards_trans = as(bind_rows(groupings), "transactions")

# comprobar los diferentes niveles asignados a las variables
cbind(cards_trans@itemInfo[["labels"]], cards_trans@itemInfo[["levels"]])

# working with transactions 
inspect(head(cards_trans, n=10))

aCards = apriori(cards_trans, parameter = list(support = 0.1, confidence = 0.8, minlen=2, maxlen=4, target = "rules"))
aCards = sort(aCards, by="support")
inspect(head(aCards, n=10))


aCards1 = apriori(cards_trans, parameter = list(support = 0.01, confidence = 0.8, minlen=2, maxlen=4, target = "rules"))
aCards1 = sort(aCards1, by="support")
inspect(head(subset(aCards, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), n=20))

redundant1 <- is.redundant(x = aCards1, measure = "confidence")
rulesPruned1 <- aCards1[!redundant1] 
inspect(head(subset(rulesPruned1, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), n=20))

aCards2 = apriori(cards_trans, parameter = list(support = 0.001, confidence = 0.8, minlen=2, maxlen=4, target = "rules"))
aCards2 = sort(aCards2, by="support")
inspect(head(subset(aCards, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), n=20))

redundant2 <- is.redundant(x = aCards2, measure = "confidence")
rulesPruned2 <- aCards2[!redundant2] 
inspect(head(subset(rulesPruned2, subset = (rhs %in% "Attrition_Flag=Attrited Customer")), n=20))

inspect(head(subset(rulesPruned2, subset = ((lhs %pin% "Total_Ct_Chng_Q4_Q1=") & (rhs %in% "Attrition_Flag=Attrited Customer")))))
inspect(head(subset(rulesPruned2, subset = ((lhs %pin% "Total_Amt_Chng_Q4_Q1=") & (rhs %in% "Attrition_Flag=Attrited Customer")))))



plot(rulesPruned2)
plot(subset(rulesPruned2, subset = (rhs %in% "Attrition_Flag=Attrited Customer"))) 


inspect(head(subset(rulesPruned2, subset = ((lhs %pin% "Customer_Age=") & (rhs %in% "Attrition_Flag=Attrited Customer")))))
plot(subset(rulesPruned2, subset = ((lhs %pin% "Customer_Age=") & (rhs %in% "Attrition_Flag=Attrited Customer"))), method="paracoord", reorder=TRUE)

plot(rulesPruned2)


#### MAKING NEGATIVE ITEMSETS
# copy original dataset in a new variable
neg_cards = cards

# spread the marital status over the corresponding columns
options = unique(unlist(cards$Marital_Status, recursive=FALSE))
for(o in options){
  neg_cards$newcol = rep(FALSE)
  neg_cards <- rename(neg_cards, !!o := newcol)
  neg_cards[grep(o, neg_cards$Marital_Status), o] = TRUE
}

# delete the original marital status col
neg_cards$Marital_Status = NULL

# make transactions object out of that
neg_cards = as(neg_cards, "transactions")

# add the complements to the corresponding columns
neg_cards = addComplement(neg_cards, options)

# make apriori object out of that
neg_cards = apriori(neg_cards, parameter = list(support = 0.001, confidence = 0.8, minlen=2, maxlen=4))

# ready to inspect!
inspect(head(subset(neg_cards, subset = (lhs %in% "!Divorced"))))


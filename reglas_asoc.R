#### MOVIES ####
library(tidyverse)
library(arules)
library (arulesViz)


movies =  read.csv("movies-awards.csv")
head(movies)
movies$Winner = replace(movies$Winner, is.na(movies$Winner), 0)
movies['Year'] = NULL
movies['Ceremony'] = NULL
head(movies)
movies$Award = as.factor(movies$Award)
movies$Winner = as.factor(movies$Winner)
movies$Cast = as.factor(movies$Cast)
movies$Film = as.factor(movies$Film)

groupings <- movies %>% group_by(Award, Winner) %>% group_split()
head(groupings)

apply(movies, 2, function(x) any(is.na(x)))

movie_trans = as(bind_rows(groupings), "transactions")

inspect(head(movie_trans, n=10))

aMovie = apriori(movie_trans, parameter = list(support = 0.01, target="frequent"))
aMovie = sort(aMovie, by="support")
inspect(head(aMovie, n=10))
barplot(table(size(aMovie)), xlab="itemset size", ylab="count")
inspect(aMovie[size(aMovie)==1])



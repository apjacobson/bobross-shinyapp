library(arules)
library(arulesViz)

bob <- read.csv("bob-ross.csv")
bob2 <- bob[c(3:69)]
bob3 <-apply(bob2,2,as.logical)
rules <- apriori(bob3)
rules_conf <- sort (rules, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(rules_conf)


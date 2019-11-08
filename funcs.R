title_word <- function(w, nwords = 1){
  if (w == "sunset"){w <- "sun"}
  bob <- read.csv("bob-ross.csv")
  w <- toupper(w)
  
  bob <- bob[bob[[w]] == 1, c("TITLE", w)]
  
  stopWords <- toupper(stopwords())
  vec <- removeWords(gsub("\"", "", paste(bob[bob[[w]] == 1,]$TITLE, collapse = ' ')), stopWords)
  vec <- gsub("---", " ", gsub(" & ", "", trimws(gsub("  ", " ", gsub("   ", " ", vec)))))
  words <- strsplit(vec, " ")[[1]]
  sample(words, nwords)
}

run_apriori <- function(word) {
  bob <- read.csv("bob-ross.csv")[c(3:69)]
  bob2 <-apply(bob,2,as.logical)
  bob3 <- as(bob2, "transactions")
  rules <- apriori(data=bob3,parameter=list(minlen=2), appearance=list(default="rhs",lhs=word), control=list(verbose=F))
  rules_conf <- sort(rules, by="confidence", decreasing=TRUE) 
  x <- DATAFRAME(rules_conf)$RHS
  y <- unique(x)
  answers <- rep(NA, length(y))
  ind =1
  for (val in y) {
    answers[ind]=str_remove_all(val, "[{}]")
    ind = ind +1
  }
  answers
}

bob <- read.csv("bob-ross.csv")[c(3:69)]
bob2 <-apply(bob,2,as.logical)
rules <- apriori(data=bob2,parameter=list(minlen=2), appearance=list(default="rhs",lhs="TREES"), control=list(verbose=F))

title_word <- function(w){
  if (w == "sunset"){w <- "sun"}
  bob <- read.csv("bob-ross.csv")
  w <- toupper(w)
  
  bob <- bob[bob[[w]] == 1, c("TITLE", w)]
  
  stopWords <- toupper(stopwords())
  vec <- removeWords(gsub("\"", "", paste(bob[bob[[w]] == 1,]$TITLE, collapse = ' ')), stopWords)
  vec <- gsub("---", " ", gsub(" & ", "", trimws(gsub("  ", " ", gsub("   ", " ", vec)))))
  words <- strsplit(vec, " ")[[1]]
  acqTag <- tagPOS(tolower(words))
  words_tagged <- strsplit(acqTag$POStagged, " ")[[1]]
  
  nouns <- c()
  adjs <- c()
  for (word in words_tagged) {
    if (str_sub(word, -3, -1) == "/NN") {
      nouns <- c(nouns, word)
    }
    if (str_sub(word, -3, -1) == "/JJ") {
      adjs <- c(adjs, word)
    } 
  }
  
  toupper(paste(str_sub(sample(adjs, 1), 1, -4), str_sub(sample(nouns, 1), 1, -4)))
}

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- NLP::annotate(s, word_token_annotator, a2)
  a3 <- NLP::annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
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

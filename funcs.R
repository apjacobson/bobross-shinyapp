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

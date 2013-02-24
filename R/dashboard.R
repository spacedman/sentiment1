dash <- function(){
}

filter=function(sentence){
  w=try(tolower(sentence))
  if(inherits(w,"try-error")){
    return("")
  }
  w = gsub("^rt ","",w,perl=TRUE)
  w = gsub("amp;","",w)
  w = gsub("@\\w*"," ",w,perl=TRUE)
  w = gsub("http:[^\\s]*"," ",w,perl=TRUE)
  w = gsub('[[:punct:]]', '', w)
  w = gsub('[[:cntrl:]]', '', w)
  w = gsub('\\d+', '', w)
  w = gsub("\\bnhs\\b"," ",w, perl=TRUE)
  w = gsub("\\bvia\\b"," ",w, perl=TRUE)
  w
}

tweetcloud <- function(tweets,...){
  words = laply(tweets,function(x){
    filter(x$text)
  })
  wordcloud(words,...)
}

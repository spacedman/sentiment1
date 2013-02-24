
setScoreTweets <- function(tweetlist,scoreFunction){
  for(i in 1:length(tweetlist)){
    tweetlist[[i]]$score = scoreFunction(tweetlist[[i]]$text)
  }
  invisible(0)
}

fScoreLM = function(scoretable,scorecolumn){
  force(scoretable)
  force(scorecolumn)
  f = function(sentence){
    scoreSentence(sentence,scoretable,scorecolumn)
  }
  f
}

scoreSentence <- function(sentence,scoretable,scorecolumn){
  require(stringr)
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl:]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = try(tolower(sentence))
  if(inherits(sentence,"try-error")){
    return(NA)
  }
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  scoreWords(words,scoretable,scorecolumn)
}

scoreWords <- function(words,scoretable,tablecolumn){
  scores = scoretable[words,tablecolumn]
  scorecount = sum(!is.na(scores))
  if(scorecount==0){
    return(NA)
  }
  mean(scores,na.rm=TRUE)
}

readLM <- function(lmfile){
  read.csv(lmfile,sep="\t",skip=2,head=TRUE,row.names=1)
}

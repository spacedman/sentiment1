getDump <- function(name,dir){
  file=file.path(dir,paste0(name,".RData"))
  e = new.env()
  load(file,env=e)
  tweets=get("tweets",env=e)
  return(tweets)
}

# hu.liu.pos = scan('data/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
# hu.liu.neg = scan('data/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

scoreTweets <- function(tweetL, pos.words, neg.words){
  texts = laply(tweetL,function(t){t$getText()})
  scores = score.sentiment(texts,pos.words,neg.words)
  return(scores$score)
}

scoreFromDumps <- function(names,dir,pos.words,neg.words){
  res=list()

  ldply(names,function(name){
    res=data.frame(name=name,mean=0,n=0)
    try({
      tweets=getDump(name,dir)
      scores = scoreTweets(tweets,pos.words,neg.words)
      res=data.frame(name=name,mean=mean(scores),n=length(scores))
    })
    return(res)
  })
        
  
}


labScores <- function(tweets,stable,scolumn){
  require(plyr)
  laply(tweets,function(t){
    scoreSentence(t$text,stable,scolumn)
  })
}

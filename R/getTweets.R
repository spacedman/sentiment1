dumpTweets <- function(names,dir){
  require(twitteR)
  for(name in names){
    cat("Getting ",name,"\n")
    try(getSaveTweets(name,dir))
  }

}

getTweets <- function(name){
  tweets = searchTwitter(name,n=1500)
  cat("read ",length(tweets)," tweets\n")
  return(tweets)
}

getSaveTweets <- function(name,dir){
  tweets=getTweets(name)
  dump = file.path(dir,paste0(name,".RData"))
  save(tweets,file=dump)
}

getDailies <- function(search,y,m,d,n,scoreFunction){
  m = sprintf("%02d",m)
  d = sprintf("%02d",d)
  dates = expand.grid(y=y,m=m,d=d)
  dates = paste(dates$y,dates$m,dates$d,sep="-")
  tweets=list()
  for(i in 1:(length(dates)-1)){
    since = dates[i]
    until = dates[i+1]
    tweets[[since]] = searchTwitter(search,since=since,until=until,n=n)
    setScoreTweets(tweets[[since]],scoreFunction)
  }
  tweets
}

basics = function(x){
      n = sum(!is.na(x))
      m = mean(x,na.rm=TRUE)
      s = sd(x,na.rm=TRUE)
      c(n=n,mean=m,sd=s)
    }

dailyF <- function(dailies,F=basics,...){
  ldply(dailies,function(x){F(unlist(lapply(x,function(x){x$score})),...)})
}

list2df <- function(d){
  ldply(unlist(d),function(x){data.frame(when=as.Date(x$created),score=x$score,text=x$text)})
}

mykmeans <- function(df,k){
  count <- 30
  max <- 0
  for(j in 1:count){
    e <- list(0,0,0,0)
    obj <- kmeans(df,k)
    vec <- obj$cluster
    
    for(i in 1:k)
      e[[i]] <- which(vec %in% i)
    
    #cost <- J(df,e,k)
    cost <- obj$betweenss
    
    if(cost > max){
      max <- cost
      w <- e
      kobj <- obj
    }
  }
  #print(e)
  #cat("----------------------------------------------------------------------\n")
  ret <- list(0,0)
  ret[[1]] <- w
  ret[[2]] <- kobj
  ret
}

findlist <- function(x,list,k){
  val <- 0
  for(i in 1:k)
    if(x %in% list[[i]]){
      val <- i
      break
    }
  val    
}

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
library(tm)
myCorpus <- read.csv("G:/R/finalexp/big/kmeans/00/csv/63.csv")
sentiWords <- read.csv("G:/R/csv/sentiWords/afinn.csv")

out <- data.frame(timestamp = integer(), person = integer(), sentence = character())

timestamps <- length(levels(as.factor(myCorpus$timestamp)))
people <- length(levels(as.factor(myCorpus$person)))

x <- split(myCorpus,myCorpus$timestamp)

implode <- function(...,sep = ' '){
  paste(..., collapse = sep)
}

for(i in 1:length(x)){
  y <- split(x[[i]],x[[i]][["person"]])
  for(j in 1:length(y))
    out <- rbind(out, data.frame(timestamp = i, person = j, sentence = implode(y[[j]][["sentence"]]), 
                                 mentions = sum(y[[j]][["mentions"]]), retweets = sum(y[[j]][["retweets"]]), 
                                 morning = sum(y[[j]][["morning"]]), afternoon = sum(y[[j]][["afternoon"]]), 
                                 night = sum(y[[j]][["night"]]), pronoun = sum(y[[j]][["pronoun"]]), 
                                 anger = sum(y[[j]][["anger"]]), ptsd = sum(y[[j]][["ptsd"]]),
                                 urlCount = sum(y[[j]][["urlCount"]]), hashtag = sum(y[[j]][["hashtag"]]),
                                 friends = sum(y[[j]][["friends"]]), followers = sum(y[[j]][["followers"]]),
                                 question = sum(y[[j]][["question"]]), avg = dim(y[[j]])[1] ))
}

write.csv(out,file = "G:/R/finalexp/big/kmeans/00/csv/person.csv")

corpus <- Corpus(VectorSource(out$sentence))
a <- DocumentTermMatrix(corpus)
a <- as.data.frame(as.matrix(a))
DF <- as.data.frame(a,stringAsFactors = FALSE)


ncount <- 0
for(i in names(DF)){
  if(grepl("^[A-Za-z]+$",i)){
    p <- paste("^",i,"$",sep = "")
    n <- grep(p,sentiWords$word)
    
    if(length(n) != 0){
      DF[[i]] <- DF[[i]]*sentiWords[n,2]
      ncount <- ncount + 1
    }
    else{
      DF[[i]] <- NULL
    }
  }
  else{
    DF[[i]] <- NULL
  }
}

tempdim <- dim(DF)[2]
#Additional features
DF$MENTIONS <- out$mentions
DF$RETWEETS <- out$retweets
DF$MORNING <- out$morning
DF$AFTERNOON <- out$afternoon
DF$NIGHT <- out$night
DF$PRONOUN <- out$pronoun
DF$ANGER <- out$anger
DF$PTSD <- out$ptsd
DF$QUESTION <- out$question
DF$URLCOUNT <- out$urlCount
DF$HASHTAG <- out$hashtag
#DF$FRIENDS <- out$friends
#DF$FOLLOWERS <- out$followers
DF$AVG <- out$df

for(x in names(DF)){
  if(max(DF[[x]]) == min(DF[[x]]))
    DF[[x]] <- NULL
  else
    DF[[x]] <- (DF[[x]] - min(DF[[x]]))/(max(DF[[x]]) - min(DF[[x]]))
}

times <- sort(rep(1:timestamps,people))
DF$timestamp <- times
write.csv(DF, file = "G:/R/finalexp/big/kmeans/00/csv/tdm.csv", row.names = FALSE)

DF <- read.csv("G:/R/finalexp/big/kmeans/00/csv/tdm.csv") 

datatimes <- split(DF, DF$timestamp)

for(i in 1:length(datatimes)){
  datatimes[[i]]$timestamp <- NULL
  row.names(datatimes[[i]]) <- 1:people
}

difftimes <- list(0)
for(i in 2:length(datatimes))
  difftimes[[i-1]] <- datatimes[[i]] - datatimes[[i-1]]




k <- 2
clusters <- list(0)
clusvec <- list(0)

for(i in 1:length(difftimes)){
    retlist <- mykmeans(difftimes[[i]],k)
    clusters[[i]] <- retlist[[1]]
    clusvec[[i]] <- retlist[[2]]$cluster
}

m <- matrix(0, nrow = length(difftimes), ncol = people)
for(z in 1:(length(clusters) - 1)){
  center1 <- data.frame()
  
  for(i in 1:k){
    temp <- as.vector(clusters[[z]][[i]])
    tempc <- apply(difftimes[[z]][temp,], 2, sum)
    tempc <- tempc/length(temp)
    center1 <- rbind(center1, tempc)
  }
  
  for(i in 1:k){
    temp <- as.vector(clusters[[z+1]][[i]])
    tempc <- apply(difftimes[[z+1]][temp,], 2, sum)
    tempc <- tempc/length(temp)
    center1 <- rbind(center1, tempc)
  }
  d <- dist(center1)
  
  min <- 9999
  i1 <- -1
  j1 <- -1
  d <- as.matrix(d)
  for(i in 1:dim(d)[1])
    for(j in 1:dim(d)[2])
      if(d[i,j] < min && d[i,j] != 0){
        min = d[i,j]
        i1 <- i
        j1 <- j
      }
  
  if(i1 > k)
    i1 <- i1 - k
  
  if(j1 > k)
    j1 <- j1 - k
  
  t <- table(clusters[[z]][[i1]][clusters[[z]][[i1]] %in% clusters[[z+1]][[j1]]])
  a <- setdiff(clusters[[z]][[i1]], as.vector(as.numeric(names(t))))
  b <- setdiff(clusters[[z+1]][[j1]], as.vector(as.numeric(names(t))))
  
  #   c <- c(a,b)
  #   c <- sort(c)
  
  vec <- rep(1, people)
  vec[as.vector(as.numeric(names(t)))] <- 0
  m[z,] <- vec
}
write.csv(m,"G:/R/finalexp/big/kmeans/00/finalresults.csv")

result <- m
X <- 1:dim(result)[1]
for(z in 1:people){
  Y <- result[,z]
  
  myFile <- paste(z,".png",sep = "")
  path <- paste("G:/R/finalexp/big/kmeans/00/plots/behavior/",myFile,sep = "")
  
  png(filename = path)
  plot(X,Y,type = "l",xlab = "timestamp",ylab = "behavior", ylim = c(0, 1))
  dev.off()
}

X <- 1:(length(difftimes) - 1)
for(z in 1:people){
  L <- c()
  for(i in 1:(length(difftimes)-1)){
    vec <- difftimes[[i]][z,]
    len <- sqrt(sum(vec^2))
    L <- append(L,len)
  }
  
  
  myFile <- paste(z,".png",sep = "")
  path <- paste("G:/R/finalexp/big/kmeans/00/plots/distance/",myFile,sep = "")
  
  png(filename = path)
  plot(X,L,type = "b",xlab = "timestamp",ylab = "distance", pch = 18)
  dev.off()
}


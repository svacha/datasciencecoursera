add2<-function(x,y){
  x+y
}

above10<-function(x){
  use <- x > 10
  x[use]
}

above <- function(x,y){
  use <- x > y
  x[use]
}

columnmean <- function(y, removeNA=TRUE) {
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc) {
    means[i] <- mean(y[, i], na.rm=removeNA)
  }
  means
}
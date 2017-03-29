externalm <- function(x,gold){
  a <- 0
  b <- 0
  c <- 0
  d <- 0
  for(i in 1:length(x)){
    j <- i + 1
    while(j <= length(x)){
      f1 <- TRUE
      f2 <- TRUE
      if(x[i]!=x[j]){
        f1 <- FALSE
      }
      if(gold[i,]!=gold[j,]){
        f2 <- FALSE
      }
      if(f1&f2){
        a <- a + 1
      }
      else if(!f1 & f2){
        b <- b + 1
      }
      else if(f1 & !f2){
        c <- c + 1
      }
      else{
        d <- d + 1
      }
      j <- j + 1
    }
  } 
  rand_index <- (a+d)/(a+b+c+d)
  precision <- a/(a+b)
  recall <- a/(a+c)
  f_score <- 2*precision*recall/(precision + recall)
  return (c(rand_index,precision,recall,f_score))
}


internalm <- function(data, x){
  centers <- x$centers[x$cluster, ]
  distances <- sqrt(rowSums((data - centers)^2))
  diameter <- vector(mode="numeric", length=length(x$size))
  for(i in 1:length(x$cluster)){
    diameter[x$cluster[i]] <- diameter[x$cluster[i]] + distances[i]   
  }
  for(i in 1:length(diameter)){
    diameter[i] <- 2*diameter[i]/x$size[i]
  }
  inter_dis <- matrix(0.0, nrow=length(x$size), ncol=length(x$size))
  for(i in 1:length(x$size)){
    j <- i
    while(j<=length(x$size)){
      inter_dis[i,j] <- sqrt(sum((x$centers[i, ] - x$centers[j, ])^2))
      inter_dis[j,i] <- inter_dis[i,j]
      j <- j+1
    }
  }
  dunn_index <- min(inter_dis[inter_dis>0])/max(diameter)
  db_index <- 0
  for(i in 1:length(x$size)){
    max_val <- 0
    for(j in 1:length(x$size)){
      if(i!=j & inter_dis[i,j]!=0){
        val <- (diameter[i] + diameter[j])/(inter_dis[i,j])
        if(val > max_val){
          max_val <- val
        }
      }
    }
    db_index <- db_index + max_val
  }
  db_index <- db_index / length(x$size)
  return (c(dunn_index,db_index))
}
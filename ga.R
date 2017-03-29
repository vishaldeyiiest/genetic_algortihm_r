source("crossover.R")
source("measures.R")

chrinp <- function(p,chr){
  if(sum(chr) == 0){
    return (TRUE)
  }
  if(length(p) == 0){
    return (FALSE)
  }
  flag = FALSE
  #print(p)
  for(i in 1:length(p)){
    flag = all(p[[i]] == chr)
    if(flag){
      break
    }
  }
  return (flag)
}

addinp <- function(p,fit,chr){
  fitval <- fitness(mydata,chr)
  # print(fitval)
  min_pos <- match(min(fit),fit)
  if(fitval > fit[min_pos]){
    p[[min_pos]] <- list(chr)
    fit[min_pos] <- fitval
  }
  # print(cat("adding",fit))
  return (fit)
}

initp <- function(n,l){
    s <-  list()
    # print(n)
    while(l > 0){
      rand_chr <- c(round(runif(n)))
      # print(rand_chr)
      if(!chrinp(s,rand_chr)){
        s[length(s)+1] <- list(rand_chr)
        l <- l - 1
        # print(l)
      }
    }
    return (s)
}

fitness <- function(data, chr){
    tmpdata <- data[chr==1]
    if(ncol(tmpdata)){
      tmpdata = cbind(tmpdata, rep(0,len=nrow(tmpdata)))
    }
    BIC = mclustBIC(tmpdata)
    mod1 = Mclust(tmpdata, x = BIC)
    clustermod <- kmeans(tmpdata, 3)
    clustermod$cluster <- mod1$classification
    x <- table(clustermod$cluster)
    clustermod$size <- c()
    for(i in x){
      clustermod$size <- c(clustermod$size, i)
    }
    clustermod$centers <- c()
    for(i in 1:length(clustermod$size)){
      clustermod$centers <- rbind(clustermod$centers, colSums(tmpdata[clustermod$cluster==i,])/clustermod$size[i])
    }
    umod <- internalm(tmpdata,clustermod)
    # print(umod)
    return (umod[2])
}


mydata <- iris[1:4]
trials <- 10
psize <- 5

s <- initp(ncol(mydata),psize)
fit <- rep(0,len=psize)
for (i in 1:length(s)){
  fit[i] <- fitness(mydata,s[[i]])
}
for (i in 1:trials){
  p <- runif(1)
  # paste("probability p = ",p)
  if(p < .5){
    c <- ceiling(runif(1)*psize)
    mut_chr <- mutation(s[[c]])
    # paste("Mutate ",mut_chr)
    if(!chrinp(s,mut_chr)){
      fit <- addinp(s,fit,mut_chr)
    }
  }else{
    c <- rep(0, len = 2)
    while(c[1] == c[2]) {
      c <- ceiling(runif(2)*psize)
    }
    # paste("crossover ",s[[c[1]]],s[[c[2]]])
    cross_chr <- crossover(s[[c[1]]],s[[c[2]]])
    for(j in 1:length(cross_chr)){
      if(!chrinp(s,cross_chr[[j]])){
        fit <- addinp(s,fit,cross_chr[[j]])
      }
    }
  }
  #print(fit)
}


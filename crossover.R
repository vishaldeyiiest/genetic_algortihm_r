crossover <- function(a, b){
  #takes a single chromosome and crossovers and returns both chromosomes
  len = length(a)
  l = len/2
  c = rep(0, len = len) # new chromosome
  d = c                 # new chromosome
  c[1:l] = a[1:l]
  c[(l+1):len] = b[(l+1):len]
  d[1:l] = b[1:l]
  d[(l+1):len] = a[(l+1):len]
  return(list(c, d))  
}

mutation <- function(a){
  # mutates a chromosome and complements a random feature if prob > 0.5
  rand = runif(1, 0, 1) # calculates a random probability
  rindex = sample(1:length(a), 1) # finds a random feature
  if(rand > 0.0){
    a[rindex] = as.numeric(!a[rindex])
  }
  return(a)
}
# 
# fitness <- function(S, a){
#   indices = which[a == 1]
#   data = S[, indices]
#   i = index.DB(data, )
# }
# a =  sample(c(0,1), replace=TRUE, size=6)
# b = sample(c(0,1), replace=TRUE, size=6)
# 
# print(crossover(a, b))
# print(mutation(a))

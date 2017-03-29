source("measures.R")
library('dbscan')
library('ggplot2')
library('mclust')
library('cluster')
library('FastKNN')

mydata <- read.table("seeds_dataset.csv",header = FALSE, sep = ",",stringsAsFactors=FALSE)
label <- mydata[8]
mydata <- mydata[1:7]

#data <- read.table("Diamond.csv",header = TRUE, sep = ",",stringsAsFactors=FALSE)
#mydata <- read.table("iris.data",header = FALSE, sep = ",",stringsAsFactors=FALSE)
#label <- mydata[5]
#mydata <- mydata[1:4]
#label <- mydata[8]??
#mydata <- mydata[1:7]
#mydata <- cbind(mydata[2],mydata[3])

wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="K",ylab="sum of withiness")

ggplot(mydata, mapping = aes(V1,V3))+ geom_point()
clusters <- kmeans(mydata, 3)

#ggplot(cbind(mydata[2],mydata[4]), mapping = aes("wfood", "age", color = as.factor(clusters$cluster)))+ geom_point()
cluster <- as.factor(clusters$cluster)
ggplot(mydata, mapping = aes(V1,V3, color = cluster))+ geom_point()
vk <- externalm(clusters$cluster, label)
uk <- internalm(mydata,clusters)
#
#
kNNdistplot(mydata, k=3)
clustersdbs <-  dbscan(mydata, .5, 4)
clusterdbs <- as.factor(clustersdbs$cluster)
ggplot(mydata, mapping = aes(V1,V3, color = clusterdbs))+ geom_point()
clustersdbs$size <- c()
x <- table(clustersdbs$cluster)
if(dimnames(x)[[1]][1] == '0'){
  x <- x[-1]
}
for(i in x){
  clustersdbs$size <- c(clustersdbs$size, i)
}
clustersdbs$centers <- c()
for(i in 1:length(clustersdbs$size)){
  clustersdbs$centers <- rbind(clustersdbs$centers, colSums(mydata[clustersdbs$cluster==i,])/clustersdbs$size[i])
}
vdb <- externalm(clustersdbs$cluster,label)
udb <- internalm(mydata,clustersdbs)
#udb <- internalm??(mydata,??clustersdbs)
#
# #
# #
d <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward")
plot(fit)
groups <- cutree(fit, k=5)
clustersha <- clustersdbs
rect.hclust(fit, k=5, border="red")
clustersha$cluster <- groups
x <- table(clustersha$cluster)
clustersha$size <- c()
for(i in x){
  clustersha$size <- c(clustersha$size, i)
}
clustersha$centers <- c()
for(i in 1:length(clustersha$size)){
  clustersha$centers <- rbind(clustersha$centers, colSums(mydata[clustersha$cluster==i,])/clustersha$size[i])
}
vh <- externalm(clustersha$cluster,label)
uh <- internalm(mydata,clustersha)

#
#
fit <- diana(mydata)
plot(fit)
groups <- cutree(fit, k=5)
rect.hclust(fit, k=5, border="red")
clustersdiana <- clustersdbs
clustersdiana$cluster <- groups
x <- table(clustersdiana$cluster)
clustersdiana$size <- c()
for(i in x){
  clustersdiana$size <- c(clustersdiana$size, i)
}
clustersdiana$centers <- c()
for(i in 1:length(clustersdiana$size)){
  clustersdiana$centers <- rbind(clustersdiana$centers, colSums(mydata[clustersdiana$cluster==i,])/clustersdiana$size[i])
}
vd <- externalm(clustersdiana$cluster,label)
ud <- internalm(mydata,clustersdiana)


BIC = mclustBIC(mydata)
mod1 = Mclust(mydata, x = BIC)
clustermod <- clusterdbs
clustermod$cluster <- mod1$classification
x <- table(clustersdbs$cluster)
for(i in x){
  clustermod$size <- c(clustermod$size, i)
}
clustermod$centers <- c()
for(i in 1:length(clustermod$size)){
  clustermod$centers <- rbind(clustermod$centers, colSums(mydata[clustermod$cluster==i,])/clustermod$size[i])
}
vmod <- externalm(clustermod$cluster,label)
umod <- internalm(mydata,clustermod)
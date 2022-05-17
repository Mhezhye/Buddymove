setwd("C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 3")
Holiday <- read.csv("buddymove_holidayiq.csv", header=TRUE, sep=",")

names(Holiday)
head(Holiday)
tail(Holiday)
summary(Holiday)
str(Holiday)

nrow(Holiday)
ncol(Holiday)
dim(Holiday)

install.packages("cluster")
library(cluster)

Holiday$User.Id<-as.factor(Holiday$User.Id)

pairs(Holiday)

plot(Religious~Shopping, data=Holiday)
with(Holiday,text(Religious~Shopping,labels=User.Id, pos= 4, cex=.6))

User.Id<-Holiday[,1]
Holiday1<-Holiday[,2:7]
Holiday1<-as.data.frame(lapply(Holiday1,normalise))

normalise <-function(df)
{
  return(((df-min(df))/(max(df)-min(df))*(1-0))+0)
}

Holiday1$User.Id<-User.Id
Holiday1<-Holiday1[,c(7,1,2,3,4,5,6)]

Holidaydistance <-dist(Holiday1, method = "euclidean",)
print(Holidaydistance, digits=3)


install.packages("factoextra")
library(factoextra)

fviz_dist(Holidaydistance)

head(Holiday1)
rownames(Holiday1)<-Holiday1$User.Id
Holiday1$User.Id<-NULL

head(Holiday1)

Holidaydistance <-dist(Holiday1, method = "euclidean")
fviz_dist(Holidaydistance)

Holiday.hclust <- hclust(Holidaydistance)
Holiday.hclust

plot(Holiday.hclust)

plot(Holiday.hclust, hang=-1)

rect.hclust(Holiday.hclust,3)

Holidayclust.average <- hclust(Holidaydistance, method= "average")
plot(Holidayclust.average, labels= Holiday$User.Id)
rect.hclust(Holidayclust.average, 4)

Holidayclust.single <- hclust(Holidaydistance, method= "single")
plot(Holidayclust.single, labels= Holiday$User.Id)
rect.hclust(Holidayclust.single, 4)

Holidayclust.centroid <- hclust(Holidaydistance, method= "centroid")
plot(Holidayclust.centroid, labels= Holiday$User.Id)
rect.hclust(Holidayclust.centroid, 4)

Holidayclust.complete <- hclust(Holidaydistance, method= "complete")
plot(Holidayclust.complete, labels= Holiday$User.Id)
rect.hclust(Holidayclust.complete, 4)

Dendrogram.centroid<-cutree(Holidayclust.centroid, 4)
Dendrogram.centroid
Dendrogram.complete<-cutree(Holidayclust.complete, 4)
Dendrogram.complete
table(Dendrogram.centroid, Dendrogram.complete)

kmeanscluster <-kmeans(Holiday[,-1],3)
kmeanscluster
  
clusplot(Holiday,kmeanscluster$cluster, color= TRUE, shade=TRUE, lines=0)


Holiday.tendency<- get_clust_tendency(Holiday1,30,graph=TRUE)
Holiday.tendency$hopkins_stat

fviz_nbclust(Holiday1,kmeans, method= "wss")

set.seed(123)
Holiday.fit<-kmeans(Holiday1, 3, nstart=30)
Holiday.fit$cluster
Holiday.fit$size

fviz_cluster(Holiday.fit,Holiday1)

fviz_cluster(Holiday.fit,Holiday1, ellipse.type = "norm")


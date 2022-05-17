#Import dataset
setwd("C:/Users/Mhezhye/Desktop/UoS/ASDM/COURSEWORK/Task 3")
Holiday <- read.csv("buddymove_holidayiq.csv", header=TRUE, sep=",")

#Inspect data
names(Holiday)
head(Holiday)
tail(Holiday)
summary(Holiday)
str(Holiday)

#check dimensions and points in buddymove dataset
nrow(Holiday)
ncol(Holiday)
dim(Holiday)

#install cluster package for cluster analysis
install.packages("cluster")
library(cluster)

#convert User.Id to a factor variable
Holiday$User.Id<-as.factor(Holiday$User.Id)

#create scatterplot matrix to compare variables
pairs(Holiday)

#plot to understand relationship between religious tourism and shopping
plot(Religious~Shopping, data=Holiday)

#expand to see user ids
with(Holiday,text(Religious~Shopping,labels=User.Id, pos= 4, cex=.6))

#remove id column before normalizing data
User.Id<-Holiday[,1]
Holiday1<-Holiday[,2:7]
Holiday1<-as.data.frame(lapply(Holiday1,normalise))

normalise <-function(df)
{
  return(((df-min(df))/(max(df)-min(df))*(1-0))+0)
}

#add back id column after normalisation
Holiday1$User.Id<-User.Id

#reaarange the columns so id is first
Holiday1<-Holiday1[,c(7,1,2,3,4,5,6)]

#calculate the euclidean distance
Holidaydistance <-dist(Holiday1, method = "euclidean",)

#round figures to 3 decimal places
print(Holidaydistance, digits=3)

install.packages("factoextra")
library(factoextra)

#visualize the distance matrices with fviz_dist() function from factoextra package
fviz_dist(Holidaydistance)

#inspect
head(Holiday1)
#set userid as row names
rownames(Holiday1)<-Holiday1$User.Id
#remove userid from dataset
Holiday1$User.Id<-NULL

#inspect
head(Holiday1)

#calculate euclidea distance
Holidaydistance <-dist(Holiday1, method = "euclidean")
fviz_dist(Holidaydistance)

#use intercluster distance matrix and create hierarchical clustering using hclust() fuction
Holiday.hclust <- hclust(Holidaydistance)
Holiday.hclust

#plot the results
plot(Holiday.hclust)

plot(Holiday.hclust, hang=-1)

#draw 3 clusters
rect.hclust(Holiday.hclust,3)

#cluster using average, single, centroid and complete linkage
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

#compare cluster membership
Dendrogram.centroid<-cutree(Holidayclust.centroid, 4)
Dendrogram.centroid
Dendrogram.complete<-cutree(Holidayclust.complete, 4)
Dendrogram.complete
table(Dendrogram.centroid, Dendrogram.complete)

#create kmeans clustering
kmeanscluster <-kmeans(Holiday[,-1],3)
kmeanscluster
  
#plot with clustplot
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


library(factoextra)
library(cluster)

#########################################
#LOAD DATA
#########################################

#load and prep data
df <- USArrests
df <- na.omit(df)
df <- scale(df)

#########################################
#DETERMINE HOW MANY CLUSTERS IS OPTIMAL
#########################################

#plot number of clusters vs. total within sum of squares
fviz_nbclust(df, kmeans, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

##########################################
#PERFORM K-MEANS CLUSTERING WITH OPTIMAL K
##########################################

#make this example reproducible
set.seed(1)

#perform k-means clustering with k = 4 clusters
km <- kmeans(df, centers = 4, nstart = 25)

#view results
km

#plot results of final k-means model
fviz_cluster(km, data = df)

#find mean of each cluster
aggregate(USArrests, by=list(cluster=km$cluster), mean)

#add cluster assigment to original data
final_data <- cbind(USArrests, cluster = km$cluster)

#view final data
head(final_data)
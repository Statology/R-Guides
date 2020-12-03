library(factoextra)
library(cluster)

#load data
df <- USArrests

#remove rows with missing values
df <- na.omit(df)

#scale each variable to have a mean of 0 and sd of 1
df <- scale(df)

#plot clusters vs within sum of squares
fviz_nbclust(df, pam, method = "wss")

#calculate gap statistic based on number of clusters
gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations

#plot number of clusters vs. gap statistic
fviz_gap_stat(gap_stat)

#make this example reproducible
set.seed(1)

#perform k-medoids clustering with k = 4 clusters
kmed <- pam(df, k = 4)

#view results
kmed

#plot results of final k-medoids model
fviz_cluster(kmed, data = df)

#add cluster assignment to original data
final_data <- cbind(USArrests, cluster = kmed$cluster)

#view final data
head(final_data)
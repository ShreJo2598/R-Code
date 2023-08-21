
# Clear the workspace
rm(list=ls())
cat("\014")
library(ggplot2)
library(cluster)
# set working directory
setwd(" ")  

###### 1. Load & Normalize the Data ###### 
# load in the data file into data.frame
df <- read.csv(file='AirlineLoyalty(1).csv', stringsAsFactors = FALSE)
# check whether there are any missing data
summary(df)
# normalize the data for all columns, except "ID" 
df.norm <- df[, -c(1)]  
df.norm <- data.frame(sapply(df.norm, scale))
summary(df.norm)

###### 2. Hierarchical Clustering ###### 
# 2.1 calculate the distance matrix & apply hclust()
euclidian_dist <- dist(df.norm, method = "euclidean")
hcluster.out <- hclust(euclidian_dist, method="ward.D")

# 2.2 cut the dendrogram into 2 clusters & append the cluster label to df
df$cluster_hc <-as.character(cutree(hcluster.out, k = 2))
df$cluster_hc

###### 3. Kmeans Clustering ###### 
# 2.1 Choosing k using the "elbow plot" 
library(factoextra)
library(cluster)    

set.seed(123)
fviz_nbclust(df.norm, kmeans, nstart = 10, k.max = 10, method = "silhouette")


###### 4. Compare Clustering Results and Label Clusters ###### 
# 4.1 Set k=2, build the cluster using kmeans
km.out <- kmeans(df.norm, centers = 2, nstart = 10) 
# append the cluster label to df
df$cluster_km <- as.character(km.out$cluster)

# the number of observations in each cluster #
table(df$cluster_km)
table(df$cluster_hc)

# summary statistics of original features by cluster
df$cluster_hc <- as.factor(df$cluster_hc)
df$cluster_km <- as.factor(df$cluster_km)

# cluster_km #
agg_km <- aggregate(. ~ cluster_km, data=df[, -c(1, 13)], FUN = mean)
t(agg_km)

# cluster_hc #
agg_hc <- aggregate(. ~ cluster_hc, data=df[, -c(1, 14)], FUN = mean)
t(agg_hc)

# Interpretation: You can have various ways to define profile for customers belonging to each cluster. 
# Taking df$cluster_km for instance:

# cluster (Frequent Travelers): This cluster consists 27% (1093 out of 3999) of customers.  
# They have much larger number of miles counted as qualifying for Top Flight Status ("Qual_miles") and higher number of miles eligible for award travel ("Balance").
# They use the frequent flyer credit card more often (much higher "cc1_miles")
# They are "older" customers ("Days_since_enroll") 
# They have covered significantly higher flight miles ("Flight_miles_12mo") and made significantly more non-flight & flight transactions ("Flight_trans_12", "Bonus_trans"). 
# ...

# cluster (Not-so-frequent Travelers): This cluster consists 73% (2906 out of 3999) of customers.
# ...

rm(list=ls(all=T))
# Load necessary libraries
library(cluster)
library(factoextra)
library(fpc)
library(readxl)

########## Q.2 (Yearly data) 
d1=read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\yearly_data 1.xlsx",sheet = 4)
class(d1)
d1=data.frame(d1)
rownames(d1)=d1[,1]
d1=d1[,-1]
View(d1)

# Scale the data
df <- scale(d1)

# Compute the dissimilarity matrix
diss <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(diss, method = "average" )
plot(hc1, cex = 0.6, hang = -1)
# Cut tree into 4 groups
sub_grp <- cutree(hc1,k=4)

# Create a color palette
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = length(unique(sub_grp)))

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, col = my_palette[sub_grp],hang=-1)

# Add rectangle around groups
rect.hclust(hc1, k = 4, border = 2:5)


# Silhouette method
# Compute and plot average silhouette for k = 2 to k = 15
k.max <- 8
avg_sil <- numeric(k.max-1)

for (k in 2:k.max) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  clust_stats <- cluster.stats(diss, km.res$cluster)
  avg_sil[k-1] <- clust_stats$avg.silwidth
}

plot(2:k.max, avg_sil,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Width")




# Perform K-means clustering
kmeans_result <- kmeans(df, centers = 4)

# Print cluster centers
print(kmeans_result$centers)

# Print count of data points in each cluster
table(kmeans_result$cluster)

# Plot the clusters
fviz_cluster(kmeans_result, data = df)




############################## Q.2 (Quartely data)
#################################### 1st Quarter
rm(list=ls(all=T))
# Load necessary libraries
library(cluster)
library(factoextra)
library(fpc)
library(readxl)
library(topsis)
d1=read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 2)
class(d1)
d1=data.frame(d1)
rownames(d1)=d1[,1]
d1=d1[,-1]

View(d1)
# Scale the data
df <- scale(d1)

# Compute the dissimilarity matrix
diss <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(diss, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
# Cut tree into 4 groups
sub_grp <- cutree(hc1,k=4)

# Create a color palette
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = length(unique(sub_grp)))

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, col = my_palette[sub_grp],hang=-1)

# Add rectangle around groups
rect.hclust(hc1, k = 3, border = 2:5)


# Silhouette method
# Compute and plot average silhouette for k = 2 to k = 15
k.max <- 8
avg_sil <- numeric(k.max-1)

for (k in 2:k.max) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  clust_stats <- cluster.stats(diss, km.res$cluster)
  avg_sil[k-1] <- clust_stats$avg.silwidth
}

plot(2:k.max, avg_sil,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Width")




# Perform K-means clustering
kmeans_result <- kmeans(df, centers = 3)

# Print cluster centers
print(kmeans_result$centers)

# Print count of data points in each cluster
table(kmeans_result$cluster)

# Plot the clusters
fviz_cluster(kmeans_result, data = df)


#################################### 2nd Quarter
rm(list=ls(all=T))
# Load necessary libraries
library(cluster)
library(factoextra)
library(fpc)
library(readxl)
d1=read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 3)
class(d1)
d1=data.frame(d1)
rownames(d1)=d1[,1]
d1=d1[,-1]


# Scale the data
df <- scale(d1)

# Compute the dissimilarity matrix
diss <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(diss, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
# Cut tree into 4 groups
sub_grp <- cutree(hc1,k=4)

# Create a color palette
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = length(unique(sub_grp)))

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, col = my_palette[sub_grp],hang=-1)

# Add rectangle around groups
rect.hclust(hc1, k = 4, border = 2:5)


set.seed(2)
# Perform K-means clustering
kmeans_result <- kmeans(df, centers = 4)

# Print cluster centers
print(kmeans_result$centers)

# Print count of data points in each cluster
table(kmeans_result$cluster)

# Plot the clusters
fviz_cluster(kmeans_result, data = df)


#################################### 3rd Quarter
rm(list=ls(all=T))
# Load necessary libraries
library(cluster)
library(factoextra)
library(fpc)
library(readxl)
d1=read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 4)
class(d1)
d1=data.frame(d1)
rownames(d1)=d1[,1]
d1=d1[,-1]


# Scale the data
df <- scale(d1)

# Compute the dissimilarity matrix
diss <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(diss, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
# Cut tree into 4 groups
sub_grp <- cutree(hc1,k=4)

# Create a color palette
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = length(unique(sub_grp)))

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, col = my_palette[sub_grp],hang=-1)

# Add rectangle around groups
rect.hclust(hc1, k = 4, border = 2:5)





# Perform K-means clustering
kmeans_result <- kmeans(df, centers = 4)

# Print cluster centers
print(kmeans_result$centers)

# Print count of data points in each cluster
table(kmeans_result$cluster)

# Plot the clusters
fviz_cluster(kmeans_result, data = df)



######################################## 4th Quarter

rm(list=ls(all=T))
# Load necessary libraries
library(cluster)
library(factoextra)
library(fpc)
library(readxl)
d1=read_xlsx("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 5)
class(d1)
d1=data.frame(d1)
rownames(d1)=d1[,1]
d1=d1[,-1]


# Scale the data
df <- scale(d1)

# Compute the dissimilarity matrix
diss <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(diss, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)
# Cut tree into 4 groups
sub_grp <- cutree(hc1,k=4)

# Create a color palette
my_palette <- colorRampPalette(c("red", "green", "blue"))(n = length(unique(sub_grp)))

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, col = my_palette[sub_grp],hang=-1)

# Add rectangle around groups
rect.hclust(hc1, k = 3, border = 2:5)


# Silhouette method
# Compute and plot average silhouette for k = 2 to k = 15
k.max <- 8
avg_sil <- numeric(k.max-1)

for (k in 2:k.max) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  clust_stats <- cluster.stats(diss, km.res$cluster)
  avg_sil[k-1] <- clust_stats$avg.silwidth
}

plot(2:k.max, avg_sil,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouette Width")



set.seed(12345)
# Perform K-means clustering
kmeans_result <- kmeans(df, centers = 4)

# Print cluster centers
print(kmeans_result$centers)

# Print count of data points in each cluster
table(kmeans_result$cluster)

# Plot the clusters
fviz_cluster(kmeans_result, data = df)





################################### Q.3
################################ 1st Quarter
####### Ranking by using Topsis Method 
wt=rep(1,ncol(df))
ind=c("+","+","+","+","-","-","+","+","-","-","+","-","+","+","+","+","-","-","-","-","+","-")
T1=topsis(df,wt,ind)
rownames(T1)=rownames(d1)
sort(data.frame(T1))
View(T1)

########################### 2nd quarter
####### Ranking by using Topsis Method 
wt=rep(1,ncol(df))
ind=c("+","+","+","+","-","-","+","+","-","-","+","-","+","+","+","+","-","-","-","-","+","-")
T1=topsis(df,wt,ind)
rownames(T1)=rownames(d1)
View(T1)

############################# 3rd quarter
####### Ranking by using Topsis Method 
wt=rep(1,ncol(df))
ind=c("+","+","+","+","-","-","+","+","-","-","+","-","+","+","+","+","-","-","-","-","+","-")
T1=topsis(df,wt,ind)
rownames(T1)=rownames(d1)
View(T1)


############################# 4th quater
####### Ranking by using Topsis Method 
wt=rep(1,ncol(df))
ind=c("+","+","+","+","-","-","+","+","-","-","+","-","+","+","+","+","-","-","-","-","+","-")
T1=topsis(df,wt,ind)
rownames(T1)=rownames(d1)
View(T1)



######################################## Question 4
###################### 1st quarter
######## Outlier detction 
rm(list=ls(all=T))
sd=read_excel("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 2)
sd=data.frame(sd)
rownames(sd)=sd[,1]
sd=sd[,-1]
outlier=matrix(,ncol=2,nrow=0)
for(i in 1:22)
{
  b=boxplot(sd[,i],plot=F)
  if(length(b$out)>0)
  { 
    t=which(sd[,i] %in% b$out)
    variable=names(sd[t,])[i]
    obs=rownames(sd[t,])
    outlier=rbind(outlier,cbind(rep(variable,length(obs)),obs))
  }
  
  
  
}
outlier

######################## 2nd quarter
######## Outlier detction 
rm(list=ls(all=T))
sd=read_excel("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 3)
sd=data.frame(sd)
rownames(sd)=sd[,1]
sd=sd[,-1]
outlier=matrix(,ncol=2,nrow=0)
for(i in 1:22)
{
  b=boxplot(sd[,i],plot=F)
  if(length(b$out)>0)
  { 
    t=which(sd[,i] %in% b$out)
    variable=names(sd[t,])[i]
    obs=rownames(sd[t,])
    outlier=rbind(outlier,cbind(rep(variable,length(obs)),obs))
  }
  
  
  
}
outlier


########################### 3rd quarter
######## Outlier detction 
rm(list=ls(all=T))
sd=read_excel("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 4)
sd=data.frame(sd)
rownames(sd)=sd[,1]
sd=sd[,-1]
outlier=matrix(,ncol=2,nrow=0)
for(i in 1:22)
{
  b=boxplot(sd[,i],plot=F)
  if(length(b$out)>0)
  { 
    t=which(sd[,i] %in% b$out)
    variable=names(sd[t,])[i]
    obs=rownames(sd[t,])
    outlier=rbind(outlier,cbind(rep(variable,length(obs)),obs))
  }
  
  
  
}
outlier


######################## 4th quarter
######## Outlier detction 
rm(list=ls(all=T))
sd=read_excel("C:\\Users\\Shiv\\Documents\\data mining\\ASS_2\\Quartely_data.xlsx",sheet = 5)
sd=data.frame(sd)
rownames(sd)=sd[,1]
sd=sd[,-1]
outlier=matrix(,ncol=2,nrow=0)
for(i in 1:22)
{
  b=boxplot(sd[,i],plot=F)
  if(length(b$out)>0)
  { 
    t=which(sd[,i] %in% b$out)
    variable=names(sd[t,])[i]
    obs=rownames(sd[t,])
    outlier=rbind(outlier,cbind(rep(variable,length(obs)),obs))
  }
  
  
  
}
outlier







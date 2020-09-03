library(tidyverse)
library(dplyr)
library(ggplot2)
#Prepare data
yr_data <-
  read.csv("responses.csv",sep=",") %>%
  filter(complete.cases(.)) %>%
  # mutate(id=row_number()) %>%
  tbl_df()

#Prepare PCA data
yr_pca<-
  yr_data[,sapply(yr_data,class)=="integer"] %>%
  select(Music:Spending.on.healthy.eating)

#Run PCA analysis
yr_pca_result<-princomp(yr_pca,cor=T)

#See the PCA output
ggplot(data=data.frame(PC=1:length(yr_pca_result$sdev),var_exp=cumsum(yr_pca_result$sdev^2/sum(yr_pca_result$sdev^2))),
       aes(x=PC,y=var_exp)) + geom_line() + geom_point() + scale_y_continuous(labels = scales::percent,breaks=seq(0,1,length.out=11)) + scale_x_continuous(breaks=seq(0,135,by=5))


#Get the Young People Survey data
yr_mds_data <- yr_pca %>% select(Dance:Opera)
print(head(yr_mds_data))



#Correlation is a similarity measure between -1 and 1
#Get the negative of it as a distance measure and add 1 to make sure distances start from 0
yr_dist <- 1 - cor(yr_mds_data)
#Apply MDS
yr_mds <- cmdscale(yr_dist,k=2)
#Provide column names
colnames(yr_mds) <- c("x","y")
print(yr_mds)


#Plot
ggplot(data.frame(yr_mds),aes(x=x,y=y)) + geom_text(label=rownames(yr_mds),angle=45,size=2)

## K-Means

#Set the seed because K-Means algorithm uses a search based method
set.seed(58)
#Apply K-Means
genre_cluster<-kmeans(yr_mds,centers=4)
##Get the clusters
mds_clusters<-data.frame(genre=names(genre_cluster$cluster),cluster_mds=genre_cluster$cluster) %>% arrange(cluster_mds,genre)
mds_clusters

#Plot the output
ggplot(data.frame(yr_mds) %>% mutate(clusters=as.factor(genre_cluster$cluster),genres=rownames(yr_mds)),aes(x=x,y=y)) + geom_text(aes(label=genres,color=clusters),angle=45,size=2) + geom_point(data=as.data.frame(genre_cluster$centers),aes(x=x,y=y)
)

#Set the seed because K-Means algorithm uses a search based method
set.seed(58)
#Apply K-Means to the raw data.
genre_cluster_raw<-kmeans(t(yr_mds_data),centers=4)

#Build the comparison data
compare_kmeans<-
  left_join(mds_clusters,
            data.frame(genre=names(genre_cluster_raw$cluster),cluster_raw=genre_cluster_raw$cluster),by="genre")

print(compare_kmeans)


#Set the seed because K-Means algorithm uses a search based method
set.seed(58)
#Prepare data (get mean scores) and apply K-Means
yr_means_data<-yr_mds_data %>% summarise_each(funs(mean)) %>% t()


yr_means_data


means_kmeans<-kmeans(yr_means_data,centers=4)
#Add to compare kmeans
compare_kmeans<-
  left_join(compare_kmeans,
            data.frame(genre=names(means_kmeans$cluster),cluster_mean=means_kmeans$cluster),by="genre")
print(compare_kmeans)



## Hierarchical Clustering

### Single link (MIN) method
yr_hc<-hclust(as.dist(yr_dist),method="single")
plot(yr_hc,hang=-1)

### Complete link (MAX) method
yr_hc<-hclust(as.dist(yr_dist),method="complete")
plot(yr_hc,hang=-1)

### Average method
yr_hc<-hclust(as.dist(yr_dist),method="average")
plot(yr_hc,hang=-1)

### Centroid method
yr_hc<-hclust(as.dist(yr_dist),method="centroid")
plot(yr_hc,hang=-1)

### Ward method
yr_hc<-hclust(as.dist(yr_dist),method="ward.D2")
plot(yr_hc,hang=-1)

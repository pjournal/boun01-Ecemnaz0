library(tidyverse)
library(dplyr)
library(ggplot2)
#Set the seed for reproducibility
set.seed(58)
#Create a coordinate matrix (all coordinates are between 0 and 1).
#Suppose the places of cities A to J
coord_mat<-matrix(round(runif(20),6),ncol=2)
#Column names of the coordinate matrix, say x and y
colnames(coord_mat)<-c("x","y")
#Row names of the coordinates are cities.
#LETTERS is a predefined vector with letters of the English alphabet.
rownames(coord_mat)<-LETTERS[1:10]
#Create distance matrix
dist_mat<-dist(coord_mat)
#Display coordinate matrix
print(coord_mat)

ggplot(as.data.frame(coord_mat),aes(x=x,y=y)) + geom_text(label=rownames(coord_mat))

print(dist_mat)


#Now let's employ Multidimensional Scaling (MDS)
#Base R has a lovely command called cmdscale (Classical multidimensional scaling)
mds_data<-cmdscale(dist_mat,k=2)
colnames(mds_data)<-c("x","y")
#Print the output
print(mds_data)
print(coord_mat)


#Let's plot the output
ggplot(as.data.frame(mds_data),aes(x=x,y=y)) + geom_text(label=rownames(mds_data)) + labs(title="MDS Output")

#Not quite similar? Let's manipulate a bit.
ggplot(as.data.frame(mds_data) %>% mutate(y=-y),aes(x=y,y=x)) + geom_text(label=rownames(mds_data)) + labs(title="MDS Output Transposed and Inverted")


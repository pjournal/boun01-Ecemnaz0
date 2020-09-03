## Supervised Learning (Regression and Classification)

library(ggplot2)
library(tidyr)
library(tidyverse)
library(dplyr)

# Linear Regression

#First check the data
head(swiss)
#Run a simple linear regression
ggplot(data=swiss) + 
  geom_point(aes(x=Examination,y=Fertility)) +
  geom_smooth(method='lm',aes(x=Examination,y=Fertility),se=TRUE)


fert_vs_exam<-lm(Fertility ~ Examination, data=swiss)
summary(fert_vs_exam)


#Now Fertility vs all
fert_vs_all<-lm(Fertility ~ ., data=swiss)
summary(fert_vs_all)

# K Nearest Neighbors (KNN)

##We are going to use knn function of the class package
## If the class package is not loaded use install.packages("class")
##Below example is from iris data set
## train is the covariates of the training set
train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
## cl is the class of the training set
## First 25 nodes are (s)etosa, second 25 are versi(c)olor, third 25 are (v)irginica
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
## test is the test set we want to predict its classes
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
##The following function uses the train set and classes to predict
#the class of the test set's classes
knn_cl<-class::knn(train, test, cl, k = 3, prob=TRUE)
knn_cl


##Let's also compare knn estimates vs actual test data classes (same order as train data)
table(data.frame(actual=cl,estimate=knn_cl))


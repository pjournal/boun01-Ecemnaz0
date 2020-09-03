#Customer Survey Data
survey_data <- data.frame(
  customer = 1:16,
  OS = c(0,0,0,0,1,0,0,0,1,1,0,1,1,1,1,1),
  Price = c(6,7,6,5,7,6,5,6,3,1,2,5,2,3,1,2),
  Software = c(5,3,4,7,7,4,7,5,5,3,6,7,4,5,6,3),
  Aesthetics = c(3,2,4,1,5,2,2,4,6,7,6,7,5,6,5,7),
  Brand = c(4,2,5,3,5,3,1,4,7,5,7,6,6,5,5,7)
)

#Let's do some exploratory analysis
summary(survey_data[,3:6])

#Correlation Matrix
cor(survey_data[,3:6])

#Do PCA with princomp function and use correlation matrix to create components
survey_pca <- princomp(as.matrix(survey_data[,3:6]),cor=T)
summary(survey_pca,loadings=TRUE)

library(ggplot2)

ggplot(data.frame(pc=1:4,cum_var=c(0.6075727,0.8478733,0.9640409,1.00000000)),aes(x=pc,y=cum_var)) + geom_point() + geom_line()

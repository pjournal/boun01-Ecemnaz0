#Made-up Example regarding Transformations
set.seed(58)
#Randomly create data points around the normal distribution
x1=rnorm(30,mean=2,sd=4)
#Get one linear transformation and one nonlinear transformation of the data
pca_data<-data.frame(x1,x2=x1*2,x3=(x1^2),x4=abs(x1)^(0.5)+rnorm(30))
#See the correlation matrix
pca_cor<-cor(pca_data)
pca_cor

#See the eigenvalues and eigenvectors
pca_eigen<-eigen(pca_cor)
pca_eigen

pca_eigen$values/sum(pca_eigen$values)

cumsum(pca_eigen$values/sum(pca_eigen$values))

#Run PCA
pca_result<-princomp(pca_data,cor=T)

#See the PCA
summary(pca_result,loadings=TRUE)

#Remove x2 See the PCA
pca_result2<-princomp(pca_data[,c("x1","x3","x4")],cor=T)
summary(pca_result2,loadings=TRUE)
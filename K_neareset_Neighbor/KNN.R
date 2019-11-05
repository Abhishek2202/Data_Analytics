setwd("/Users/abhishekgupta/Desktop/")
data1<-read.csv(file.choose(), header=T)
attach(data1)
colnames(data1) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' )
str(data1)
Rings <- as.numeric(Rings)
Rings <- cut(Rings, br=c(-1,8,11,35), labels = c("young", "adult", "old"))
Rings <- as.factor(Rings)
abalone <- data1
abalone$sex <- NULL
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
  }
abalone[1:7] <- as.data.frame(lapply(abalone[1:7], normalize))
ind <- sample(2, nrow(abalone), replace=TRUE, prob=c(0.7, 0.3))
KNNtrain <- abalone[ind==1,]
KNNtest <- abalone[ind==2,]
sqrt(2924)
library(class)
KNNpred <- knn(train = KNNtrain[1:7], test = KNNtest[1:7], cl = KNNtrain$rings, k = 54)
KNNpred
table(KNNpred)


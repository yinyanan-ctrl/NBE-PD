rm(list=ls())
dev.off()

#install.packages("randomForest")
library(randomForest)
library(pROC)

train_final<-read.csv("data_train-1.csv",header=T,sep=",")
#data1<-read.csv("data_test-1.csv",header=T,sep=",")
train_final$LN <-as.factor(train_final$LN )
#data1$LN<-as.factor(data1$LN )

set.seed(100)
mtry_result <- NULL
n<-length(names(train_final))
for(i in 1:(n-1)){
  mtry_fit<-randomForest(LN ~ .,data=train_final,mtry=i)
  err<-mean(mtry_fit$err.rate)
  mtry_result<-rbind(mtry_result,err)
}
mtry_result <- data.frame(mtry_result)
write.table(mtry_result,"mtry_result.csv",sep=",")

LN_randomforest <- randomForest(LN ~ .,data=train_final,ntree=1000, mtry=45, important=TRUE,proximity=TRUE)
LN_importance<-LN_randomforest$importance
write.csv(LN_importance,"LN_importance.csv",row.names = T)
plot(LN_randomforest)
varImpPlot(LN_randomforest)

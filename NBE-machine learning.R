rm(list=ls())
dev.off()
options(stringsAsFactors = FALSE) 
library(caret)
library(pROC)
library(survey)
train_df<-read.csv("all subjects.csv",header =  T,row.names = 1,check.names = F)
train_df$Group<-as.factor(train_df$Group)
nrow<-nrow(train_df)
rowname<-row.names(train_df)
set.seed(1234)
train<-sample(rowname,floor(nrow*0.7))
train_data<-train_df[train,]
test<-setdiff(rowname,train)
test_data<-train_df[test,]
train_df
logit_model <- glm(Group~.,data=train_df, family = binomial) 
#logit_step <- step(logit_model, direction = 'backward') 
#summary(logit_step)
fitControl<-trainControl(method="repeatedcv",
                         number = 10,
                         repeats = 10,
                         classProbs = TRUE,
                         savePredictions = TRUE )
set.seed(1111)
glm.model.cv <- train(Group ~  BLM , 
                      data= train_data,
                      method = "glm",
                      family='binomial',
                      metric = "ROC",
                      trControl = fitControl
)
varImp(glm.model.cv)
summary(glm.model.cv)
logit_model_step <- glm(Group ~ BLM ,
                        data=train_data, family = binomial(link = "logit")) 
glm.probs <- predict(glm.model.cv,train_data)
table(train_data$Group,glm.probs)
confusionMatrix(train_data$Group,glm.probs)
#t <- predict(glm.model.cv,train_data,type = "prob")
#write.table(t,"train_data.csv",sep=",")
#predict(logit_step,train_df)
pred_train <- data.frame(Prob = round(predict(logit_model_step, newdata = train_data,type="response"),4), GoldStandard = train_data$Group, stringsAsFactors = F)
blue <- "#0093FF"
peach  <- "#E43889"
roc.train <- plot.roc(pred_train[,2], pred_train[,1], ylim=c(0,1),xlim=c(1,0),
                      smooth=F, 
                      ci=TRUE, 
                      legacy.axes=T,print.auc=T)
legend.paste <- c(paste0("Train dataset AUC: ",round(roc.train$auc,3), " (",paste0(round(roc.train$ci[1],3),"-",round(roc.train$ci[3],3)),")") )
plot(1-roc.train$specificities, roc.train$sensitivities, 
     col=blue, xlab="1-Specificity (FPR)", main="", ylab="Sensitivity (TPR)",
     lwd=2, type="l",  xlim=c(0,1),ylim=c(0,1))
lines(x=c(0,1),y=c(0,1),lwd=1.5,lty=2,col="grey40") 
legend("bottomright", bty="n", 
       fill=c(blue,peach,"NA"), 
       legend.paste,
       cex=.8, border=NA, y.intersp=1, x.intersp=0.2 )
glm.probs <- predict(glm.model.cv,test_data)
#table(test_data$Group,glm.probs)#????????
confusionMatrix(test_data$Group,glm.probs)
pred_test <- data.frame(Prob = round(predict(logit_model_step, newdata = test_data,type="response"),4), GoldStandard = test_data$Group, stringsAsFactors = F)
blue <- "#0093FF"
peach  <- "#E43889"
roc.test <- plot.roc(pred_test[,2], pred_test[,1], ylim=c(0,1),xlim=c(1,0),
                     smooth=F, 
                     ci=TRUE, 
                     legacy.axes=T,print.auc=T)
legend.paste <- c(paste0("test dataset AUC: ",round(roc.test$auc,3), " (",paste0(round(roc.test$ci[1],3),"-",round(roc.test$ci[3],3)),")") )
plot(1-roc.test$specificities, roc.test$sensitivities, 
     col=blue, xlab="1-Specificity (FPR)", main="", ylab="Sensitivity (TPR)",
     lwd=2, type="l",  xlim=c(0,1),ylim=c(0,1))
lines(x=c(0,1),y=c(0,1),lwd=1.5,lty=2,col="grey40") 
legend("bottomright", bty="n", 
       fill=c(blue,peach,"NA"), 
       legend.paste,
       cex=.8, border=NA, y.intersp=1, x.intersp=0.2 )

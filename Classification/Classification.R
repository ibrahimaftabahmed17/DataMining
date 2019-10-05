library(ROCR)
library(tidyverse)
library(MASS)
library(ggplot2)
library(GGally)
library(gridExtra)
library(tidyr)
library(dplyr)
require(RColorBrewer)
set.seed(12698282)
n<-500
x1<-runif(n,max=1,min=0)
x2<-rep(times=n,x=0)
for (i in 1:n){
  if(i%%2==1){
    x2[i]<-1
  }
}
p<-1/(1+(exp(-(-1 + 5.2 *x1 - 0.4*x2))))
y<-rbinom(n,1,p)
datasetp<-cbind.data.frame(y,x1,x2)
model.glm.logit<-glm(y~., family=binomial, data=datasetp)
summary(model.glm.logit)

predtest.logit <- prediction(predict(model.glm.logit,type="response"), y)
perftest.logit <- performance(predtest.logit, "tpr", "fpr")
plot(perftest.logit, colorize=TRUE)
unlist(slot(performance(predtest.logit, "auc"), "y.values"))

library(PRROC)
predict.logit<-predict(model.glm.logit,type="response")
score1.logit<- predict.logit[y==1]
score0.logit<- predict.logit[y==0]
roc.logit<- roc.curve(score1.logit, score0.logit, curve = T)
roc.logit$auc
pr.logit<-pr.curve(score1.logit, score0.logit, curve = T)
pr.logit
plot(pr.logit)



p.seq.logit <- seq(0.01, 1, 0.01)

costfuncnoweights<-function(obs, pred.p, pcut){
  weight1<- 1
  weight0<- 1
  mean(((obs==0)&(pred.p>pcut))*weight0|((obs==1)&(pred.p<pcut))*weight1)
}
cost.logit <- rep(0, length(p.seq.logit))  
for(i in 1:length(p.seq.logit)){ 
  cost.logit[i] <- costfuncnoweights(obs = y, pred.p = predict(model.glm.logit, type="response"), pcut = p.seq.logit[i])  
}

plot(p.seq.logit,cost.logit)

p.seq.logit[cost.logit==min(cost.logit)]

predictedoutput.logit<-as.numeric(predict(model.glm.logit,type = "response")>=0.56)
matrixout.logit<-cbind.data.frame(y,predictedoutput.logit)

contingencymatrix.logit<-as.data.frame(matrix(cbind(sum(matrixout.logit$y==1 & matrixout.logit$predictedoutput==1),
                                                    sum(matrixout.logit$y==1 & matrixout.logit$predictedoutput==0),
                                                    sum(matrixout.logit$y==0 & matrixout.logit$predictedoutput==1),
                                                    sum(matrixout.logit$y==0 & matrixout.logit$predictedoutput==0)),ncol=2))

names(contingencymatrix.logit)<-c("Predict = 1", "Predict = 0")

rownames(contingencymatrix.logit)<-c("Y = 1","Y = 0")



model.glm.probit<-glm(y~., family=binomial(link = "probit"), data=datasetp)
summary(model.glm.probit)


predtest.probit <- prediction(predict(model.glm.probit,type="response"), y)
perftest.probit <- performance(predtest.probit, "tpr", "fpr")
plot(perftest.probit, colorize=TRUE)
unlist(slot(performance(predtest.probit, "auc"), "y.values"))


predict.probit<-predict(model.glm.probit,type="response")
score1.probit<- predict.probit[y==1]
score0.probit<- predict.probit[y==0]
roc.probit<- roc.curve(score1.probit, score0.probit, curve = T)
roc.probit$auc
pr.probit<-pr.curve(score1.probit, score0.probit, curve = T)
pr.probit
plot(pr.probit)

p.seq.probit <- seq(0.01, 1, 0.01)

cost.probit <- rep(0, length(p.seq.logit))  
for(i in 1:length(p.seq.logit)){ 
  cost.probit[i] <- costfuncnoweights(obs = y, pred.p = predict(model.glm.probit, type="response"), pcut = p.seq.probit[i])  
}

plot(p.seq.probit,cost.probit)

p.seq.probit[cost.probit==min(cost.probit)]

predictedoutput.probit<-as.numeric(predict(model.glm.probit,type="response")>0.56)
matrixout.probit<-cbind.data.frame(y,predictedoutput.probit)

contingencymatrix.probit<-as.data.frame(matrix(cbind(sum(matrixout.probit$y==1 & matrixout.probit$predictedoutput==1),
                                                     sum(matrixout.probit$y==1 & matrixout.probit$predictedoutput==0),
                                                     sum(matrixout.probit$y==0 & matrixout.probit$predictedoutput==1),
                                                     sum(matrixout.probit$y==0 & matrixout.probit$predictedoutput==0)),ncol=2))

names(contingencymatrix.probit)<-c("Predict = 1", "Predict = 0")
rownames(contingencymatrix.probit)<-c("Y = 1","Y = 0")

############################
set.seed(05954655)
n<-500
x1<-runif(n,max=1,min=0)
x2<-rep(times=n,x=0)
for (i in 1:n){
  if(i%%2==1){
    x2[i]<-1
  }
}
p<-pnorm(-1 + 5.2 *x1 - 0.4*x2)
y<-rbinom(n,1,p)
datasetp<-cbind.data.frame(y,x1,x2)
model.glm.logit<-glm(y~., family=binomial, data=datasetp)
summary(model.glm.logit)

costfuncnoweights<-function(obs, pred.p, pcut){
  weight1<- 1
  weight0<- 1
  mean(((obs==0)&(pred.p>pcut))*weight0|((obs==1)&(pred.p<pcut))*weight1)
}

predtest.logit <- prediction(predict(model.glm.logit,type="response"), y)
perftest.logit <- performance(predtest.logit, "tpr", "fpr")
plot(perftest.logit, colorize=TRUE)
unlist(slot(performance(predtest.logit, "auc"), "y.values"))

library(PRROC)
predict.logit<-predict(model.glm.logit,type="response")
score1.logit<- predict.logit[y==1]
score0.logit<- predict.logit[y==0]
roc.logit<- roc.curve(score1.logit, score0.logit, curve = T)
roc.logit$auc
pr.logit<-pr.curve(score1.logit, score0.logit, curve = T)
pr.logit
plot(pr.logit)



p.seq.logit <- seq(0.01, 1, 0.01)

cost.logit <- rep(0, length(p.seq.logit))  
for(i in 1:length(p.seq.logit)){ 
  cost.logit[i] <- costfuncnoweights(obs = y, pred.p = predict(model.glm.logit, type="response"), pcut = p.seq.logit[i])  
}

plot(p.seq.logit,cost.logit)

p.seq.logit[cost.logit==min(cost.logit)]

predictedoutput.logit<-as.numeric(predict(model.glm.logit,type = "response")>=0.53)
matrixout.logit<-cbind.data.frame(y,predictedoutput.logit)

contingencymatrix.logit<-as.data.frame(matrix(cbind(sum(matrixout.logit$y==1 & matrixout.logit$predictedoutput==1),
                                                    sum(matrixout.logit$y==1 & matrixout.logit$predictedoutput==0),
                                                    sum(matrixout.logit$y==0 & matrixout.logit$predictedoutput==1),
                                                    sum(matrixout.logit$y==0 & matrixout.logit$predictedoutput==0)),ncol=2))

names(contingencymatrix.logit)<-c("Predict = 1", "Predict = 0")

rownames(contingencymatrix.logit)<-c("Y = 1","Y = 0")



model.glm.probit<-glm(y~., family=binomial(link = "probit"), data=datasetp)
summary(model.glm.probit)


predtest.probit <- prediction(predict(model.glm.probit,type="response"), y)
perftest.probit <- performance(predtest.probit, "tpr", "fpr")
plot(perftest.probit, colorize=TRUE)
unlist(slot(performance(predtest.probit, "auc"), "y.values"))


predict.probit<-predict(model.glm.probit,type="response")
score1.probit<- predict.probit[y==1]
score0.probit<- predict.probit[y==0]
roc.probit<- roc.curve(score1.probit, score0.probit, curve = T)
roc.probit$auc
pr.probit<-pr.curve(score1.probit, score0.probit, curve = T)
pr.probit
plot(pr.probit)

p.seq.probit <- seq(0.01, 1, 0.01)

costfuncnoweights<-function(obs, pred.p, pcut){
  mean(((obs==0)&(pred.p>pcut))|((obs==1)&(pred.p<pcut)))
}
cost.probit <- rep(0, length(p.seq.logit))  
for(i in 1:length(p.seq.logit)){ 
  cost.probit[i] <- costfuncnoweights(obs = y, pred.p = predict(model.glm.probit, type="response"), pcut = p.seq.probit[i])  
}

plot(p.seq.probit,cost.probit)

p.seq.probit[cost.probit==min(cost.probit)]

predictedoutput.probit<-as.numeric(predict(model.glm.probit,type="response")>0.52)
matrixout.probit<-cbind.data.frame(y,predictedoutput.probit)

contingencymatrix.probit<-as.data.frame(matrix(cbind(sum(matrixout.probit$y==1 & matrixout.probit$predictedoutput==1),
                                                     sum(matrixout.probit$y==1 & matrixout.probit$predictedoutput==0),
                                                     sum(matrixout.probit$y==0 & matrixout.probit$predictedoutput==1),
                                                     sum(matrixout.probit$y==0 & matrixout.probit$predictedoutput==0)),ncol=2))

names(contingencymatrix.probit)<-c("Predict = 1", "Predict = 0")
rownames(contingencymatrix.probit)<-c("Y = 1","Y = 0")







###############################

german_credit <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

colnames(german_credit)<-c("chk_acct","duration","credit_his","purpose","amount","saving_acct","present_emp","installment_rate","sex","other_debtor","present_resid","property","age","other_install","housing","n_credits","job","n_people","telephone","foreign","response")

#orginal response coding 1= good, 2 = bad
#we need 0 = good, 1 = bad
german_credit$response <-german_credit$response - 1


german_credit_numeric<-read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric")
german_credit<-as.tibble(german_credit)

german_credit_n <-german_credit%>%
  lapply(as.numeric)%>%
  as.tibble()


dt.long <- gather(german_credit_n, "variable", "value", 1:21)
dt.long$value<-as.numeric(dt.long$value)

ViolinPlotAll<-ggplot(dt.long,aes(factor(variable), value))+
  geom_violin(aes(fill=factor(variable)))+
  geom_boxplot(alpha=0.3, color="black", width=.1)+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

BoxPlotAll<-ggplot(dt.long,aes(factor(variable), value))+
  geom_boxplot(alpha=0.3, color="black", width=.1)+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

HistAll<-ggplot(dt.long,aes(x=value))+
  geom_histogram(aes(fill=factor(variable),y=..density..),color="black")+
  geom_density(color="black")+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

ScatterFactors<-german_credit %>%
  gather(., "variable", "value", c(1,3,4,6,7,9,10,12,14,15,17,19,20)) %>% 
  ggplot(aes(x = value, y = response)) +
  geom_point(aes(color=as.factor(variable)),alpha=0.1) +
  geom_jitter(aes(color=as.factor(variable)),alpha=0.1,height =0.1)+
  facet_wrap(~ variable, scales = "free",ncol=3) +
  theme_minimal()+
  theme(legend.position = "none")

ScatterNum<-german_credit %>%
  gather(., "variable", "value", c(2,5,8,11,13,16,18))%>% 
  ggplot(aes(x = value, y = response)) +
  geom_point(aes(color=as.factor(variable)),alpha=0.1) +
  geom_jitter(aes(color=as.factor(variable)),alpha=0.1,height =0.1)+
  facet_wrap(~ variable, scales = "free",ncol=3) +
  theme_minimal()+
  theme(legend.position = "none")


PPCor<-ggcorr(german_credit[1:21],method = c("pairwise", "pearson"),low = "blue", mid = "white", high = "red",label = TRUE,label_round = 2)+
  ggtitle("Pairwise Pearson - Correlation HeatMap") + 
  theme_minimal()

set.seed(12698282)
sample_index<-sample(nrow(german_credit),nrow(german_credit)*0.75)
german_credit_train<-german_credit[sample_index,]
german_credit_test<-german_credit[-sample_index,]
german_credit_n_train<-german_credit_n[sample_index,]
german_credit_n_test<-german_credit_n[-sample_index,]

model.german0<-glm(data=german_credit_train,family=binomial,response~.)

model.germannull<-glm(data=german_credit_train,family=binomial,response~1)
model.germanforward<-step(model.germannull, scope=list(lower=model.germannull, upper=model.german0),direction=c("forward"))
model.germanbackward<-step(model.german0)
summary(model.germanbackward)
summary(model.germanforward)

library(glmnet)

model.lasso<-glmnet(x=as.matrix(german_credit_n_train[1:20]),y=as.matrix(german_credit_n_train[21]),family = "binomial")
model.lasso.cv<-cv.glmnet(x=as.matrix(german_credit_n_train[1:20]),y=as.matrix(german_credit_n_train[21]),family = "binomial",type.measure = "class")

plot(model.lasso)
plot(model.lasso.cv)

(model.lasso.cv$lambda.1se)
(model.lasso.cv$lambda.min)

coef(model.lasso,s=(model.lasso.cv$lambda.1se))

model.lasso.f<-glm(response~ chk_acct + duration + credit_his + amount + saving_acct + present_emp + installment_rate + sex + other_debtor + property + age + other_install ,data=german_credit_n_train, family = "binomial")



# tLL <- model.lasso$nulldev - deviance(model.lasso)
# k <- model.lasso$df
# n <- model.lasso$nobs
# AICc <- -tLL+2*k+2*k*(k+1)/(n-k-1)
# sum(AICc)
# 
# BIC<-log(n)*k - tLL
# sum(BIC)



predtest.german <- prediction(predict(model.germanbackward,type="response"), german_credit_train[21])
perftest.german <- performance(predtest.german, "tpr", "fpr")
plot(perftest.german, colorize=TRUE)
unlist(slot(performance(predtest.german, "auc"), "y.values"))


predict.german<-predict(model.germanbackward,type="response")
score1.german<- predict.german[german_credit_train$response==1]
score0.german<- predict.german[german_credit_train$response==0]
roc.german<- roc.curve(score1.german, score0.german, curve = T)
roc.german$auc
pr.german<-pr.curve(score1.german, score0.german, curve = T)
pr.german
plot(pr.german)

costfuncwithweights<-function(obs, pred.p, pcut){
  weight1<- 5
  weight0<- 1
  mean(((obs==0)&(pred.p>pcut))*weight0|((obs==1)&(pred.p<pcut))*weight1)
}

p.seq.german <- seq(0.01, 1, 0.01)

cost.german <- rep(0, length(p.seq.german))  

for(i in 1:length(p.seq.german)){ 
  cost.german[i] <- costfuncwithweights(obs = german_credit_train[21], pred.p = predict(model.germanbackward, type="response"), pcut = p.seq.german[i])  
}

plot(p.seq.german,cost.german)

p.seq.german[cost.german==min(cost.german)]

predictedoutput.german<-as.numeric(predict(model.germanbackward,type = "response")>=0.56)
matrixout.german<-cbind.data.frame(german_credit_train[21],predictedoutput.german)

contingencymatrix.german<-as.data.frame(matrix(cbind(sum(matrixout.german$response==1 & matrixout.german$predictedoutput.german==1),
                                                     sum(matrixout.german$response==1 & matrixout.german$predictedoutput.german==0),
                                                     sum(matrixout.german$response==0 & matrixout.german$predictedoutput.german==1),
                                                     sum(matrixout.german$response==0 & matrixout.german$predictedoutput.german==0)),ncol=2))

names(contingencymatrix.german)<-c("Predict = 1", "Predict = 0")

rownames(contingencymatrix.german)<-c("Y = 1","Y = 0")

outofsample<-predict(model.germanbackward, newdata = german_credit_test[1:20], type="response")
pred.outofsample <- prediction(outofsample, german_credit_test[21])
perf.outofsample <- performance(pred.outofsample, "tpr", "fpr")
plot(perf.outofsample, colorize=TRUE)
predtest.german.out<- prediction(predict(model.germanbackward,type="response",newdata=german_credit_test), german_credit_test[21])
unlist(slot(performance(predtest.german.out, "auc"), "y.values"))



score1.german.out<- outofsample[german_credit_test$response==1]
score0.german.out<- outofsample[german_credit_test$response==0]
pr.german.out<-pr.curve(score1.german.out, score0.german.out, curve = T)
plot(pr.german.out)

predictedoutput.german.out<-as.numeric(predict(model.germanbackward,type = "response",newdata = german_credit_test)>=0.56)
matrixout.german<-cbind.data.frame(german_credit_test[21],predictedoutput.german.out)

contingencymatrix.german<-as.data.frame(matrix(cbind(sum(matrixout.german$response==1 & matrixout.german$predictedoutput.german==1),
                                                     sum(matrixout.german$response==1 & matrixout.german$predictedoutput.german==0),
                                                     sum(matrixout.german$response==0 & matrixout.german$predictedoutput.german==1),
                                                     sum(matrixout.german$response==0 & matrixout.german$predictedoutput.german==0)),ncol=2))

names(contingencymatrix.german)<-c("Predict = 1", "Predict = 0")

rownames(contingencymatrix.german)<-c("Y = 1","Y = 0")

bankruptcydata<-read_csv("bankruptcy.csv")

bankruptcydata$FYEAR<-as.factor(bankruptcydata$FYEAR)
summary(bankruptcydata)

bankruptcydata_scaled<-bankruptcydata
bankruptcydata_scaled[4:13]<-scale(bankruptcydata[4:13])

dt.long.bank <- gather(bankruptcydata_scaled, "variable", "value", c(1,2,4:13))
dt.long.bank$value<-as.numeric(dt.long.bank$value)

ViolinPlotAll.bank<-ggplot(dt.long.bank,aes(factor(variable), value))+
  geom_violin(aes(fill=factor(variable)))+
  geom_boxplot(alpha=0.3, color="black", width=.1)+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

BoxPlotAll.bank<-ggplot(dt.long.bank,aes(factor(variable), value))+
  geom_boxplot(alpha=0.3, color="black", width=.1)+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

HistAll.bank<-ggplot(dt.long.bank,aes(x=value))+
  geom_histogram(aes(fill=factor(variable),y=..density..),color="black")+
  geom_density(color="black")+
  labs(x = "", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  facet_wrap(~variable, scales="free",ncol=3)

grid.arrange(ViolinPlotAll.bank,HistAll.bank,ncol=2)

ScatterFactors.bank<-bankruptcydata_scaled[,1:2]%>%
  ggplot(aes(x = FYEAR, y = DLRSN)) +
  geom_jitter(alpha=0.1,height =0.1)+
  theme_minimal()+
  theme(legend.position = "none")


ScatterNum.bank<-bankruptcydata_scaled%>%
  gather(., "variable", "value", c(4:13))%>%
  ggplot(aes(x = value, y = DLRSN)) +
  geom_jitter(aes(color=as.factor(variable)),alpha=0.1,height =0.1)+
  facet_wrap(~ variable, scales = "free",ncol=3) +
  theme_minimal()+
  theme(legend.position = "none")


PPCor.bank<-ggcorr(bankruptcydata_scaled,method = c("pairwise", "pearson"),low = "blue", mid = "white", high = "red",label = TRUE,label_round = 2)+
  ggtitle("Pairwise Pearson - Correlation HeatMap") + 
  theme_minimal()



set.seed(12698282)
sample_index<-sample(nrow(bankruptcydata_scaled),nrow(bankruptcydata_scaled)*0.75)


bankruptcydata_scaled_train<-bankruptcydata_scaled[sample_index,]
bankruptcydata_scaled_test<-bankruptcydata_scaled[-sample_index,]



model.bankruptcy.0<-glm(data=bankruptcydata_scaled_train[-c(1,3)],family=binomial,DLRSN~.)
summary(model.bankruptcy.0)
BIC(model.bankruptcy.0)

model.bankruptcy.backwards<-step(model.bankruptcy.0)
summary(model.bankruptcy.backwards)
BIC(model.bankruptcy.backwards)

model.bankruptcy.null<-glm(data=bankruptcydata_scaled_train[-c(1,3)],family=binomial,DLRSN~1)

model.bankruptcy.forward<-step(model.bankruptcy.null,scope=list(lower=model.bankruptcy.null, upper=model.bankruptcy.0),direction = "forward")
summary(model.bankruptcy.forward)
BIC(model.bankruptcy.forward)

model.lasso.bank<-glmnet(x=as.matrix(bankruptcydata_scaled_train[-c(1,2,3)]),y=as.matrix(bankruptcydata_scaled_train[2]),family = "binomial")model.lasso.bank.cv<-cv.glmnet(x=as.matrix(bankruptcydata_scaled_train[-c(1,2,3)]),y=as.matrix(bankruptcydata_scaled_train[2],family = "binomial"),type.measure = "class")
model.lasso.bank.cv<-cv.glmnet(x=as.matrix(bankruptcydata_scaled_train[-c(1,2,3)]),y=as.matrix(bankruptcydata_scaled_train[2],family = "binomial"),type.measure = "class")


plot(model.lasso.bank)
plot(model.lasso.bank.cv)

(model.lasso.bank.cv$lambda.1se)
(model.lasso.bank.cv$lambda.min)

coef(model.lasso.bank,s=(model.lasso.bank.cv$lambda.1se))


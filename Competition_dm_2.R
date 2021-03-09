rm(list=ls())
library(tidyverse)
library(glmnet)
library(spaMM)

# modify your working directory with the data file if necessary (using the setwd() command):
# setwd("my-working-directory")

# read data:
load("d1.RData")
load("d2.RData")
load("d3.RData")

data_train_DF=rbind(d1,d2,d3)
rm(d1,d2,d3)
summary(data_train_DF)
#data_train_DF %>% filter(year==1999,month==3) %>% 
#  ggplot(aes(x=lon, y=lat, color=CNT))+geom_point()
# show severity thresholds:
u_ba # for BA
u_cnt # for CNT
# show weights used for the weighted RPS:
weights_ba
weights_cnt

# explore the data:
dim(data_train_DF)
names(data_train_DF)

# data points to predict:
sum(is.na(data_train_DF$BA))
sum(is.na(data_train_DF$CNT))

# histogram of training data on log-scale for better readability:
hist(log(1+data_train_DF$CNT), xlab = "log(1+CNT)", main = "Histogram of log-transformed CNT")
hist(log(1+data_train_DF$BA), xlab = "log(1+BA)", main = "Histogram of log-transformed BA")

#####Time series by sites
head(data_train_DF)

Site<-paste(data_train_DF$lon,data_train_DF$lat)
UnSite<-unique(Site)
length(UnSite)

#Four examples
#10,100,1000,3000
par(mfrow=c(2,2))
examples=c(10,100,1000, 3000)
for (i in 1:4) {
index_s<-examples[i]
CNT_s<-data_train_DF$CNT[Site==UnSite[index_s]]
Year_s<-data_train_DF$year[Site==UnSite[index_s]]
Month_s<-data_train_DF$month[Site==UnSite[index_s]]
Date_s<-paste(Year_s,Month_s, sep="_")
plot(1:length(Date_s),CNT_s, xlab="Time", ylab="Number of fires", pch=19, col="red")
title(paste("Site=",UnSite[index_s], ", from", Date_s[1], "to", Date_s[length(Date_s)] ))
}

# CNT (Poisson regression): ####

# remove rows with data to predict:
train_DF = data_train_DF[!is.na(data_train_DF$CNT),]
test_DF = data_train_DF[is.na(data_train_DF$CNT),]
# remove BA (which contains more NA values):
train_DF = subset(train_DF, select = -BA)
# in test data, remove BA and the NA column of CNT:
test_DF = subset(test_DF, select = -c(CNT, BA))
dim(train_DF)
dim(test_DF)
dim(train_DF) # dimension of training data

####Sample 50% of the data in train_DF for model selection -> train_select_DF 
####Keep 50% for model building -> test_select_DF

set.seed(1)
SelNum<-sample(1:nrow(train_DF), round(0.5*nrow(train_DF)), replace=F)
train_select_DF<-train_DF[-c(SelNum),]
test_select_DF<-train_DF[c(SelNum),]

####Model 1: glm
# train the model:
fit1 = glm(CNT ~ ., data = train_select_DF, family = poisson(link = "log"))
summary(fit1)
save(fit1, file="fit1")

# calculate predictions:
pred_mean_cnt_glm = predict(fit1, test_select_DF, type = "response")

####Model 2: glmnet
Y<-as.matrix(train_select_DF$CNT)
X<-as.matrix(train_select_DF[,-1])
fit_cvglmnet = cv.glmnet(X,Y,family="poisson", nfolds=3)
par(mfrow=c(1,1))
plot(fit_cvglmnet)
save(fit_cvglmnet, file="fit_cvglmnet")
load(file = "fit_cvglmnet")

fit_cvglmnet$lambda.1se
coef(fit_cvglmnet, s="lambda.1se")

# calculate predictions:
pred_mean_cnt_lasso = predict(fit_cvglmnet, newx=as.matrix(test_select_DF[,-1]), s="lambda.min", type = "response")

#####Comparison of the two types of predictions
plot(pred_mean_cnt_lasso, pred_mean_cnt_glm, xlab="LASSO", ylab="GLM")
abline(0,1)
summary(pred_mean_cnt_lasso)
summary(pred_mean_cnt_glm)

#### calculate the matrix with estimated exceedance probability of the severity thresholds:
prediction_cnt = matrix(nrow = nrow(test_select_DF), ncol = length(u_cnt))
indicatrice_cnt=prediction_cnt
pred_mean_cnt=pred_mean_cnt_lasso
#pred_mean_cnt=pred_mean_cnt_lasso

for(k in 1:length(u_cnt)){
  prediction_cnt[,k] = ppois(u_cnt[k], lambda = pred_mean_cnt)
  Ind_k=test_select_DF$CNT
  Ind_k[u_cnt[k]<test_select_DF$CNT]<-1
  Ind_k[u_cnt[k]>=test_select_DF$CNT]<-0
  indicatrice_cnt[,k]=Ind_k
}

####Computation of Scnt

Scnt=sum(((indicatrice_cnt-prediction_cnt)^2%*%weights_cnt))
Scnt




######Stop running here######




# prediction_cnt has to be submitted for the competition

# explore the mean of estimated probabilities for each severity threshold:
apply(prediction_cnt, 2, mean)

#
# BA (log-normal regression): ####
# Procedure: we here fit the log-normal model only to positive values of BA, and then combine this with the count model above for the probability of BA=0.
# remove rows with data to predict:
train_DF = data_train_DF[!is.na(data_train_DF$BA),]
test_DF = data_train_DF[is.na(data_train_DF$BA),]
# remove CNT (which contains more NA values):
train_DF = subset(train_DF, select = -CNT)
# # in test data, remove CNT and the NA column of BA:
test_DF = subset(test_DF, select = -c(CNT, BA))
dim(train_DF)
dim(test_DF)
# use only positive BA values for fitting the log-normal GLM:
train_DF = train_DF[train_DF$BA > 0 ,]
# train the model:
fit = lm(log(BA) ~ ., data = train_DF) 
summary(fit)

# extract the predictive standard deviation:
sd_gauss = sd(fit$residuals)
sd_gauss

# extract predicted Gaussian means of BA>0:
pred_mean_logba = predict(fit, test_DF)
hist(pred_mean_logba)
# calculate estimated exceedance probabilities for the BA component:
prediction_ba = matrix(nrow = 80000, ncol = length(u_ba))
for(k in 1:length(u_ba)){
  # we here use prediction_cnt[,1] for the probability P(BA = 0).
  prediction_ba[,k] = prediction_cnt[,1] + (1-prediction_cnt[,1]) * pnorm(log(u_ba[k]), mean = pred_mean_logba, sd = sd_gauss)
  #prediction_ba[,k] = pnorm(log(u_ba[k]), mean = pred_mean_logba, sd = sd_gauss)
}
# prediction_ba has to be submitted for the competition

# explore the mean of estimated probabilities for each severity threshold:
apply(prediction_ba, 2, mean)

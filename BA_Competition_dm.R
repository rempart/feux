rm(list=ls())
library(tidyverse)
library(glmnet)
library(ranger)
library(lme4)

# modify your working directory with the data file if necessary (using the setwd() command):
# setwd("my-working-directory")

#############
# read data##
#############

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

##########################
#####Time series by sites#
##########################

head(data_train_DF)

Site<-paste(data_train_DF$lon,data_train_DF$lat)
UnSite<-unique(Site)
length(UnSite)

#Four examples of sites
#10,100,1000,3000
par(mfrow=c(2,2))
examples=c(10,100,1000, 3000)
for (i in 1:4) {
index_s<-examples[i]
BA_s<-log(data_train_DF$BA[Site==UnSite[index_s]]+1)
Year_s<-data_train_DF$year[Site==UnSite[index_s]]
Month_s<-data_train_DF$month[Site==UnSite[index_s]]
Date_s<-paste(Year_s,Month_s, sep="_")
plot(1:length(Date_s),BA_s, xlab="Time", ylab="Burnt area (log)", pch=19, col="red")
title(paste("Site=",UnSite[index_s], ", from", Date_s[1], "to", Date_s[length(Date_s)] ))
}

###########################
####Train and Test dataset#
###########################

# remove rows with data to predict:
train_DF = data_train_DF[!is.na(data_train_DF$BA),]
test_DF = data_train_DF[is.na(data_train_DF$BA),]
# remove CNT (which contains more NA values):
train_DF = subset(train_DF, select = -CNT)
# # in test data, remove CNT and the NA column of BA:
test_DF = subset(test_DF, select = -c(CNT, BA))
dim(train_DF)
dim(test_DF)

#Site id
Site<-paste(train_DF$lon,train_DF$lat)
#Month-Year id
Time<-paste(train_DF$month, train_DF$year)
#Site*time
SiteTime<-paste(Site,train_DF$year)

####################################
##### Categories of CNT and weights#
####################################

Cat_BA=train_DF$BA
Weight_BA=train_DF$BA
#Weight_list=c(weights_ba[1], diff(weights_ba))
Weight_list=weights_ba

for(k in 2:length(u_ba)){
  Cat_BA[(train_DF$BA>u_ba[k-1]) & (train_DF$BA<=u_ba[k])] = u_ba[k]
  Weight_BA[(train_DF$BA>u_ba[k-1]) & (train_DF$BA<=u_ba[k])] = Weight_list[k]
}

Cat_BA[train_DF$BA<=0]=u_ba[1]
Weight_BA[train_DF$BA<=0]=Weight_list[1]

Cat_BA[(train_DF$BA>100000)]=200000
Weight_BA[(train_DF$BA>100000)]=0

unique(Cat_BA)
table(Cat_BA)

summary(Weight_BA)

##############################################################################        
####Sample 50% of the data in train_DF for model selection 
####-> train_select_DF: train dataset for continuous predictions
####-> train_select_Cat_BA: categories of BA (categorical RF)
####-> train_select_weight: weights (weighted categorical RF)
####-> train_select_site and train_select_time: for random effect model
####Keep 50% for model building 
####-> test_select_DF: train dataset for continuous predictions
####-> test_select_Cat_BA: categories of BA (categorical RF)
####-> test_select_weight: weights (weighted categorical RF)
####-> test_select_site and test_select_time: for random effect model
##############################################################################

set.seed(1)
SelNum<-sample(1:nrow(train_DF), round(0.5*nrow(train_DF)), replace=F)
train_select_DF<-train_DF[-c(SelNum),]
test_select_DF<-train_DF[c(SelNum),]
#Same selection for the categorical variable
train_select_Cat_BA<-Cat_BA[-c(SelNum)]
test_select_Cat_BA<-Cat_BA[c(SelNum)]
train_select_weight<-Weight_BA[-c(SelNum)]
#Same selection for site ID
train_select_site<-Site[-c(SelNum)]
test_select_site<-Site[c(SelNum)]
#Same selection for time
train_select_time<-Time[-c(SelNum)]
test_select_time<-Time[c(SelNum)]

##################
#####Modelling####
##################

####Model 1: glmer
train_glmer<-cbind(train_select_DF, site=train_select_site, time=train_select_time, year=train_select_year)
fit_glmer_ba = lmer(log(BA+1) ~ 1+(1|time)+(1|site), data = train_glmer)
summary(fit_glmer_ba)
Pred_glmer_train_ba<-predict(fit_glmer_ba, type="response", re.form=NULL)
plot(log(train_glmer$BA+1),Pred_glmer_train_ba)
save(fit_glmer_ba, file="fit_glmer_ba")
load(file="fit_glmer_ba")
# calculate predictions:
test_glmer=cbind(test_select_DF, site=test_select_site, time=test_select_time)
pred_mean_ba_glmer = predict(fit_glmer_ba, newdata=test_glmer, re.form=NULL, type = "response")
plot(pred_mean_ba_glmer,log(test_select_DF$BA+1), xlab="Predictions", ylab="observations")
abline(0,1)

###Model 3: ranger for categories
train_select_DF_cat=train_select_DF
train_select_DF_cat$BA=as.factor(train_select_Cat_BA)
fit_ranger_cat_ba=ranger(BA~., data=train_select_DF_cat, probability = TRUE)
save(fit_ranger_cat_ba, file="fit_ranger_cat_ba")
load(file="fit_ranger_cat_ba")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$BA=as.factor(test_select_Cat_BA)
pred_ba_ranger_cat=predict(fit_ranger_cat_ba, data=test_select_DF_cat)
head(pred_ba_ranger_cat$predictions)
summary(apply(pred_ba_ranger_cat$predictions,1, sum))

###Model 4: ranger for categories weighted
#######Model 2bis: ranger + site and time effects
train_2bis<-cbind(train_select_DF, Pred=Pred_glmer_train_ba)
train_2bis$BA=as.factor(train_select_Cat_BA)
fit_ranger_cat_glmer_ba=ranger(BA~., data=train_2bis, probability = TRUE)
save(fit_ranger_cat_glmer_ba, file="fit_ranger_cat_glmer_ba")
load(file="fit_ranger_cat_glmer_ba")
#Predictions
test_2bis=cbind(test_select_DF, Pred=pred_mean_ba_glmer)
test_2bis$BA=as.factor(test_select_Cat_BA)
pred_ba_ranger_cat_glmer=predict(fit_ranger_cat_glmer_ba, data=test_2bis)
head(pred_ba_ranger_cat_glmer$predictions)
summary(apply(pred_ba_ranger_cat_glmer$predictions,1, sum))


########################
####Model evaluation####
########################


#######Function SBA for categorical outputs

SBA_cat=function(Obs, Pred,u_ba) {
  
  # calculate the matrix with estimated exceedance probability of the severity thresholds:
  indicatrice_ba=Pred
  Proba_cum=t(apply(Pred,1, cumsum))
  prediction_ba=Proba_cum
  
  for(k in 1:length(u_ba)){
    Ind_k=Obs
    Ind_k[u_ba[k]<Obs]<-0
    Ind_k[u_ba[k]>=Obs]<-1
    indicatrice_ba[,k]=Ind_k
  }
  
  #Computation of Sba
  
  Sba=sum(((indicatrice_ba-prediction_ba)^2%*%weights_ba))
  
  return(Sba)}

#List including all model predictions

Names_cat=c("RF cat", "RF cat glmer")
Proba_list_ba=list(pred_ba_ranger_cat$predictions,pred_ba_ranger_cat_glmer$predictions)
save(Proba_list_ba,file="Proba_list_ba")
load("Proba_list_ba")

SBA_all=c()

for (m in 1:2) {
Pred=Proba_list_ba[[m]]
Pred=Pred[,-ncol(Pred)]
Obs=test_select_DF$BA
Resc=SBA_cat(Obs, Pred,u_ba)
SBA_all=c(SBA_all,Resc)
}

SBA_all

ResultTAB=as.data.frame(cbind(Names_cat,round(SBA_all,2)))
names(ResultTAB)=c("Names", "S_ba")
ResultTAB


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

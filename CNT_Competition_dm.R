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
CNT_s<-data_train_DF$CNT[Site==UnSite[index_s]]
Year_s<-data_train_DF$year[Site==UnSite[index_s]]
Month_s<-data_train_DF$month[Site==UnSite[index_s]]
Date_s<-paste(Year_s,Month_s, sep="_")
plot(1:length(Date_s),CNT_s, xlab="Time", ylab="Number of fires", pch=19, col="red")
title(paste("Site=",UnSite[index_s], ", from", Date_s[1], "to", Date_s[length(Date_s)] ))
}

###########################
####Train and Test dataset#
###########################

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
#Site id
Site<-paste(train_DF$lon,train_DF$lat)
#Month-Year id
Time<-paste(train_DF$month, train_DF$year)
#Site*Month
SiteMonth<-paste(Site,train_DF$month)

####################################
##### Categories of CNT and weights#
####################################

Cat_CNT=train_DF$CNT
Weight_CNT=train_DF$CNT
Weight_list=weights_cnt

for(k in 2:length(u_cnt)){
  Cat_CNT[(train_DF$CNT>u_cnt[k-1]) & (train_DF$CNT<=u_cnt[k])] = u_cnt[k]
  Weight_CNT[(train_DF$CNT>u_cnt[k-1]) & (train_DF$CNT<=u_cnt[k])] = Weight_list[k]
}

Cat_CNT[train_DF$CNT<=0]=u_cnt[1]
Weight_CNT[train_DF$CNT<=0]=Weight_list[1]

Cat_CNT[(train_DF$CNT>100)]=200
Weight_CNT[(train_DF$CNT>100)]=0

unique(Cat_CNT)
table(Cat_CNT)

##############################################################################        
####Sample 50% of the data in train_DF for model selection 
####-> train_select_DF: train dataset for continuous predictions
####-> train_select_Cat_CNT: categories of CNT (categorical RF)
####-> train_select_weight: weights (weighted categorical RF)
####-> train_select_site and train_select_time: for random effect model
####Keep 50% for model building 
####-> test_select_DF: train dataset for continuous predictions
####-> test_select_Cat_CNT: categories of CNT (categorical RF)
####-> test_select_weight: weights (weighted categorical RF)
####-> test_select_site and test_select_time: for random effect model
##############################################################################

set.seed(1)
SelNum<-sample(1:nrow(train_DF), round(0.5*nrow(train_DF)), replace=F)
train_select_DF<-train_DF[-c(SelNum),]
test_select_DF<-train_DF[c(SelNum),]
#Same selection for the categorical variable
train_select_Cat_CNT<-Cat_CNT[-c(SelNum)]
test_select_Cat_CNT<-Cat_CNT[c(SelNum)]
train_select_weight<-Weight_CNT[-c(SelNum)]
#Same selection for site ID
train_select_site<-Site[-c(SelNum)]
test_select_site<-Site[c(SelNum)]
#Same selection for time
train_select_time<-Time[-c(SelNum)]
test_select_time<-Time[c(SelNum)]
#Same selection for site*month
train_select_sitemonth<-SiteMonth[-c(SelNum)]
test_select_sitemonth<-SiteMonth[c(SelNum)]
#Same selection for year
train_select_year<-train_DF$year[-c(SelNum)]
test_select_year<-train_DF$year[c(SelNum)]

##################
#####Modelling####
##################

####Model 1: glm
# train the model:
fit1 = glm(CNT ~ ., data = train_select_DF, family = poisson(link = "log"))
summary(fit1)
save(fit1, file="fit1")
load("fit1")
# calculate predictions:
pred_mean_cnt_glm = predict(fit1, test_select_DF, type = "response")
plot(pred_mean_cnt_glm,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

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
plot(pred_mean_cnt_lasso,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

####Model 3: glmer
train_glmer<-data.frame(CNT=train_select_DF$CNT,year=train_select_year, site=train_select_site, time=train_select_time, sitemonth=train_select_sitemonth)
fit_glmer = glmer(CNT ~ 1+(1|site)+(1|sitemonth), data = train_glmer, family = poisson(link = "log"))
summary(fit_glmer)
Pred_glmer_train<-predict(fit_glmer, type="response", re.form=NULL)
plot(train_glmer$CNT,Pred_glmer_train)
save(fit_glmer, file="fit_glmer")
load(file="fit_glmer")
# calculate predictions:
test_glmer=data.frame(sitemonth=test_select_sitemonth, time=test_select_time, site=test_select_site, year=test_select_year)
pred_mean_cnt_glmer = predict(fit_glmer, newdata=test_glmer, re.form=NULL, type = "response")
plot(pred_mean_cnt_glmer,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

####Model 4: ranger
fit_ranger_permut=ranger(CNT~., data=train_select_DF,importance='permutation')
save(fit_ranger_permut, file="fit_ranger_permut")
load(file="fit_ranger_permut")
IMP=importance(fit_ranger_permut)
IMP<-IMP[order(IMP, decreasing=T)]
IMP
barplot(IMP, horiz=T, names.arg=names(IMP),cex.names=0.25)
#Predictions
pred_mean_cnt_ranger=predict(fit_ranger_permut, data=test_select_DF)
plot(pred_mean_cnt_ranger$predictions,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

#######Model 4bis: ranger + site and time effects
train_4bis<-cbind(train_select_DF, Pred=Pred_glmer_train)
fit_ranger_glmer=ranger(CNT~., data=train_4bis)
save(fit_ranger_glmer, file="fit_ranger_glmer")
load(file="fit_ranger_glmer")
#Predictions
test_4bis=cbind(test_select_DF, Pred=pred_mean_cnt_glmer)
pred_mean_cnt_ranger_glmer=predict(fit_ranger_glmer, data=test_4bis)
plot(pred_mean_cnt_ranger_glmer$predictions,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

###Model 5: ranger (10 most influential inputs)
fit_ranger_small=ranger(CNT~lc7+lon+clim5+lat+lc5+altiMean+clim3+clim8+lc11+clim7, data=train_select_DF)  
save(fit_ranger_small, file="fit_ranger_small")
load(file="fit_ranger_small")
#Predictions
pred_mean_cnt_ranger_small=predict(fit_ranger_small, data=test_select_DF)
plot(pred_mean_cnt_ranger_small$predictions,test_select_DF$CNT, xlab="Predictions", ylab="observations")
abline(0,1)

###Model 6: ranger for categories
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=as.factor(train_select_Cat_CNT)
fit_ranger_cat=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, num.trees=1000)
save(fit_ranger_cat, file="fit_ranger_cat")
load(file="fit_ranger_cat")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)
pred_cnt_ranger_cat=predict(fit_ranger_cat, data=test_select_DF_cat)
head(pred_cnt_ranger_cat$predictions)
summary(apply(pred_cnt_ranger_cat$predictions,1, sum))

###Model 7: ranger for categories weighted
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=as.factor(train_select_Cat_CNT)
fit_ranger_cat_w=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, case.weights=train_select_weight)
save(fit_ranger_cat_w, file="fit_ranger_cat_w")
load(file="fit_ranger_cat_w")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)
pred_cnt_ranger_cat_w=predict(fit_ranger_cat_w, data=test_select_DF_cat)
head(pred_cnt_ranger_cat_w$predictions)
summary(apply(pred_cnt_ranger_cat_w$predictions,1, sum))

########################
####Model evaluation####
########################

#######Function SCNT for continuous outputs

SCNT_cont=function(Obs, Pred, Name, u_cnt) {

#plot(Pred, Obs, xlab="Predictions", ylab="Observations")
#abline(0,1)
#title(Name)

RMSE=sqrt(mean((Obs-Pred)^2))

# calculate the matrix with estimated exceedance probability of the severity thresholds:
prediction_cnt = matrix(nrow = length(Pred), ncol = length(u_cnt))
indicatrice_cnt=prediction_cnt
pred_mean_cnt=Pred

for(k in 1:length(u_cnt)){
  prediction_cnt[,k] = ppois(u_cnt[k], lambda = pred_mean_cnt)
  Ind_k=Obs
  Ind_k[u_cnt[k]<Obs]<-0
  Ind_k[u_cnt[k]>=Obs]<-1
  indicatrice_cnt[,k]=Ind_k
}

#Computation of Scnt

Scnt=sum(((indicatrice_cnt-prediction_cnt)^2%*%weights_cnt))

return(c(RMSE, Scnt))}

#######Function SCNT for categorical outputs

SCNT_cat=function(Obs, Pred,u_cnt) {
  
  # calculate the matrix with estimated exceedance probability of the severity thresholds:
  indicatrice_cnt=Pred
  Proba_cum=t(apply(Pred,1, cumsum))
  prediction_cnt=Proba_cum
  
  for(k in 1:length(u_cnt)){
    Ind_k=Obs
    Ind_k[u_cnt[k]<Obs]<-0
    Ind_k[u_cnt[k]>=Obs]<-1
    indicatrice_cnt[,k]=Ind_k
  }
  
  #Computation of Scnt
  
  Scnt=sum(((indicatrice_cnt-prediction_cnt)^2%*%weights_cnt))
  
  return(Scnt)}


#List including all model predictions

Names_cont=c("glm", "lasso", "glmer_site_time", "RF", "RF glmer", "RF reduced")
Pred_list=list(pred_mean_cnt_glm, pred_mean_cnt_lasso, pred_mean_cnt_glmer, pred_mean_cnt_ranger$predictions, 
               pred_mean_cnt_ranger_glmer$predictions, pred_mean_cnt_ranger_small$predictions)
Names_cat=c("RF cat unweighted", "RF cat weighted")
Proba_list=list(pred_cnt_ranger_cat$predictions,pred_cnt_ranger_cat_w$predictions)

save(Pred_list,file="Pred_list")
save(Proba_list,file="Proba_list")
load("Pred_list")
load("Proba_list")

Proba_list[[1]]<-pred_cnt_ranger_cat$predictions
#Pred_list[[5]]<-pred_mean_cnt_ranger_glmer$predictions

RMSE_all=c()
SCNT_all=c()

for (m in 1:6) {
Pred=unlist(Pred_list[m])
Obs=test_select_DF$CNT
Res=SCNT_cont(Obs, Pred, Names[m],u_cnt)
RMSE_all=c(RMSE_all,Res[1])
SCNT_all=c(SCNT_all,Res[2])
}

for (m in 1:2) {
Pred=Proba_list[[m]]
Pred=Pred[,-ncol(Pred)]
Obs=test_select_DF$CNT
Resc=SCNT_cat(Obs, Pred,u_cnt)
RMSE_all=c(RMSE_all, NA)
SCNT_all=c(SCNT_all,Resc)
}

ResultTAB=as.data.frame(cbind(c(Names_cont,Names_cat),round(RMSE_all,2),round(SCNT_all,7)))
names(ResultTAB)=c("Names", "RMSE", "S_cnt")
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

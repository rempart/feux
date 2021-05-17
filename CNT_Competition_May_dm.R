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
#Weight_CNT=train_DF$CNT
#Weight_list=weights_cnt

for(k in 2:length(u_cnt)){
  Cat_CNT[(train_DF$CNT>u_cnt[k-1]) & (train_DF$CNT<=u_cnt[k])] = u_cnt[k]
  #Weight_CNT[(train_DF$CNT>u_cnt[k-1]) & (train_DF$CNT<=u_cnt[k])] = Weight_list[k]
}

Cat_CNT[train_DF$CNT<=0]=u_cnt[1]
#Weight_CNT[train_DF$CNT<=0]=Weight_list[1]

Cat_CNT[(train_DF$CNT>100)]=200
#Weight_CNT[(train_DF$CNT>100)]=0

unique(Cat_CNT)
table(Cat_CNT)

##############################################################################        
####Sample 16.5% of the data in train_DF for model selection 
####-> train_select_DF: train dataset for continuous predictions
####-> train_select_Cat_CNT: categories of CNT (categorical RF)
####-> train_select_weight: weights (weighted categorical RF)
####-> train_select_site and train_select_time: for random effect model
####Keep 100-16.5% for model building 
####-> test_select_DF: train dataset for continuous predictions
####-> test_select_Cat_CNT: categories of CNT (categorical RF)
####-> test_select_weight: weights (weighted categorical RF)
####-> test_select_site and test_select_time: for random effect model
##############################################################################

set.seed(1)
SelNum<-sample(1:nrow(train_DF), round(0.165*nrow(train_DF)), replace=F)
train_select_DF<-train_DF[-c(SelNum),]
test_select_DF<-train_DF[c(SelNum),]
#Same selection for the categorical variable
train_select_Cat_CNT<-Cat_CNT[-c(SelNum)]
test_select_Cat_CNT<-Cat_CNT[c(SelNum)]
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

###Model 1: ranger for categories
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=as.factor(train_select_Cat_CNT)
fit_ranger_cat=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, num.trees=500)
fit_ranger_cat
save(fit_ranger_cat, file="fit_ranger_cat")
load(file="fit_ranger_cat")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)
pred_cnt_ranger_cat=predict(fit_ranger_cat, data=test_select_DF_cat)
head(pred_cnt_ranger_cat$predictions)
summary(apply(pred_cnt_ranger_cat$predictions,1, sum))
save(pred_cnt_ranger_cat, file="pred_cnt_ranger_cat")

###Model 2: ranger for categories higher mtry
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=as.factor(train_select_Cat_CNT)
fit_ranger_cat_higher_mtry=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, num.trees=500,mtry=5)
fit_ranger_cat_higher_mtry
save(fit_ranger_cat_higher_mtry, file="fit_ranger_cat_higher_mtry")
load(file="fit_ranger_cat_higher_mtry")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)
pred_cnt_ranger_cat_higher_mtry=predict(fit_ranger_cat_higher_mtry, data=test_select_DF_cat)
head(pred_cnt_ranger_cat_higher_mtry$predictions)
summary(apply(pred_cnt_ranger_cat_higher_mtry$predictions,1, sum))
save(pred_cnt_ranger_cat_higher_mtry, file="pred_cnt_ranger_cat_higher_mtry")
load(file="pred_cnt_ranger_cat_higher_mtry")

###Model 3: ranger for categories lower mtry
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=as.factor(train_select_Cat_CNT)
fit_ranger_cat_lower_mtry=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, num.trees=500,mtry=2)
fit_ranger_cat_lower_mtry
save(fit_ranger_cat_lower_mtry, file="fit_ranger_cat_lower_mtry")
load(file="fit_ranger_cat_lower_mtry")
#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)
pred_cnt_ranger_cat_lower_mtry=predict(fit_ranger_cat_lower_mtry, data=test_select_DF_cat)
head(pred_cnt_ranger_cat_lower_mtry$predictions)
summary(apply(pred_cnt_ranger_cat_lower_mtry$predictions,1, sum))
save(pred_cnt_ranger_cat_lower_mtry, file="pred_cnt_ranger_cat_lower_mtry")
load(file="pred_cnt_ranger_cat_lower_mtry")

###Model 4: "double ranger" for categories
#Ranger for zeros
train_select_DF_cat=train_select_DF
train_select_DF_cat$CNT=train_select_Cat_CNT
train_select_DF_cat$CNT[train_select_DF_cat$CNT>0]<-1
train_select_DF_cat$CNT=as.factor(train_select_DF_cat$CNT)
table(train_select_DF_cat$CNT)
fit_ranger_cat_zero=ranger(CNT~., data=train_select_DF_cat, probability = TRUE, num.trees=500)
fit_ranger_cat_zero
save(fit_ranger_cat_zero, file="fit_ranger_cat_zero")
load(file="fit_ranger_cat_zero")
#Ranger for non zeros
train_select_DF_cat_nz=train_select_DF[train_select_DF$CNT>0,]
train_select_Cat_CNT_nz<-train_select_Cat_CNT[train_select_DF$CNT>0]
train_select_DF_cat_nz$CNT=as.factor(train_select_Cat_CNT_nz)
table(train_select_DF_cat_nz$CNT)
fit_ranger_cat_notzero=ranger(CNT~., data=train_select_DF_cat_nz, probability = TRUE, num.trees=500)
fit_ranger_cat_notzero
save(fit_ranger_cat_notzero, file="fit_ranger_cat_notzero")
load(file="fit_ranger_cat_notzero")

#Predictions
test_select_DF_cat=test_select_DF
test_select_DF_cat$CNT=as.factor(test_select_Cat_CNT)

pred_cnt_ranger_cat_zero=predict(fit_ranger_cat_zero, data=test_select_DF_cat)
head(pred_cnt_ranger_cat_zero$predictions)

pred_cnt_ranger_cat_notzero=predict(fit_ranger_cat_notzero, data=test_select_DF_cat)
head(pred_cnt_ranger_cat_notzero$predictions)

pred_cnt_ranger_cat_double=cbind(pred_cnt_ranger_cat_zero$predictions[,1], pred_cnt_ranger_cat_zero$predictions[,2]*pred_cnt_ranger_cat_notzero$predictions)

save(pred_cnt_ranger_cat_double,file="pred_cnt_ranger_cat_double")
load(file="pred_cnt_ranger_cat_double")

########################
####Model evaluation####
########################

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

Names_cat=c("RF default","RF cat highmtry", "RF cat lowmtry", "RF double")
Proba_list_pred=list(pred_cnt_ranger_cat$predictions,pred_cnt_ranger_cat_higher_mtry$predictions,
                     pred_cnt_ranger_cat_lower_mtry$predictions,pred_cnt_ranger_cat_double)

#save(Pred_list,file="Pred_list")
save(Proba_list_pred,file="Proba_list_pred")
#load("Pred_list")
load("Proba_list_pred")

#Proba_list[[1]]<-pred_cnt_ranger_cat$predictions
#Pred_list[[5]]<-pred_mean_cnt_ranger_glmer$predictions

#RMSE_all=c()
SCNT_all=c()

#for (m in 1:6) {
#Pred=unlist(Pred_list[m])
#Obs=test_select_DF$CNT
#Res=SCNT_cont(Obs, Pred, Names[m],u_cnt)
#RMSE_all=c(RMSE_all,Res[1])
#SCNT_all=c(SCNT_all,Res[2])
#}

for (m in 1:4) {
Pred=Proba_list_pred[[m]]
Pred=Pred[,-ncol(Pred)]
Obs=test_select_DF$CNT
Resc=SCNT_cat(Obs, Pred,u_cnt)
#RMSE_all=c(RMSE_all, NA)
SCNT_all=c(SCNT_all,Resc)
}

ResultTAB=as.data.frame(cbind(Names_cat,round(SCNT_all,4)))
names(ResultTAB)=c("Names", "S_cnt")
ResultTAB


######Stop running here######


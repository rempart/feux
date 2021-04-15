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

###Model 3: ranger for categories
train_DF_cat=train_DF
train_DF_cat$BA=as.factor(Cat_BA)
pred_ranger_cat_ba=ranger(BA~., data=train_DF_cat, probability = TRUE)
save(pred_ranger_cat_ba, file="pred_ranger_cat_ba")
load(file="pred_ranger_cat_ba")
#Predictions
pred_ba_ranger_cat=predict(pred_ranger_cat_ba, data=test_DF)
head(pred_ba_ranger_cat$predictions)
summary(apply(pred_ba_ranger_cat$predictions,1, sum))
pred_ba_ranger_cat_final<-pred_ba_ranger_cat$predictions[,-ncol(pred_ba_ranger_cat$predictions)]
save(pred_ba_ranger_cat_final, file="pred_ba_ranger_cat_final")
load(file="pred_ba_ranger_cat_final")

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

###Model 6: ranger for categories
#Train
train_DF_cat=train_DF
train_DF_cat$CNT=as.factor(Cat_CNT)
pred_ranger_cat_cnt=ranger(CNT~., data=train_DF_cat, probability = TRUE, num.trees=500)
save(pred_ranger_cat_cnt, file="pred_ranger_cat_cnt")
load(file="pred_ranger_cat_cnt")
#Predictions
pred_cnt_ranger_cat=predict(pred_ranger_cat_cnt, data=test_DF)
head(pred_cnt_ranger_cat$predictions)
summary(apply(pred_cnt_ranger_cat$predictions, 1, sum))
pred_cnt_ranger_cat_final<-pred_cnt_ranger_cat$predictions[,-ncol(pred_cnt_ranger_cat$predictions)]
save(pred_cnt_ranger_cat_final, file="pred_cnt_ranger_cat_final")
load(file="pred_cnt_ranger_cat_final")


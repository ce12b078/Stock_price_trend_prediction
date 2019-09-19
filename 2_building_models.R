#In this code, we will split processed rain data into train/test & train some models
#Tried logistic regression with all vars
#Tried stepwise forward var selection with glm to find most imp vars


#Loading the processed data
setwd("C:/Users/dell/Desktop/mmm/kaggle1")
getwd()
trdata<-read.csv("train_processed.csv", header = T, na.strings=c(""))

#Removing last 12 rows as their return variables we can't calculate
trdata = trdata[1:5910,]
#train = trdata[1:4000,]
#test = trdata[4001:5910,]

# Creating 2:1 split of train/test
train_obs<-sample(c(TRUE,TRUE,FALSE),nrow(trdata),rep=TRUE)
test_obs<-(!train_obs)

table(train_obs)
table(test_obs)

train = trdata[train_obs,]
test = trdata[test_obs,]

dim(train)
dim(test)

#####################################################################################


#Applying logistic regression with all vars

start_time = Sys.time()
model1 = glm (TargetVariable ~ .,data=train , family = binomial)
end_time = Sys.time()
end_time - start_time


predicttrain = predict(model1, type="response")
table(train$TargetVariable, predicttrain>0.5)

#acc = (1405+2293)/nrow(train) = 0.93

predicttest = predict(model1 , type = "response" , newdata = test)
table(test$TargetVariable , predicttest>0.5)

#acc = (606+1041)/nrow(test) = 0.85


#install.packages("ROCR")
library("ROCR")

ROCRpred = prediction(predicttrain, train$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

View(ROCRpred)
View(ROCRperf)
View(auc.tmp)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc
# train auc = 0.98

ROCRpred = prediction(predicttest, test$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc
# test auc = 0.93

#####################################################################################


# stepwise forward var selection with glm

start_time = Sys.time()
nomodel<-glm(TargetVariable~1, data = train, family = "binomial")
forwards=step(nomodel,scope=list(lower=formula(nomodel),upper=formula(model1)),direction = "forward")
end_time = Sys.time()
end_time - start_time

summary(forwards)



# The most imp variables turn out to be Variable74LAST_PRICE + Variable55LAST_PRICE + Variable169LAST

model2 = glm(TargetVariable ~ Variable74LAST_PRICE + Variable55LAST_PRICE + Variable169LAST, data=train, family = binomial)


predicttrain = predict(model2, type="response")
table(train$TargetVariable, predicttrain>0.5)

#acc = (1275+2218)/nrow(train) = 0.89

predicttest = predict(model2 , type = "response" , newdata = test)
table(test$TargetVariable , predicttest>0.5)

#acc = (656+1113)/nrow(test) = 0.89


#install.packages("ROCR")
library("ROCR")

ROCRpred = prediction(predicttrain, train$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc

# train auc = 0.96

ROCRpred = prediction(predicttest, test$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc

# test auc = 0.95

#####################################################################################
#####################################################################################
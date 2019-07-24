
#Loading data

rm(list=ls())

setwd("C:/Users/hp/Downloads/kaggle1")
getwd()
data<-read.csv("TrainingData.csv", header = T, na.strings=c(""))

trdata <- data
dim(trdata)


#Removing variables having only missing values

trdata = trdata[, !sapply(trdata, function(x)all(is.na(x))), drop=F]
dim(trdata)


#Checking variable classes

type_cnts<- sapply(trdata, class)
table(type_cnts)


#removing columns in form of factors as all have 0 or missing

summary(trdata[,names(which(type_cnts == "factor"))])
rem_names<-names(which(type_cnts == "factor"))
trdata <- trdata[,!(names(trdata) %in% rem_names)]
dim(trdata)


#Removing var with only 2 unique integer values as one of them is NA

uni_cnts<- sapply(trdata,function(x)(length(unique(x))))
table(uni_cnts[uni_cnts %in% c(1,2,3,4,5)])
unique2<-trdata[,names(which(uni_cnts==2))]
sapply(unique2, function(x)(unique(x)))

rem_names<-names(which(uni_cnts==2))
trdata <- trdata[,(!names(trdata) %in% rem_names)]
dim(trdata)



# checking how var with 3 or 4 uniques are distributed

unique3<-trdata[,names(which(uni_cnts==3))]
sapply(unique3, function(x)(unique(x)))

unique4<-trdata[,names(which(uni_cnts==4))]
sapply(unique4, function(x)(unique(x)))
dim(trdata)


#Checking how table() and tabulate results vary

v<-trdata$Variable150HIGH
uniqv <- unique(v)
match(v, uniqv)
tabulate(match(v, uniqv))
table(v)
table(v,useNA = "ifany")


#Removing all columns having no of missing values more than 2000


temp<-is.na(trdata)
fun1<-function(x){sum(is.na(x))}
fun1(trdata$Variable150HIGH)
freq<-sapply(trdata,fun1)
table(freq)

na_vars1<-names(which(freq<2000))
length(na_vars1)
trdata <- trdata[na_vars1]
dim(trdata)



#replacing all NA of, all var having (only 2 unique values and NA), with the most frequent value 






getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (i in 1:ncol(trdata)) { if(length(unique(trdata[,i])) == 3) {
  resultmode <- getmode(trdata[,i])
  trdata[,i][is.na(trdata[,i])] <- resultmode
} }

dim(trdata)



#Calculating return variables for all variables

for (i in 1:499) { for (j in 1:5922) {if (is.na(trdata[j,i])) trdata[j,i] =
    mean(trdata[,i], na.rm = T)} }

cols.dont.want <- "Timestamp"
trdata <- trdata[, ! names(trdata) %in% cols.dont.want, drop = F]
for(i in 1:498) { for(j in 13:5922) { trdata[j,i] = (trdata[j,i]-
                                                       trdata[j-12,i]) } }

dim(trdata)


#Splitting the data into train & test

trdata$TargetVariable = data$TargetVariable
trdata$Prev_movement = c(0,data$TargetVariable[1:5921])


trdata = trdata[13:5922,]
#train = trdata[1:4000,]
#test = trdata[4001:5910,]
dim(trdata)




train_obs<-sample(c(TRUE,TRUE,FALSE),nrow(trdata),rep=TRUE)
test_obs<-(!train_obs)

table(train_obs)
table(test_obs)

train = trdata[train_obs,]
test = trdata[test_obs,]

dim(train)
dim(test)


head(trdata$TargetVariable, 20)
head(trdata$Prev_movement, 20)

#####################################################################################


#Applying logistic regression with all vars


start_time = Sys.time()
model1 = glm (TargetVariable ~ .,data=train , family = binomial)
end_time = Sys.time()
end_time - start_time


predicttrain = predict(model1, type="response")
table(train$TargetVariable, predicttrain>0.5)

#acc = (1251+2235)/nrow(train) = 0.89

predicttest = predict(model1 , type = "response" , newdata = test)
table(test$TargetVariable , predicttest>0.5)

#acc = (605+1054)/nrow(test) = 0.83


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

# train auc = 0.95

ROCRpred = prediction(predicttest, test$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc

# test auc = 0.86


#####################################################################################




# Rescale/standardize data & apply glm

library('oce')
library('normalr')
library('BBmisc')

dim(trdata)

train = trdata[train_obs,]
test = trdata[test_obs,]



trdata_st <- normalize(as.data.frame(train[,1:499]), method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
test_st <- normalize(as.data.frame(test[,1:499]), method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")

mean(train_st[,1])
var(train_st[,1])

mean(test_st[,1])
var(test_st[,1])


train_st$TargetVariable = train$TargetVariable
test_st$TargetVariable = test$TargetVariable

names(train_st)
names(test_st)


#rescale(x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE))


start_time = Sys.time()
model1 = glm (TargetVariable ~ .,data=train_st , family = binomial)
end_time = Sys.time()
end_time - start_time


predicttrain = predict(model1, type="response")
table(train_st$TargetVariable, predicttrain>0.5)

#acc = (1266+2194)/nrow(train_st) = 0.89

predicttest = predict(model1 , type = "response" , newdata = test_st[,1:518])
table(test_st$TargetVariable , predicttest>0.5)


#(611+1090)/nrow(test_st) = 0.84



########################################################################################





#####################################################################################







# Now let's check which variables are important using PCA
library("FactoMineR")
dim(trdata)

vars_lst <- !(names(trdata) %in% "TargetVariable")
dim(trdata[,vars_lst])

start_time = Sys.time()
res.pca <- PCA(trdata[,vars_lst],graph = FALSE)
end_time = Sys.time()
end_time - start_time
print(res.pca)


head(res.pca$eig[,1:2])
head(res.pca$ind$coord)


#install.packages('factoextra')
library("factoextra")
fviz_screeplot(res.pca,ncp=10)

?fviz_pca_var
fviz_pca_var(res.pca,select.var = list(cos2 = 0.7))


?fviz_pca_contrib
fviz_pca_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_pca_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_pca_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_pca_contrib(res.pca, choice = "var", axes = 1:2, top = 50)

#####################################################################################
PCAdat<-(res.pca$ind$coord[,1:3])
PCAdat<-as.data.frame(PCAdat)
names(PCAdat)
dim(PCAdat)
dim(trdata)
PCAdat$TargetVariable<-trdata$TargetVariable
train = PCAdat[train_obs,]
test = PCAdat[test_obs,]

dim(train)
dim(test)

#Applying logistic regression with 3 PC


start_time = Sys.time()
model1 = glm (TargetVariable ~ .,data=train , family = binomial)
end_time = Sys.time()
end_time - start_time


predicttrain = predict(model1, type="response")
table(train$TargetVariable, predicttrain>0.5)

#acc = (60+2360)/nrow(train) = 0.62

predicttest = predict(model1 , type = "response" , newdata = test)
table(test$TargetVariable , predicttest>0.5)

#acc = (23+1223)/nrow(test) = 0.62







######################################################################

#install.packages("caret")


dim(trdata)
cols.dont.want <- "TargetVariable" 
subset1 = trdata[,!names(trdata) %in% cols.dont.want] 


for(i in 1:499){ for (j in 1:79){
  sdvector = c(subset1[j,i])
  k = j+80 
  while(k<=5910){
    sdvector <- c(sdvector, subset1[k,i])
    k = k+80 
  } 
  sdvalue = sd(sdvector, na.rm = FALSE)
  if (sdvalue != 0){
    l = j 
    while(l<=5910){
      subset1[l,i] = subset1[l,i]/sdvalue
      l = l+80
    }
  }
  else {
    print(i)
    print(j)
  }
}
}
subset1$TargetVariable = trdata$TargetVariable 


train_obs<-sample(c(TRUE,TRUE,FALSE),nrow(subset1),rep=TRUE)
test_obs<-(!train_obs)

table(train_obs)
table(test_obs)

train1 = subset1[train_obs,]
test1 = subset1[test_obs,]



#train1 = subset1[1:4029,] 
#test1 = subset1[4030:5910,] 
#model4 = glm(TargetVariable ~ Variable74LAST_PRICE + Variable55LAST_PRICE + Variable169LAST, data=train1, family = binomial)

start_time = Sys.time()
model1 = glm (TargetVariable ~ .,data=train1 , family = binomial)
end_time = Sys.time()
end_time - start_time


predicttrain = predict(model1, type="response")
table(train1$TargetVariable, predicttrain>0.5)

#acc = (1233+2023)/nrow(train1) = 0.83

predicttest = predict(model1 , type = "response" , newdata = test1)
table(test1$TargetVariable , predicttest>0.5)

#acc = (613+985)/nrow(test1) = 0.80




xTrain<-train1[,!(names(train1) %in%  ("TargetVariable"))]
yTrain<-train1[,(names(train1) %in%  ("TargetVariable"))]
xTest<-test1[,!(names(test1) %in%  ("TargetVariable"))]
yTest<-test1[,c("TargetVariable")]
dim(xTrain)
dim(yTrain)
yTest

library(e1071)
model6 <- svm(xTrain, yTrain, type='C-classification', kernel='radial', cost=1) 
svmpred <- predict(model6, xTest,decision.values = TRUE) 
table(yTest, svmpred)

#acc on test data = (651+1103)/nrow(xTest) = 88%

#print(confusionMatrix(svmpred,yTest)) 
# "caret" package is needed for confusionMatrix function 
# this model gave accuracy of 0.904 on test data and 0.894 on train data 

svm.roc <- prediction(attributes(svmpred)$decision.values, yTest) 
svm.auc <- performance(svm.roc, 'tpr', 'fpr') 
aucsvm <- performance(svm.roc, 'auc') 
plot(svm.auc) 
auc <- as.numeric(aucsvm@y.values) 
# gave auc of 0.87 for test data 



#################################################################################


library(caret)

preProcValues <- preProcess(subset1[,1:499], method = c("knnImpute","center","scale"))

#install.packages('RANN')
library('RANN')
train_processed <- predict(preProcValues, subset1[,1:499])
sum(is.na(train_processed))

dim(train_processed)

train_processed$TargetVariable<-as.factor(subset1$TargetVariable)



index <- createDataPartition(train_processed$TargetVariable, p=0.75, list=FALSE)
trainSet <- train_processed[ index,]
testSet <- train_processed[-index,]

dim(trainSet)
dim(testSet)


# trying gbm with tuning grid
fitControl<-trainControl(method = "repeatedcv", number=4, repeats = 2)
modelLookup(model='gbm')

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))
grid <- expand.grid(n.trees=c(1000),shrinkage=c(0.01),n.minobsinnode = c(3),interaction.depth=c(5))


model_gbm<-train(trainSet[,1:499],trainSet[,500],method='gbm',trControl=fitControl,tuneGrid=grid)
print(model_gbm)


#stepwise forward logistic regression to find imp variables
trainSet1 = trainSet
head(trainSet1)
model1 = glm (TargetVariable ~ .,data=trainSet1 , family = binomial)
start_time = Sys.time()
nomodel<-glm(TargetVariable~1, data = trainSet1, family = "binomial")
forwards=step(nomodel,scope=list(lower=formula(nomodel),upper=formula(model1)),direction = "forward")
end_time = Sys.time()
end_time - start_time

summary(forwards)
#best model is TargetVariable ~ Prev_movement + Variable113HIGH + Variable22HIGH

model2 = glm(TargetVariable ~ Prev_movement + Variable113HIGH + Variable22HIGH, data=trainSet1, family = binomial)


predicttrain = predict(model2, type="response")
table(trainSet1$TargetVariable, predicttrain>0.5)

#acc = (1445+2428)/nrow(trainSet1) = 0.87

predicttest = predict(model2 , type = "response" , newdata = testSet)
table(testSet$TargetVariable , predicttest>0.5)

#acc = (475+827)/nrow(testSet) = 0.88






###################Tuning grid randomForest


?trainControl
?rfeControl
?sbfControl



outcomeName<-'TargetVariable'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]


fitControl<-trainControl(method = "repeatedcv", number=4, repeats = 2)

"rf" %in% names(getModelInfo())
modelLookup(model='rf')
grid <- expand.grid(mtry=c(2,3))



predictors <- c("Prev_movement","Variable113HIGH","Variable22HIGH")

model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf',trControl=fitControl,tuneGrid=grid)
print(model_rf)
plot(model_rf)
varImp(object=model_rf)
plot(varImp(object=model_rf),main="RF - Variable Importance")

predictions<-predict.train(object=model_rf,testSet[,predictors],type="raw")
table(predictions)
table(testSet$TargetVariable , predictions)

# acc =(461+828)/nrow(testSet) = 0.87





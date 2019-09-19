#Apply PCA to processed data to check for results after dimensionality reduction
#Doing time of the day normalisation of variables
#Then, building a glm model on time of day normalised vars



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

#install.packages("FactoMineR")
#install.packages("factoextra")

# Now let's check which variables are important using PCA
library("FactoMineR")
dim(train)

start_time = Sys.time()
res.pca <- PCA(train[,1:499],graph = FALSE)
end_time = Sys.time()
end_time - start_time
print(res.pca)

#Visualizing top components
head(res.pca$eig[,1:2])


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


#NOTE : In PCA results, 1st PC explains 36% of var, after that 2nd,3rd PCs onwards, very less var is explained by PC's
#So, ging ahead with some advanced techniques using caret, without PCA

# Rescale/standardize data & apply glm
#install.packages("caret")
library(caret)

preProcValues <- preProcess(trdata[1:5910,1:499], method = c("knnImpute","center","scale"))

#install.packages('RANN')
library('RANN')
train_processed <- predict(preProcValues, trdata[1:5910,1:499])
sum(is.na(train_processed))

dim(train_processed)
train_processed$TargetVariable=trdata[1:5910,500]
train_processed$TargetVariable<-as.factor(train_processed$TargetVariable)

index <- createDataPartition(train_processed$TargetVariable, p=0.75, list=FALSE)
trainSet <- train_processed[ index,]
testSet <- train_processed[-index,]

dim(trainSet)
dim(testSet)

############################################################################

#Recursive Feature Elimination(RFE) recursively removes features, and builds a model using the remaining attributes and calculates model accuracy.
#This code will take some time to run on local system

control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName<-'TargetVariable'
predictors<-names(trainSet)[!names(trainSet) %in% outcomeName]
Loan_Pred_Profile <- rfe(trainSet[,predictors], trainSet[,outcomeName],
                         rfeControl = control)

Loan_Pred_Profile


############################################################################
#Building advanced models using caret

predictors<-c("Variable74LAST_PRICE" , "Variable55LAST_PRICE" , "Variable169LAST")

#To check if glm model exists in caret model list
"glm" %in% names(getModelInfo())


model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')


#Tuning a gbm model
fitControl<-trainControl(method = "repeatedcv", number=4, repeats = 4)
modelLookup(model='gbm')

grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),interaction.depth=c(1,5,10))

model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)
print(model_gbm)





############################################################################
#Doing time of the day normalisation of variables
#Then, building a glm model on time of day normalised vars


cols.dont.want <- "TargetVariable" 
subset1 = train_processed[,!names(train_processed) %in% cols.dont.want] 


for(i in 1:499){ for (j in 1:79){
  sdvector = c(subset1[j,i])
  k = j+80 
  while(k<=499){
    sdvector <- c(sdvector, subset1[k,i])
    k = k+80 
  } 
  sdvalue = sd(sdvector, na.rm = FALSE)
  if (sdvalue != 0){
    l = j 
    while(l<=499){
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
subset1$TargetVariable = train_processed$TargetVariable 
train1 = subset1[1:4029,] 
test1 = subset1[4030:5910,] 
model4 = glm(TargetVariable ~ Variable74LAST_PRICE + Variable55LAST_PRICE + Variable169LAST, data=train1, family = binomial)



predicttrain = predict(model4, type="response")
table(train1$TargetVariable, predicttrain>0.5)

#acc = (1394+2192)/nrow(train1) = 0.89

predicttest = predict(model4 , type = "response" , newdata = test1)
table(test1$TargetVariable , predicttest>0.5)

#acc = (585+1073)/nrow(test1) = 0.88


#install.packages("ROCR")
library("ROCR")

ROCRpred = prediction(predicttrain, train1$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc

# train auc = 0.95

ROCRpred = prediction(predicttest, test1$TargetVariable)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
auc.tmp <- performance(ROCRpred,"auc")
auc <- as.numeric(auc.tmp@y.values)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))
auc

# test auc = 0.95

#########################################################################
#########################################################################
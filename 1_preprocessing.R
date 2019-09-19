#In this code, we will preprocess the train data, clean it and crate return vars

#Loading data
rm(list=ls())
setwd("C:/Users/dell/Desktop/mmm/kaggle1")
getwd()
data<-read.csv("TrainingData.csv", header = T, na.strings=c(""))



#Removing variables having only missing values
trdata <- data
dim(trdata)
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
for(i in 1:498) { for(j in 1:5910) { trdata[j,i] = (trdata[j,i]-
                                                      trdata[j+12,i]) } }

dim(trdata)


#Splitting the data into train & test

trdata$TargetVariable = data$TargetVariable




# save the processed data
write.csv(trdata,"train_processed.csv")


############################################################################
############################################################################


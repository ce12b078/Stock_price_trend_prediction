# Stock_price_trend_prediction
supervised machine learning to predict the movement of a stock price

1_preprocessing
R file
#In this code, we will preprocess the train data, clean it and crate return vars

2_building_models
R file
#In this code, we will split processed data into train/test & train some models
#Tried logistic regression with all vars
#Tried stepwise forward var selection with glm to find most imp vars

3_PCA_timeOfDayNorm_Caret_ML_models
R file
#Apply PCA to processed data to check for results after dimensionality reduction
#Doing time of the day normalisation of variables
#Then, building a glm model on time of day normalised vars

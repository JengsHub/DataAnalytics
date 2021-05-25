rm(list = ls())
# set to working directory
options(digits=4)

#Reading the data
WAUS <- read.csv("CloudPredict2021.csv")
L <- as.data.frame(c(1:49))
set.seed(29637996) # Your Student ID is the random seed
L <- L[sample(nrow(L), 10, replace = FALSE),] # sample 10 locations
WAUS <- WAUS[(WAUS$Location %in% L),]
WAUS <- WAUS[sample(nrow(WAUS), 2000, replace = FALSE),] # sample 3000 rows

# Checking number of NA values in each column
colSums(is.na(WAUS))

# More than half of these columns are empty, so we remove it
WAUS$Sunshine = NULL
WAUS$Evaporation = NULL

# show number of entries before and after removing rows
cat("Number of Entries before removal: ", nrow(WAUS),"\n")

# Omitting NAs
WAUS = na.omit(WAUS)

cat("Number of Entries after removal: ", nrow(WAUS), "\n")

# Data checking
num_missing_values <- function(x) { return(sum(is.na(x)))}
num_empty_cells <- function(x) { return(sum(x==""))}

num_rows = length(colnames(WAUS))
new_table = data.frame(num_missing_values = integer(num_rows))

new_table$num_missing_values = apply(WAUS,2,num_missing_values)
new_table$num_empty_cells = apply(WAUS,2,num_empty_cells)
rownames(new_table) = colnames(WAUS)

new_table
unique(WAUS$Location)

sum(WAUS$CloudTomorrow == 1)
sum(WAUS$CloudTomorrow == 0)
# Identify the proportion of 'Cloudy'
sum(WAUS$CloudTomorrow == 1)  / nrow(WAUS) * 100

# Identify the proportion of 'clear'
sum(WAUS$CloudTomorrow == 0) / nrow(WAUS) * 100


# Descriptions of predictors, what is real valued attributes?
WAUS_real_valued = WAUS[c("MinTemp", "MaxTemp", "Rainfall", "Pressure9am", 
                          "Pressure3pm", "Temp9am", "Temp3pm")]
summary(WAUS_real_valued)

# Categorize direction character to number
# For WindGustDir
wind_gust_label = as.data.frame(unique(WAUS$WindGustDir))
wind_gust_label$ID = 1:nrow(wind_gust_label)
colnames(wind_gust_label) <- c("WindGustDir","WindGustDirID")
wind_gust_label
WAUS = merge(x = WAUS, y= wind_gust_label, by="WindGustDir")

# For WindDir9am
wind_dir_9_label = as.data.frame(unique(WAUS$WindDir9am))
wind_dir_9_label$ID = 1:nrow(wind_dir_9_label)
colnames(wind_dir_9_label) <- c("WindDir9am","WindDir9amID")
wind_dir_9_label
WAUS = merge(x = WAUS, y= wind_dir_9_label, by="WindDir9am")

# For WindDir3pm
wind_dir_3_label = as.data.frame(unique(WAUS$WindDir3pm))
wind_dir_3_label$ID = 1:nrow(wind_dir_3_label)
colnames(wind_dir_3_label) <- c("WindDir3pm","WindDir3pmID")
wind_dir_3_label
WAUS = merge(x = WAUS, y= wind_dir_3_label, by="WindDir3pm")

# For RainToday
rain_tod_label = as.data.frame(unique(WAUS$RainToday))
rain_tod_label$ID = 1:nrow(rain_tod_label)
colnames(rain_tod_label) <- c("RainToday","RainTodayID")
rain_tod_label
WAUS = merge(x = WAUS, y= rain_tod_label, by="RainToday")

# Replace old columns with new ones
WAUS$WindGustDir = WAUS$WindGustDirID 
WAUS$WindDir3pm = WAUS$WindDir3pmID
WAUS$WindDir9am = WAUS$WindDir9amID
WAUS$RainToday = WAUS$RainTodayID

WAUS$WindGustDirID = NULL
WAUS$WindDir9amID = NULL
WAUS$WindDir3pmID = NULL
WAUS$RainTodayID = NULL

fit = lm(CloudTomorrow ~ ., data = WAUS)
fit
summary(fit)

# filter p values lower than 0.45
WAUS = WAUS[c("Location", "Day", "Year", "MinTemp","MaxTemp", "WindSpeed3pm",
              "Humidity9am", "WindGustSpeed",
              "Humidity3pm", "Pressure3pm", "Temp9am", "Temp3pm", "Rainfall",
              "CloudTomorrow")]

# Converting the Location, WindDir3pm and 9am to factors
WAUS$Location = factor(WAUS$Location)

set.seed(29637996) #Student ID as random seed
train.row = sample(1:nrow(WAUS), 0.7*nrow(WAUS))
data.train = WAUS[train.row,]
data.test = WAUS[-train.row,]

# Decision Tree Classification model
library(tree)
data.train$CloudTomorrow = as.factor(data.train$CloudTomorrow)
DT_Model = tree(CloudTomorrow ~ ., data = data.train)

# Naive Bayes
library(e1071)
NB_Model = naiveBayes(CloudTomorrow ~ ., data = data.train)

# Bagging
library(adabag)
bag_Model = bagging(CloudTomorrow	~	.,	data= data.train,	mfinal=10)

# Boosting
library(rpart)
boost_Model = boosting(CloudTomorrow ~ ., data = data.train, mfinal = 10)

# Random Forest
library(randomForest)
rf_Model = randomForest(CloudTomorrow ~., data = data.train) 

# Use test data to classify test cases as "cloudy" or "not cloudy"
data.test$CloudTomorrow = as.factor(data.test$CloudTomorrow)

# obtain prediction for dt
dt.predict = predict(DT_Model, data.test, type = "class")
dt_table = table(predicted = dt.predict, actual = data.test$CloudTomorrow)
cat("Matrix for Decision Tree:")
dt_table

cat("Accuracy for Decision Tree:")
accu_dt = sum(diag(dt_table))/nrow(data.test)
accu_dt

# obtain prediction for naive bayes
nb.predict = predict(NB_Model, data.test)
nb_table = table(predicted = nb.predict, actual = data.test$CloudTomorrow) 
cat("Matrix for Naive Bayes:")
nb_table

cat("Accuracy for Naive Bayes:")
accu_nb = sum(diag(nb_table))/nrow(data.test)
accu_nb

# obtain prediction bagging
bag.predict = predict.bagging(bag_Model,data.test)

accu_bag = sum(diag(bag.predict$confusion))/nrow(data.test)
cat("Accuracy for Bagging:")
accu_bag

# obtain prediction for boosting
boost.predict = predict.boosting(boost_Model,data.test)

accu_boost = sum(diag(boost.predict$confusion))/nrow(data.test)
cat("Accuracy for Boosting:")
accu_boost


# obtain prediction for random forest
rf.predict = predict(rf_Model,data.test)

rf_table = table(predicted = rf.predict, actual = data.test$CloudTomorrow)
cat("Matrix for Random Forest:")
rf_table

accu_rf = sum(diag((rf_table))/nrow(data.test))
cat("Accuracy for Random Forest:")
accu_rf

# Calculate confidence for predicting "cloudy tomorrow" for each class
library(ROCR)

# For decision tree
dt_predict_conf = predict(DT_Model, data.test,type = "vector")
dt_pred = prediction(dt_predict_conf[,2], data.test$CloudTomorrow)
dt_perf = performance(dt_pred, "tpr", "fpr")
plot(dt_perf,col = "blue")

# for naive bayes
nb_pred_conf = predict(NB_Model, data.test, type = "raw")
nb_pred = prediction(nb_pred_conf[,2], data.test$CloudTomorrow)
nb_perf = performance(nb_pred, "tpr", "fpr")
plot(nb_perf ,add = TRUE,col = "green")

# for bagging
bag_pred = prediction(bag.predict$prob[,2], data.test$CloudTomorrow)
bag_perf = performance(bag_pred, "tpr", "fpr")
plot(bag_perf ,add = TRUE,col = "yellow")

# for boosting
boost_pred = prediction(boost.predict$prob[,2], data.test$CloudTomorrow)
boost_perf = performance(boost_pred, "tpr", "fpr")
plot(boost_perf ,add = TRUE,col = "red")

# for random forest 
rf_pred_prob = predict(rf_Model, data.test, type = "prob")
predData = cbind(rf_pred_prob[,2],data.test$CloudTomorrow)
temp_df = data.frame(predData)


rf_pred_conf = prediction(temp_df[,1], temp_df[,2])
rf_pred = performance(rf_pred_conf,"tpr","fpr")
plot(rf_pred, add = TRUE,col = "orange")

# Adding legend
title("ROC Curve for Classification models")
legend("bottomright", plot_range[2],
       c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest")
       , col=c("blue","green","yellow","red","orange"), pch=21:22, lty=1:2)


# AUC for each classifier
# Decision tree AUC
dt_auc = performance(dt_pred, "auc")
cat("Decision Tree AUC: ", (as.numeric(dt_auc@y.values)))

# Naive Bayes tree AUC
nb_auc = performance(nb_pred, "auc")
cat("Naive Bayes AUC: ", (as.numeric(nb_auc@y.values)))

# Bagging AUC
bag_auc = performance(bag_pred, "auc")
cat("Bagging AUC: ", (as.numeric(bag_auc@y.values)))

# Boosting AUC
boost_auc = performance(boost_pred, "auc")
cat("Boosting AUC: ", (as.numeric(boost_auc@y.values)))

# Random Forest AUC
rf_auc = performance(rf_pred_conf, "auc")
cat("Random Forest AUC: ", (as.numeric(rf_auc@y.values)))

# Identifying attribute importance for classification models
cat("#Decision Tree Attribute Importance: \n") 
print(summary(DT_Model)) 
plot(DT_Model)
text(DT_Model, pretty = 0)


# Naive Bayes assumes all attributes are of equal importance

cat("\n#Baging Attribute Importance: \n") 
print(bag_Model$importance) 

cat("\n#Boosting Attribute Importance: \n") 
print(boost_Model$importance) 

cat("\n#Random Forest Attribute Importance: \n") 
print(rf_Model$importance)

test_dt_model =	cv.tree(DT_Model,	FUN	=	prune.misclass)
test_dt_model
# size 3 best

prune_dt_model =	prune.misclass(DT_Model,	best	=	3)
summary(prune_dt_model)
plot(prune_dt_model)
text(prune_dt_model, pretty = 0)

# Testing performance of pruned Decision Tree
prune_dt.predict = predict(prune_dt_model, data.test, type = "class")
prune_dt_pred = prediction(dt_predict_conf[,2], data.test$CloudTomorrow)
prune_dt_table = table(predicted = prune_dt.predict, actual = data.test$CloudTomorrow)
cat("Matrix for Pruned Decision Tree:")
prune_dt_table

cat("Accuracy for Pruned Decision Tree:")
accu_prune_dt = sum(diag(prune_dt_table))/nrow(data.test)
accu_prune_dt

# ROC for Pruned Decision Tree
prune_dt_predict_conf = predict(prune_dt_model, data.test,type = "vector")
prune_dt_pred = prediction(prune_dt_predict_conf[,2], data.test$CloudTomorrow)
prune_dt_perf = performance(prune_dt_pred, "tpr", "fpr")

# Pruned Decision Tree AUC
pruned_dt_auc = performance(prune_dt_pred, "auc")
cat("Pruned Decision Tree AUC: ", (as.numeric(pruned_dt_auc@y.values)))

# Plotting the prunned decision tree with others
plot(prune_dt_perf,col = "purple")
# Re-plotting all the previous performance
plot(dt_perf, add = TRUE,col = "blue")
plot(nb_perf ,add = TRUE,col = "green")
plot(bag_perf ,add = TRUE,col = "yellow")
plot(boost_perf ,add = TRUE,col = "red")
plot(rf_pred, add = TRUE,col = "orange")

# Adding legend
title("ROC Curve for Classification models")
legend("bottomright", plot_range[2],
       c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "Pruned Decision Tree")
       , col=c("blue","green","yellow","red","orange", "purple"), pch=21:22, lty=1:2)


# Creating the best tree-based classifier by adjustment
library(pROC)
library(caret)

new_train = data.train[c("Location",
                         "Humidity3pm",
                         "Humidity9am",
                         "CloudTomorrow")]

new_test = data.test[c("Location",
                       "Humidity3pm",
                       "Humidity9am",
                       "CloudTomorrow")]


# Finding the best settings using Random Forest with k-fold cross
# validation for parameter settings 
controls = trainControl(method = "cv", number = 10, search = "grid")

set.seed(288)
rf_default_settings = train(CloudTomorrow~.,
                            data = new_train,
                            method = "rf",
                            metric = "Accuracy",
                            trControl = controls)

set.seed(288)
parameter_grid = expand.grid(.mtry = c(1: 10))
rf_mtry_adjusted = train(CloudTomorrow~.,
                         data = new_train,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = parameter_grid,
                         trControl = controls,
                         ntree = 600)

best_mtry = rf_mtry_adjusted$bestTune$mtry
best_mtry

# Performing test with the best 'mtry' parameter obtained previously
parameter_grid = expand.grid(.mtry = best_mtry)

list_maxnodes <- list()


# Iterate over different values of 'maxnodes' parameter, best mtry is used here
for (maxnodes in c(100: 110)) {
  set.seed(288)
  rf_maxnode_settings = train(CloudTomorrow ~ .,
                              data = new_train,
                              method = "rf",
                              metric = "Accuracy",
                              tuneGrid = parameter_grid,
                              trControl = controls,
                              maxnodes = maxnodes,
                              ntree = 600)
  
  curr_maxnodes = toString(maxnodes)
  list_maxnodes[[curr_maxnodes]] = rf_maxnode_settings
}

results_mtry = resamples(list_maxnodes)
summary(results_mtry)  # 106 for maxnodes

store_maxtrees = list()
for (ntree in c(1000, 1500, 2000, 2500, 3000, 3500)) {
  set.seed(288)
  rf_maxtree_setting = train(CloudTomorrow ~ .,
                             data = new_train,
                             method = "rf",
                             metric = "Accuracy",
                             tuneGrid = parameter_grid,
                             trControl = controls,
                             maxnodes = 106,
                             ntree = ntree)
  
  curr_ntree = toString(ntree)
  store_maxtrees[[curr_ntree]] = rf_maxtree_setting
}

results_tree <- resamples(store_maxtrees)
summary(results_tree)

# 2500 trees is the best

## Final model with parameter best settings
set.seed(288)
rf_final <- train(CloudTomorrow ~ .,
                  new_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = parameter_grid,
                  trControl = controls,
                  ntree = 2500,
                  maxnodes = 106)

# Evaluate the models on the test data set
default_pred = predict(rf_default_settings, new_test)
final_pred = predict(rf_final, new_test)

# Confusion matrix and their accuracy
default_table = table(predicted = default_pred, actual = new_test$CloudTomorrow)
cat("Matrix for default Random Forest:")
default_table

accu_def_rf = sum(diag((default_table))/nrow(new_test))
cat("Accuracy for default Random Forest:")
accu_def_rf

# For final settings random forest
cv_table = table(predicted = final_pred, actual = new_test$CloudTomorrow)
cat("Matrix for Cross Validated Random Forest:")
cv_table

# Find accuracy
accu_cv_rf = sum(diag((cv_table))/nrow(new_test))
cat("Accuracy for Cross Validated Random Forest:")
accu_cv_rf

# Plotting the Cross validated Random Forest
rf_pred_prob = predict(rf_final, new_test, type = "prob")
cv_predData = cbind(rf_pred_prob[,2], new_test$CloudTomorrow)
cv_temp_df = data.frame(cv_predData)


cv_rf_pred_conf = prediction(cv_temp_df[,1], cv_temp_df[,2])
cv_rf_pred = performance(cv_rf_pred_conf,"tpr","fpr")
plot(cv_rf_pred,col = "black")

# Plotting the old ones
plot(prune_dt_perf,add = TRUE,col = "purple")
plot(dt_perf, add = TRUE,col = "blue")
plot(nb_perf ,add = TRUE,col = "green")
plot(bag_perf ,add = TRUE,col = "yellow")
plot(boost_perf ,add = TRUE,col = "red")
plot(rf_pred, add = TRUE,col = "orange")

# Adding legend
title("ROC Curve for Classification models")
legend("bottomright", plot_range[2],
       c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest", "Pruned Decision Tree", "CV Random Forest")
       , col=c("blue","green","yellow","red","orange", "purple", "black"), pch=21:22, lty=1:2)

# Random Forest AUC
cv_rf_auc = performance(cv_rf_pred_conf, "auc")
cat("CV Random Forest AUC: ", (as.numeric(cv_rf_auc@y.values)))

library(neuralnet)
library(car)

# Remove variables here
ann_train = data.train[c("Location",
                         "Humidity3pm",
                         "Humidity9am",
                         "CloudTomorrow")]

ann_test = data.test[c("Location",
                       "Humidity3pm",
                       "Humidity9am",
                       "CloudTomorrow")]


DATAmerge = rbind(ann_train, ann_test)

# DATAmerge$Location = as.factor(DATAmerge$Location)

attach(DATAmerge)
Recodedata = model.matrix(~ Location+Humidity3pm+Humidity9am)

DATAmerge = cbind(DATAmerge,Recodedata)

DATAmerge = DATAmerge[, c(6,7,8,9,10,11,12,13,14,15,16,4)]
DATAmerge$CloudTomorrow = recode(DATAmerge$CloudTomorrow," '0' = 'FALSE' ; '1' = 'TRUE' ")
DATAmerge$CloudTomorrow = as.logical(DATAmerge$CloudTomorrow)
ann_train = DATAmerge[1 : 972,]
ann_test = DATAmerge[973: 1389,]

# ANN 
# pre-process and select required attributes
ann_model = neuralnet(CloudTomorrow ~ ., ann_train, hidden = 2)
plot(ann_model)

# Testing the ANN model
ann_pred = compute(ann_model, ann_test)
ann_pred_final = round(ann_pred$net.result, 0)
ann_table = table(predicted = ann_pred_final, actual = ann_test$CloudTomorrow)

acc_ann = sum(diag(ann_table))/nrow(ann_test)
cat("Accuracy of ANN: ", acc_ann)


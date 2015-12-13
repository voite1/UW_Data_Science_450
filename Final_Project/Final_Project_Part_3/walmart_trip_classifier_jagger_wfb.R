
##=========================================================================================
##
##   Script Name   : walmart_trip_classifier.R 
##   Description   : this R script is used to solve walmart trip type clasification problem.
##                   (This is Project for UW Data Science course 450 Semester 3)
##   Reference     : https://www.kaggle.com/c/walmart-recruiting-trip-type-classification 
##   Project Group : Group 10 (Aleksey Kramer , Jagger Bodas , Winston Featherly-Bean )
##   versions      :
##                  v1.0         :Feature Engineering to create following columns
##                                  weekday , 
##                                  basket_item_count,
##                                  basket_return_count,
##                                  basket_size,
##                                  return,
##                                  converted Dept Descipltion column to multiple binary columns
##                                 
##                                Excluded columns UPC and Fineline.
##                                Total Features = 78 
##                                Merged multipletransactions within a basket to form single row
##                                for each visit/basket.
##                                Built RF classifier with 500 tress. 
##                                
##                  v1.1:         incorporated incomplete cases in test data.    
##                                Accuracy (using training data) :  0.6597018
##                                Kaggle score : 1.65507
##
##                  V1.1 (current): Created weighted average ensemble model
##                                  
##===========================================================================================


## load libraries
library(randomForest)
library(arules)
library(arulesViz)
library(data.table) 
library(e1071)
library(rpart)


## set working directory
setwd('C:/Users/Locus/Documents/Winston/Class - Data Science/Class - Data at Scale/Project/Part II')


## function to load data 
load_data <- function(filename = train.csv){   

    # read data 
    data <- read.csv(filename, header = TRUE)
    
    # remove tupples with missing data
    # this line is commented because it reduces testing dataset
    # which is not acceptable by kaggle for submission.
    # data <- data[complete.cases(data),]
    
    # remove duplicate data 
    data <-  data[!duplicated(data), ]
    
    
    return(data) 

}


# function to format data 
format_data <- function(data){

    # changes data types as appropriate 
    data$Upc <- as.factor(data$Upc)
    
    # data$TripType <- as.factor(data$TripType) 
    
    data$FinelineNumber <- as.factor(data$FinelineNumber)
    
    return(data)
  
}

# add column called weekend , If Fri,Sat,Sun then 1 else 0 
add_weekend_columnn <- function(data){
  
  # add new column and initialize 
  data$Weekend <- 0
  
  # Set Value for Weekend
  data[data$Weekday %in% c('Friday','Saturday', 'Sunday') , ]$Weekend <- 1
  
  # Conver numeric column to factor
  data$Weekend <- as.factor(data$Weekend)
  
  return(data)
  
  
}

## this function adds dept description columns in binary format 
add_binary_dept_desc_column <- function(data){

  # extract VisitNumber and DepartmentDescription columns in seperate Data Frame
  data_dept <- data[,c('VisitNumber', 'DepartmentDescription')]
  
  # list down all the Dept Descriptions available
  dept_cols <- levels(data_dept$DepartmentDescription)
  
  
  # create boolean columns for Dept Description
  
  data_dept_binary <- setNames(data.frame(data_dept$VisitNumber, do.call(rbind,lapply(data_dept$DepartmentDescription, function(x) as.integer(dept_cols %in% x)) )), c("VisitNumber", dept_cols))
                               
  # Combine boolean Dept Description data with original Data 
  # data <- cbind(data, as.data.frame(lapply(data_dept_binary[,-1], factor)))
  data <- cbind(data, (data_dept_binary[,-1]))
  

                               
  return(data)
  
}


## following function adds new column called "basket_item_count"
add_basket_item_count_column <- function(data){
  
  
  # convert data frame into data table , and define Key column
  dt <- data.table(data, key = "VisitNumber")
  
  # create and populate new column called "basket_item_count" , exclude returned items
  temp_table <- setNames(dt[, sum(ScanCount[ScanCount > 0 ]),by = "VisitNumber" ], c("VisitNumber" , "basket_item_count"))
  
  # merge parent table with above created temp table
  x <- merge(dt, temp_table, by = "VisitNumber")
  
  # return as data.frame
  return(as.data.frame(x))
  
}


## following function adds new column called "basket_return_count"
add_basket_return_count_column <- function(data){
  
  
  # convert data frame into data table , and define Key column
  dt <- data.table(data, key = "VisitNumber")
  
  # create and populate new column called "basket_item_count" , exclude returned items
  temp_table <- setNames(dt[, abs(sum(ScanCount[ScanCount < 0 ])),by = "VisitNumber" ], c("VisitNumber" , "basket_return_count"))
  
  # merge parent table with above created temp table
  x <- merge(dt, temp_table, by = "VisitNumber")
  
  # return as data.frame
  return(as.data.frame(x))
  
}


## this function adds column called basket size
## it have dependancy on column called basket_item_count
## i.e. this function should be called only after 
## adding basket_item_count column.
add_basket_size_column <- function(data){
  
  # Create new column and initilize it 
  data$basket_size <- "Empty"
  
  # create Bins as follow 
  data[data$basket_item_count %in% c(1:5),]$basket_size <- "Extra Small" 
  data[data$basket_item_count %in% c(6:20),]$basket_size <- "Small" 
  data[data$basket_item_count %in% c(21:50),]$basket_size <- "Medium" 
  data[data$basket_item_count %in% c(50:100),]$basket_size <- "Large" 
  data[data$basket_item_count > 100 ,]$basket_size <- "Extra Large" 
  
  # convert column to factor 
  data$basket_size <- as.factor(data$basket_size)
  
  return(data)
  
}


# this function adds return column 
# which indicates if there are any return 
# for particular Visit 
add_return_column <- function(data){

  # create new column and initialize 
  data$return <- 0
  
  # identify visits which had return 
  data[data$basket_return_count > 0,]$return <- 1
  
  # convert this column to factor
  data$return <- as.factor(data$return)
  
  return(data)
}

# this function aggregates all the possible data for each visit 
# in single row i.e. this is last step before feeding 
# training data to a classifier 
get_single_row_for_each_visit <- function(data , data_type){

  # Identify columns which we dont want to include while aggregating transaction per visit
  drop_col <- c("Upc","ScanCount", "DepartmentDescription" , "FinelineNumber")
  
  # create data.table after excluding above columns
  dt <- data.table(data[, ! names(data) %in% drop_col], key = "VisitNumber")
  
  # use make.names to remove any spaces in col names 
  # especially Dept description columns
  names(dt) <- make.names(names(dt), unique=TRUE)
  
  # identify columns which are not related to depts
  non_dept_col <- c("basket_item_count" , "basket_return_count", "basket_size" , "return" , "VisitNumber" , "TripType" , "Weekday" , "Weekend" )
  
  # following verification is in place to check if its training data or testing data
  # in case of testing data , we will not have "TripType" column 
  # and hence it need to be excluded.
  if(data_type == "testing")
  {
    non_dept_col <- non_dept_col[!non_dept_col== "TripType"]
  }
  
  # get aggregation for all the dept columns by VisitNumber
  dt_dept_desc_aggr_by_visit <- dt[, lapply(.SD, sum, na.rm=TRUE), by=VisitNumber, .SDcols= names(dt)[! names(dt) %in% non_dept_col] ] 
  
  # create subset of non dept data per VisitNumber
  non_dept_data <- data[,non_dept_col]
  non_dept_data <- non_dept_data[!duplicated(non_dept_data),]
  
  # combine non dept data and aggregated dept data
  combined_data <- merge(non_dept_data ,as.data.frame(dt_dept_desc_aggr_by_visit ), by = "VisitNumber")
  
  return(combined_data)
  
}


## this function includes all the tasks of feature engineering
## training data as well as testing data need to go through this 
## feature engineering before getting fed to classifier
perform_feature_engineering <- function (data , data_type){
  
# format training set 
data <- format_data(data)

# add weekend column
data <- add_weekend_columnn(data)

# add dept description as individual bolean columns 
data <- add_binary_dept_desc_column(data)

# add new column called "basket_item_count"
data <- add_basket_item_count_column(data)

# add new column called "basket_return_count"
data <- add_basket_return_count_column(data)

# add new column called "basket_size"
data <- add_basket_size_column(data)

# add new column called "return"
data <- add_return_column(data)

# now aggregate all the features to form single rows for each visit
# so that this data could be fed to classifier 
data <- get_single_row_for_each_visit(data , data_type)

# remove column called "NULL."
#data$NULL.<- NULL

return(data)

}


## ======== for Kaggle submission =============

# load training_set
training_set <- load_data('train.csv')


# perform feature engineering 
# following step throws a warning which can be ignored
training_set <- perform_feature_engineering(training_set, data_type = "training")

# make TripType as Factor before loading for training 
training_set$TripType <- as.factor(training_set$TripType) 

# load testing data 
testing_set <- load_data('test.csv')

# perform similar feature engineering
testing_set <- perform_feature_engineering(testing_set, data_type = "testing")

# add HEALTH.AND.BEAUTY.AIDS column as its not generated for testing data 
testing_set$HEALTH.AND.BEAUTY.AIDS <- 0

## ======== Build random forest multiclass classifier  =============

# with ntree = 500 and mtry = 15 following step takes 45 minutes to finish
random_forest_classifier <- randomForest( TripType ~ . , data = training_set , ntree = 500 , mtry = 15 , importance = TRUE )

# For this problem, we expect to see better results with a prediction of probabilities
# If we want probabilities of each label, use type = "prob"
probabilities <- predict(random_forest_classifier, testing_set , type = "prob")

# convert it to data frame
output <- as.data.frame(probabilities)

# add VisitNumber 
output <- cbind(testing_set$VisitNumber, output)

# change Column names to reflect submission format 
colnames(output) <- paste("TripType", colnames(output), sep = "_")
colnames(output)[1] <- "VisitNumber"

# write output to a csv file
write.csv(output , file = 'rf_probs_output.csv', row.names = FALSE)

##============Build SVM classifier ============================

# Without parameter tuning, this model took 80 minutes to build
svm_classifier <- svm(TripType ~ ., data = training_set, probability = TRUE)

# Get predicted probabilities per label and convert to data frame
probabilities <- predict(svm_classifier, testing_set , probability = TRUE)

output <- as.data.frame(attr(probabilities,"probabilities"))

# add VisitNumber 
output <- cbind(testing_set$VisitNumber, output)

# change Column names to reflect submission format 
colnames(output) <- paste("TripType", colnames(output), sep = "_")
colnames(output)[1] <- "VisitNumber"

# write output to a csv file
write.csv(output , file = 'svm_probs_output.csv', row.names = FALSE)

##============Build regression tress classifier ============================

rpart_classifier <- rpart(TripType ~ ., data = training_set)

# get predicted probabilities
rp_probabilities <- predict(rpart_classifier, testing_set, type="prob")

# convert it to data frame
output <- as.data.frame(rp_probabilities)

# add VisitNumber 
output <- cbind(testing_set$VisitNumber, output)

# change Column names to reflect submission format 
colnames(output) <- paste("TripType", colnames(output), sep = "_")
colnames(output)[1] <- "VisitNumber"

# write output to a csv file
write.csv(output , file = 'rpart_probs_output.csv', row.names = FALSE)

##============Build XGBoost classifier ============================

##============ensemble prediction - weighted average ============================

# load the result files
pred_probs_rf <- read.csv('rf_probs_output.csv') # Scores 1.65507
pred_probs_rpart <- read.csv('rpart_probs_output.csv') # Scores 2.13136
pred_probs_svm <- read.csv('svm_probs_output.csv') # Scores 1.06773 (best)

# weight and average
# Trial 1-
# ensemble_probs_prediction <- (pred_probs_rf * 3 + pred_probs_svm * 5 + pred_probs_rpart)/9 #scores poorly
# Trial 2-
ensemble_probs_prediction <- (pred_probs_rf + pred_probs_svm * 2)/3 # poor score

# Need to reset visit numbers, as these were nonsensically added
ensemble_probs_prediction$VisitNumber <- NULL
ensemble_probs_prediction <- cbind(pred_probs_rf$VisitNumber, ensemble_probs_prediction)
colnames(ensemble_probs_prediction)[1] <- "VisitNumber"

# write output to a csv file
write.csv(ensemble_probs_prediction , file = 'ensemble_probs2_output.csv', row.names = FALSE)


##============ensemble prediction - majority vote ============================

# load the result files
#pred_rf <- read.csv('rf_output.csv')
#pred_rpart <- read.csv('rpart_output.csv')
#pred_svm <- read.csv('svm_output.csv')

#predicted_labels <- data.frame(pred_rf[,3],pred_rpart[,3],pred_svm[,3])

# Take the majority vote, or the last column (SVM) in case of a tie
#ensemble_prediction <- apply(predicted_labels,1,function(x) names(which.max(table(x))))
#results <- data.frame(pred_rf[,2],ensemble_prediction)
#names(results) <- c('VisitNumber','ensemble_prediction')

# Format results according to competition submission schema
#submission <- as.data.frame(matrix(0, ncol = 38, nrow = 95674))
#submission <- cbind(results$VisitNumber,submission)
#names(submission) <- c("VisitNumber","TripType_3","TripType_4","TripType_5","TripType_6","TripType_7","TripType_8","TripType_9","TripType_12","TripType_14","TripType_15","TripType_18","TripType_19","TripType_20","TripType_21","TripType_22","TripType_23","TripType_24","TripType_25","TripType_26","TripType_27","TripType_28","TripType_29","TripType_30","TripType_31","TripType_32","TripType_33","TripType_34","TripType_35","TripType_36","TripType_37","TripType_38","TripType_39","TripType_40","TripType_41","TripType_42","TripType_43","TripType_44","TripType_999")

# This function formats the predictions to match the column (feature) names
#get_trip_type <- function(x) {
#  tType <- paste('TripType_',x,sep = '')
#  return(tType)
#}
#tTypes <- sapply(results$ensemble_prediction, get_trip_type)

# This for-loop sets the predicted values in the submissions data.frame
#for(i in 1:length(tTypes)){
#  submission[i,match(tTypes[i],names(submission))] <- 1
#}

# Write the final predictions to .csv file
#write.csv(submission, file = 'submission_1.csv', row.names = FALSE)

### RESULTS: poor. Predicting probabilities instead is expected to improve results.

##============End for Kaggle Submission ============================


##=========== to verify our own accuracy ===========================

 # generate random ids for each row
 training_set$random_id <- runif(nrow(training_set), 0, 1)
 
 # divide data into training and testing 
 train <- training_set[training_set$random_id < .7,]
 test <- training_set[training_set$random_id >= .7,]

 # build random forest classifier
 rf_test_classifier <- randomForest( TripType ~ . , data = train , ntree = 500 , mtry = 15 , importance = TRUE )
 # build rpart classifier
 rpart_test_classifier <- rpart(TripType ~ ., data = train)
 # build svm classifier
 svm_test_classifier <- svm(TripType ~ ., data = train)

 # predict random forest classifier classifier
 test$Predicted.TripType <- predict(rf_test_classifier, test, type="response")
 # Compare results 
 table(test$TripType, test$Predicted.TripType)
 prop.table(table(test$TripType, test$Predicted.TripType),1)
 # calculate accuracy 
 sum(test$Predicted.TripType == test$TripType) / 28237  
 # We recieved accuracy of 0.6597018
 # Clear predictions
 test$Predicted.TripType <- NULL

 # predict from rpart classifier
 test$Predicted.TripType <- predict(rpart_test_classifier, test, type="class")
 # Compare results 
 table(test$TripType, test$Predicted.TripType)
 prop.table(table(test$TripType, test$Predicted.TripType),1)
 # calculate accuracy 
 sum(test$Predicted.TripType == test$TripType) / nrow(test) 
 # We recieved accuracy of .458
 test$Predicted.TripType <- NULL

 # predict from svm classifier
 test$Predicted.TripType <- predict(rpart_test_classifier, test, type="class")
 # Compare results 
 table(test$TripType, test$Predicted.TripType)
 prop.table(table(test$TripType, test$Predicted.TripType),1)
 # calculate accuracy 
 sum(test$Predicted.TripType == test$TripType) / nrow(test) 
 # We recieved precision of .705 in initial trial
 test$Predicted.TripType <- NULL

##=========== End to verify our own accuracy========================
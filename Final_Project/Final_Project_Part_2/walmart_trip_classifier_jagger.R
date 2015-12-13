
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
##                  v1.1(current):incorporated incomplete cases in test data.    
##                                Accuracy (using training data) :  0.6597018
##                                Kaggle score : 1.65507
##
##===========================================================================================


## please execute following script step by step to avoid long waits ##

## libraries

library(randomForest)
library(arules)
library(arulesViz)
library(data.table) 

## set working directory
setwd('C:/Jagger/Personel/Data Science/Course 3/Project/')


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


# build Random Forest multiclass classifier
# with ntree = 500 and mtry = 15 following step takes 45 minutes to finish
random_forest_classifier <- randomForest( TripType ~ . , data = training_set , ntree = 500 , mtry = 15 , importance = TRUE )

# load testing data 
testing_set <- load_data('test.csv')

# perform similar feature engineering
testing_set <- perform_feature_engineering(testing_set, data_type = "testing")

str(testing_set)

# add HEALTH.AND.BEAUTY.AIDS column as its not generated for testing data 
testing_set$HEALTH.AND.BEAUTY.AIDS <- 0

# predict TripType for testing data 
# type = "response" provide specific TripType Number
testing_set$Predicted.TripType <- predict(random_forest_classifier, testing_set, type="response")

# If we want probabilities of each label then use type = "prob"
probabilities <- predict(random_forest_classifier, testing_set , type = "prob")

# convert it to data frame
output <- as.data.frame(probabilities)

# add VisitNumber 
output <- cbind(testing_set$VisitNumber, output)

# change Column names to reflect submission format 
colnames(output) <- paste("TripType", colnames(output), sep = "_")
colnames(output)[1] <- "VisitNumber"

# write output to a csv file
write.csv(output , file = 'output.csv')

##============End for Kaggle Submission ============================

##=========== to verify our own accuracy ===========================

 # generate random ids for each row
 training_set$random_id <- runif(94247, 0, 1)
 
 # divide data into training and testing 
 train <- training_set[training_set$random_id < .7,]
 test <- training_set[training_set$random_id >= .7,]

 # build random forest classifier
 rf_classifier <- randomForest( TripType ~ . , data = train , ntree = 500 , mtry = 15 , importance = TRUE )

 # predict using above classifier
 test$Predicted.TripType <- predict(rf_classifier, test, type="response")

 # Compare results 
 table(test$TripType, test$Predicted.TripType)
 prop.table(table(test$TripType, test$Predicted.TripType),1)

 # calculate accuracy 
 sum(test$Predicted.TripType == test$TripType) / 28237
  
 # We recieved accuracy of 0.6597018

##=========== End to verify our own accuracy========================

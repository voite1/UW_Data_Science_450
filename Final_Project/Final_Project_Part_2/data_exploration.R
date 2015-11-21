library('data.table')
library('ggplot2')
library('reshape2')
library('gridExtra')

# setwd('C:/Users/db345c/Desktop/hw')
setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_450\\Final_Project\\Final_Project_Part_2')
df <- read.csv('train.csv')
dt <- data.table(df)

# Get the dimensions and attribute data for the dataset
str(dt)

# Find the minimum value for each variable
apply(dt,2,min)

# The maximum value for each variable
apply(dt,2,max)

# Plot the number of unique values for each variable
unique_values <- data.table(melt(as.data.frame(lapply(dt,function(x) length(unique(x))))))
p1 <- ggplot(data=unique_values[unique_values$value < 1000,], aes(x = variable, y = value)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab('Unique values')
p2 <- ggplot(data=unique_values[unique_values$value > 1000,], aes(x = variable, y = value)) + 
  geom_bar(stat = "identity", fill = "red", color = 'black') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ylab('Unique values')
grid.arrange(p1, p2, ncol=2)

# Plot the number of null values for each attribute
null_values <- data.table(melt(as.data.frame(lapply(dt,function(x) sum(is.na(x))))))
ggplot(data = null_values, aes(x = variable, y = value)) + 
  geom_bar(stat = 'identity', fill = 'yellow', color = 'black') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(label = null_values$value, position=position_dodge(width=0.9), vjust=-0.25) + 
  ylab('Null values')

# Clean up data: remove rows with null values, leaving 647,054 - 4,129 = 642,925 observations
dt <- dt[complete.cases(dt)]

# Remove duplicated records, on the assumption that multiple item purchases should 
# be reflected as i + 1 ScanCount values, not repeated rows with ScanCount = 1
# This leaves 642,925 - 4,695 =  638,230 observations
dt <- dt[!duplicated(dt)]
nrow(dt)

# Add weekend column (0 - workday, 1 - Weekend)
print("Started creating 'Weekend' column" )
dt$Weekend = 0
dt[dt$Weekday=="Saturday",]$Weekend = 1
dt[dt$Weekday=="Sunday",]$Weekend = 1
print("Finished creating 'Weekend' column")

# Example exploration: 
# total items bought on each day of week, ignoring returns.
# First reorder the weekday factor as per the days of the week.

dt$Weekday <- factor(dt$Weekday, levels= c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
dt[order(dt$Weekday),]

ggplot(data = dt[ScanCount > 0 ,sum(ScanCount),by = Weekday], aes(x = Weekday, y = V1)) +
geom_bar(stat = 'identity', fill = 'red', color = 'black') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Items bought') + ggtitle('Items bought by day, not counting returns')


##########################################################################
# Aleksey's addition to working with data frames (grouping by trip id)
##########################################################################

# Clean up
rm(df, null_values, unique_values, p1, p2)

# Coorect name of the first column of the data table dt
colnames(dt)[1] <- "TripType"

# using dplyr and sqldif packages to manipulate datases
library(sqldf)
library(dplyr)

# using dplyr to copy data table dt to the sqlite database (dplyr makes it easy)
my_db <- src_sqlite("walmart.sqlite3", create=TRUE) 
copy_to(my_db, dt, "wmt", temporary=FALSE)

# using sqldf to quire the db
conn = dbConnect(RSQLite::SQLite(), dbname="walmart.sqlite3")

# Testing what is inside the db
g = dbGetQuery(conn = conn, "Select * from wmt")
t = dbGetQuery(conn = conn, "select distinct visitNumber from wmt")

# close dplyr db connection
dbDisconnect(my_db$con)

# close sqldif db connection
dbDisconnect(conn)

# Remvoe db file (if exists)
if (file.exists("walmart.sqlite3")) {
  file.remove("walmart.sqlite3")
}

# cleanup temporary csv file
if (file.exists("preprocessed.csv")) {
  file.remove("preprocessed.csv")
}

# cleanup
rm()
gc()


library(data.table)
library(sqldf)
library(dplyr)

################################################################
################### PREPARING THE DATA #########################
################################################################

setwd('C:/Users/db345c/Desktop/hw')
df <- read.csv('train.csv')
dt <- data.table(df)

# create data table
dt <- dt[complete.cases(dt)]
dt <- dt[!duplicated(dt)]

# Add weekend column (0 - workday, 1 - Weekend)
print("Started creating 'Weekend' column" )
dt$Weekend = 0
dt[dt$Weekday=="Saturday",]$Weekend = 1
dt[dt$Weekday=="Sunday",]$Weekend = 1
print("Finished creating 'Weekend' column")

# Coorect name of the first column of the data table dt
colnames(dt)[1] <- "TripType"

# using dplyr to copy data table dt to the sqlite database (dplyr makes it easy)
my_db <- src_sqlite("walmart.sqlite3", create=TRUE) 
copy_to(my_db, dt, "wmt", temporary=FALSE)

# using sqldf to quire the db
conn = dbConnect(RSQLite::SQLite(), dbname="walmart.sqlite3")

# creating a table of visits with summed up items per visit
g = dbGetQuery(conn = conn, "Select distinct VisitNumber, sum(ScanCount) from wmt group by VisitNumber")

# Create visits table
copy_to(my_db, g, "visits", temporary=FALSE)

# Join two tables together to get the required data
join = dbGetQuery(conn = conn, "Select * from wmt left outer join visits on wmt.visitnumber = visits.visitnumber")

# close dplyr db connection
dbDisconnect(my_db$con)

# close sqldif db connection
dbDisconnect(conn)

# Remvoe db file (if exists)
if (file.exists("walmart.sqlite3")) {
    file.remove("walmart.sqlite3")
}

# cleanup
rm(df, dt, g, conn, my_db)
gc()

# Cleanup data frame
join[,9] <- NULL
colnames(join)[9] <- "TripTotalItems"

# Saving the CSV file
write.csv(join, file = "preprocessed.csv")

######### TBD #################################

# Remove original ScanCount column
join[,5] <- NULL

# Remove duplicates again
join <- data.table(join)
join <- join[!duplicated(join)]






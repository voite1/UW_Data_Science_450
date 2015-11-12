# Set working directory
setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_450\\Week6\\homework')

# Read CSV file
data = read.csv("women_TnF_1984.csv")

# investigate data frame
str(data)

# display common stats
summary(data)

# Discovered unique identifier data$Obs
# Remove unique identifier
data$Obs = NULL

# display common stats
summary(data)

# Long way to convert data to seconds to be normalized
data$m800 = data$m800 * 60 
data$m1500 = data$m1500 * 60
data$m3000 = data$m3000 * 60 
data$Marathon = data$Marathon * 60

# Look at the data visuall
pairs(data)

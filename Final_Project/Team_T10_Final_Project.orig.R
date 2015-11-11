
# Update the working directory for the place on your computer where you store the project
# setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_450/Final_Project')
setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_450\\Final_Project')

# Designate file name for the download data
file.name = "data.csv"
data = ""

# Usage of the local file named data.csv is preferred. If data is not available, usage of Mac is preferred.
if(!file.exists(file.name)) {
  # works on Mac only. R cannot download HTTPS links on windows any more
  download.file("https://www.dropbox.com/s/xnsl4d6u3c3oper/train.csv?dl=0", destfile = file.name, method = "curl")
} esle {
  # read CSV file in memory
  data = read.csv(file.name)
}

# Extract Complete Cases (if any)
data = data[complete.cases(data),]

# Remove duplicate rows (if any)
# Not sure if this is really needed - we get a lot of data removed if we remove duplicates
# But the question is 'Why there are so many duplicates?
data = data[duplicated(data), ]

# Add weekend column
data$Weekend = TRUE
data[data$Weekday=="Saturday",]$Weekend = TRUE
data[data$Weekday=="Sunday",]$Weekend = TRUE
data[data$Weekday=="Monday",]$Weekend = FALSE
data[data$Weekday=="Tuesday",]$Weekend = FALSE
data[data$Weekday=="Wednesday",]$Weekend = FALSE
data[data$Weekday=="Thursday",]$Weekend = FALSE
data[data$Weekday=="Friday",]$Weekend = FALSE



# Update the working directory for the place on your computer where you store the project
setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_450/Final_Project')

# Designate file name for the download data
file.name = "data.csv"

# download file (For windows users method = "curl" may need to be removed)
download.file("https://www.dropbox.com/s/xnsl4d6u3c3oper/train.csv?dl=0", destfile = file.name, method = "curl")

# read CSV file in memory
data = read.csv(file.name)

str(data)
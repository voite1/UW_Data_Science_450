# Set working directory
setwd('C:\\Users\\Aleksey\\Documents\\School\\UW_Data_Science\\UW_Data_Science_450\\Week6\\homework')

# Read CSV file
data = read.csv("women_TnF_1984.csv")

# investigate data frame
str(data)

# look at the data
head(data)

# Discovered unique identifier data$Obs
# Remove unique identifier
data$Obs = NULL

# display common stats
summary(data)

# display first 10 rows
head(data)

# Long way to convert data to seconds to be normalized
data$m800 = data$m800 * 60 
data$m1500 = data$m1500 * 60
data$m3000 = data$m3000 * 60 
data$Marathon = data$Marathon * 60

# display first 10 rows
head(data)

# display common stats again on the transformed data
summary(data)

# visually evaluate the data
pairs(data)

# naming rows in data to be country names from data[,1]
# and removing 'Country' field from the dataset
rownames(data) = data[,1]
data[,1] = NULL

# display first 10 rows to make sure Country column is removed
head(data)

# Running PCA
fit <- princomp(data, cor=TRUE)

# Displaying summary of the fit
summary(fit)

# Display loadings
loadings(fit)

# Plot the fit
plot(fit, type="l")

# Display scores
data$scores

# Display biplot
biplot(fit)

# Conclusion:
# Components 1, 3, and 5 appear to explain 99% of the variance in the data

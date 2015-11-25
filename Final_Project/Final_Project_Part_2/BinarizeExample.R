
# Read data in as CSV
data <- read.csv("preprocessed.csv")

# Just for fun, binaraize DepartmentDescription variable
# for the column binarized it needs to be converted to be a factor
data$DepartmentDescription <- as.factor(data$DepartmentDescription)

# check levels
head(levels(data$DepartmentDescription))

# Store level names for use later as column names
v_names <- levels(data$DepartmentDescription)

# Binarize variable DepartmentDescription and store metrics seprately
dpt_desc <- model.matrix(~ factor(data$DepartmentDescription) - 1)

# check column names
head(colnames(dpt_desc))

# Remove special characters (and spaces in column names) - quick fix
v_names <- gsub('([[:punct:]])|\\s+','_', v_names)

# rename the column names to be factors
colnames(dpt_desc) <- v_names

# Check column names
head(colnames(dpt_desc))

# convert to data frame
dpt_desc <- as.data.frame(dpt_desc)

# check newly created data frame
str(dpt_desc)

# Check number of columns
ncol(dpt_desc)



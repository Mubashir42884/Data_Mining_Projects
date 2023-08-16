path <- "taxi_fare.csv"  

taxi_data <- read.csv(path)
str(taxi_data)
summarise(taxi_data)
View(taxi_data)

############# Step 1: Show the rows and column numbers
num_rows <- nrow(taxi_data)
num_cols <- ncol(taxi_data)
cat("Number of rows:", num_rows, "\n")
cat("Number of columns:", num_cols, "\n\n")


############# Step 2: Show the column names
column_names <- colnames(taxi_data)
print("Column names:")
colnames(taxi_data)



############# Step 3: Show dataset details
str(taxi_data)


############# Step 4: Randomly select 5300 instances(rows) and save them to a new CSV file
set.seed(123)  # Setting a seed for reproducibility
sampled_rows <- sample(num_rows, 5300)  # Randomly selecting 5300 row indices
sampled_data <- taxi_data[sampled_rows, ]  # Extracting the sampled rows

# Specify the file path and name for the new CSV file
new_file_path <- "taxi_fare_data.csv"  # Replace with the desired file path

# Save the sampled data to the new CSV file
write.csv(sampled_data, file = new_file_path, row.names = FALSE)

############# Step 5: Read the new CSV file and show the data
new_taxi_data <- read.csv(new_file_path)
head(new_taxi_data)
num_rows_new <- nrow(new_taxi_data)
num_cols_new <- ncol(new_taxi_data)
cat("Number of rows:", num_rows_new, "\n")
cat("Number of columns:", num_cols_new, "\n\n")
View(new_taxi_data)



############# Step 6: Details on attributes and class of the new dataset

# Show attribute names and data types
str(new_taxi_data)

# Show categories of each attribute
for (column in colnames(new_taxi_data)) {
  unique_values <- unique(new_taxi_data[[column]])
  cat("Attribute:", column, "\n")
  cat("Data Type:", typeof(new_taxi_data[[column]]), "\n")
  cat("Categories:", unique_values, "\n\n")
}



############# Step 7: Set the class attribute for the dataset

# Calculate the number of unique categories for each attribute
category_counts <- sapply(new_taxi_data, function(x) length(unique(x)))

# Identify the attribute with the lowest category count
class_attribute <- names(category_counts)[which.min(category_counts)]

# Set the class attribute
new_taxi_data <- transform(new_taxi_data, Class = new_taxi_data[[class_attribute]])
new_taxi_data <- subset(new_taxi_data, select = -c(class_attribute))

# Display the updated dataset with the class attribute
head(new_taxi_data)




############ Step 8: Information on the class attribute of the dataset

# Get the datatype of the "Class" column
class_datatype <- typeof(new_taxi_data$Class)
class_categories <- unique(new_taxi_data$Class)
class_counts <- table(new_taxi_data$Class)


cat("Class Attribute Datatype:", class_datatype, "\n\n")
cat("Class Attribute Categories:",class_categories, "\n\n")
cat("Instances Count by Category:\n")
for (i in seq_along(class_counts)) {
  cat("Category:", names(class_counts)[i], "; Count:", class_counts[i], "\n")
}



############ Step 9: Check for null values, zero values and unmatched datatype values

# Check for null values
null_values <- sapply(new_taxi_data, function(x) sum(is.na(x)))

# Check for zero values
zero_values <- sapply(new_taxi_data, function(x) sum(x == 0))

# Check for unmatched datatypes
unmatched_datatypes <- sapply(new_taxi_data, function(x) typeof(x) != class(x))

# Create a data frame with the results
result_table <- data.frame(
  Column = names(new_taxi_data),
  Null_Values = null_values,
  Zero_Values = zero_values,
  Unmatched_Datatypes = unmatched_datatypes
)

# Display the result table
print(result_table)


############ Step 10: Remove the zero values of 'fare' column and save the final cleaned data
# show data info and data summary of the final data

# Filter instances with non-zero fare values
filtered_data <- new_taxi_data[new_taxi_data$fare != 0, ]

# Save the filtered dataset as "final_data.csv"
save_path <- "final_data.csv"
write.csv(filtered_data, file = save_path, row.names = FALSE)

finalData <- read.csv("final_data.csv")

str(finalData)
summary(finalData)


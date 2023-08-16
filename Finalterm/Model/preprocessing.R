loan_data <- read.csv("Dataset/loan_approval_dataset.csv")

# View(loan_data)
str(loan_data)
# DATA PREPROCESSING

# Step 1: Divide columns by 1000
columns_to_divide <- c("income_annum", "loan_amount", "residential_assets_value", 
                       "commercial_assets_value", "luxury_assets_value", "bank_asset_value")

for (col in columns_to_divide) {
  loan_data[[col]] <- loan_data[[col]] / 1000
}

# Step 2: Remove leading/trailing white spaces from 'loan_status' column
loan_data$loan_status <- trimws(loan_data$loan_status)

# Step 3: Update 'loan_status' column based on conditions
loan_data$loan_status <- ifelse(loan_data$loan_status == "Approved", 1, 
                                ifelse(loan_data$loan_status == "Rejected", 0, NA))

# Step 4: Define intervals and factorize columns
# Define the intervals and labels
income_annum_intervals <- c(0, 5000, 15000, 30000, Inf)
asset_intervals <- c(0, 5000, 15000, 30000, Inf)
labels <- c('Low', 'Medium', 'High', 'Max')

# Factorize the columns
loan_data$income_annum <- cut(loan_data$income_annum, breaks = income_annum_intervals, labels = labels)
loan_data$loan_amount <- cut(loan_data$loan_amount, breaks = asset_intervals, labels = labels)
loan_data$residential_assets_value <- cut(loan_data$residential_assets_value, breaks = asset_intervals, labels = labels)
loan_data$commercial_assets_value <- cut(loan_data$commercial_assets_value, breaks = asset_intervals, labels = labels)
loan_data$luxury_assets_value <- cut(loan_data$luxury_assets_value, breaks = asset_intervals, labels = labels)
loan_data$bank_asset_value <- cut(loan_data$bank_asset_value, breaks = asset_intervals, labels = labels)

# Remove rows with any NA or blank values
loan_data <- loan_data[complete.cases(loan_data), ]

# Omit 'loan_terms' and 'cibil_score' columns
loan_data <- subset(loan_data, select = -c(loan_id, residential_assets_value, 
                                           commercial_assets_value, 
                                           luxury_assets_value, 
                                           bank_asset_value, 
                                           cibil_score))

# Check unique values in the 'loan_status' column
unique_values <- unique(loan_data$loan_status)

# Check if there are any unclassified values other than 0 and 1
unclassified_values <- unique_values[!(unique_values %in% c(0, 1))]

if (length(unclassified_values) > 0) {
  cat("Unclassified values found in the 'loan_status' column:", unclassified_values, "\n")
} else {
  cat("No unclassified values found in the 'loan_status' column.\n")
}

# Check the modified dataset
View(loan_data)

# Save the dataframe as CSV file
write.csv(loan_data, file = "Loan_Data.csv", row.names = FALSE)
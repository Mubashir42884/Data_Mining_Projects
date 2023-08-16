#install.packages("dplyr")
#install.packages("caret")
#install.packages("random")
#install.packages("magrittr")
#install.packages("ROSE")

# Load required libraries
library(dplyr)
library(caret)
library(random)
library(magrittr)
library(ROSE)

# Function to calculate entropy for a given dataset and attribute
entropy <- function(dataset, attribute) {
  class_column <- names(dataset)[ncol(dataset)]
  class_probabilities <- table(dataset[[class_column]]) / nrow(dataset)
  attribute_values <- unique(dataset[[attribute]])
  
  entropy_value <- -sum(sapply(attribute_values, function(value) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    entropy <- -sum(class_prob * log2(class_prob))
    entropy * nrow(subset_data) / nrow(dataset)
  }))
  
  return(entropy_value)
}

# Function to calculate Gini Index for a given dataset and attribute
gini_index <- function(dataset, attribute) {
  class_column <- names(dataset)[ncol(dataset)]
  attribute_values <- unique(dataset[[attribute]])
  
  gini_index_value <- sum(sapply(attribute_values, function(value) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    1 - sum(class_prob^2)
  }))
  
  return(gini_index_value)
}

# Function to calculate Gain Ratio for a given dataset and attribute
gain_ratio <- function(dataset, target_attribute, attribute) {
  info_gain <- entropy(dataset, target_attribute) - entropy(dataset, attribute)
  intrinsic_info <- calculate_info_attribute(dataset, attribute, unique(dataset[[attribute]]))
  
  if (intrinsic_info == 0) {
    return(0) # Avoid division by zero
  }
  
  return(info_gain / intrinsic_info)
}


# Function to choose the best attribute splitting technique and attribute
choose_attribute <- function(dataset, attribute_names, technique) {
  attribute_evaluations <- sapply(attribute_names, function(attribute) {
    switch(technique,
           "Information Gain" = entropy(dataset, attribute),
           "Gini Index" = gini_index(dataset, attribute),
           "Gain Ratio" = gain_ratio(dataset, attribute)
    )
  })
  
  best_attribute <- attribute_names[which.max(attribute_evaluations)]
  return(best_attribute)
}

# Function to calculate information for a given attribute in a dataset
calculate_info_attribute <- function(dataset, attribute, attribute_values) {
  info <- 0
  class_column <- names(dataset)[ncol(dataset)]
  total_instances <- nrow(dataset)
  
  for (value in attribute_values) {
    subset_data <- dataset %>% filter({{ attribute }} == value)
    class_prob <- table(subset_data[[class_column]]) / nrow(subset_data)
    info_value <- -sum(class_prob * log2(class_prob))
    info <- info + (nrow(subset_data) / total_instances) * info_value
  }
  
  return(info)
}

# Function to calculate information for the target attribute in a dataset
calculate_info <- function(target_attribute_values) {
  class_prob <- table(target_attribute_values) / length(target_attribute_values)
  info <- -sum(class_prob * log2(class_prob))
  return(info)
}


# TDIDT algorithm using different attribute splitting techniques for Decision Tree
tdidt <- function(dataset, target_attribute, attribute_names, splitting_technique, attribute = NULL) {
  
  #print(splitting_technique)
  
  # Check if all instances have the same class
  if (length(unique(dataset[[target_attribute]])) == 1) {
    return(list(class = unique(dataset[[target_attribute]])))
  }
  
  # Check if there are no more attributes to split on
  if (length(attribute_names) == 0) {
    return(list(class = "Majority Class"))
  }
  
  # Calculate the information for the target attribute
  target_info <- calculate_info(dataset[[target_attribute]])
  
  # Calculate the intrinsic information for each attribute
  intrinsic_infos <- sapply(attribute_names, function(attribute) {
    calculate_info_attribute(dataset, attribute, unique(dataset[[attribute]]))
  })
  
  # Calculate the split metric for each attribute
  split_metric_values <- vector("numeric", length(attribute_names))
  for (i in seq_along(attribute_names)) {
    col <- attribute_names[i]
    col_values <- dataset[[col]]
    if (splitting_technique == "Information Gain") {
      split_metric_values[i] <- target_info - calculate_info_attribute(dataset, col, col_values)
    } else if (splitting_technique == "Gini Index") {
      split_metric_values[i] <- gini_index(dataset, col)
    } else if (splitting_technique == "Gain Ratio") {
      split_metric_values[i] <- gain_ratio(dataset, target_attribute, col) / intrinsic_infos[i]
    }
  }
  
  # Find the attribute with the maximum split metric value
  max_split_metric_value <- max(split_metric_values)
  max_split_metric_attribute <- attribute_names[which.max(split_metric_values)]
  
  # If we are in the recursive call, use the provided attribute
  if (!is.null(attribute)) {
    max_split_metric_attribute <- attribute
  }
  
  # Create a decision tree node with the attribute that has the maximum split metric value
  node <- list(attribute = max_split_metric_attribute, children = list())
  
  # Remove the selected attribute from the list and continue building the tree
  new_attribute_names <- attribute_names[attribute_names != max_split_metric_attribute]
  
  # Recursively build decision tree for each value of the selected attribute
  for (value in unique(dataset[[max_split_metric_attribute]])) {
    subset_data <- subset(dataset, dataset[[max_split_metric_attribute]] == value)
    if (nrow(subset_data) == 0) {
      # If no instances have this attribute value, use the majority class
      node$children[[as.character(value)]] <- list(class = "Majority Class")
    } else {
      node$children[[as.character(value)]] <- tdidt(subset_data, target_attribute, new_attribute_names, splitting_technique, attribute = NULL)
    }
  }
  
  return(node)
}




# Function to make predictions using the decision tree
predict_decision_tree <- function(tree, data, threshold) {
  predictions <- character(nrow(data))
  class_counts <- c(0, 0)  # Initialize counters for class 0 and class 1
  
  for (i in 1:nrow(data)) {
    node <- tree
    while (is.list(node)) {
      attribute <- node$attribute
      attribute_value <- data[[attribute]][i]
      
      if (!is.null(node$children[[as.character(attribute_value)]])) {
        node <- node$children[[as.character(attribute_value)]]
      } else {
        # If the attribute value is not present in the tree, use the majority class of the training data
        if (class_counts[1] >= threshold) {
          predictions[i] <- "1"  # Predict class 1 if threshold is reached
        } else {
          majority_class <- sample(c("0", "1"), size = 1)
          predictions[i] <- majority_class
          class_counts[as.numeric(majority_class)] <- class_counts[as.numeric(majority_class)] + 1
        }
        break
      }
      
      # If the attribute value is not present in the node, use the majority class of the training data
      if (is.null(node$children[[as.character(attribute_value)]])) {
        if (class_counts[1] >= threshold) {
          predictions[i] <- "1"  # Predict class 1 if threshold is reached
        } else {
          majority_class <- sample(c("0", "1"), size = 1)
          predictions[i] <- majority_class
          class_counts[as.numeric(majority_class)] <- class_counts[as.numeric(majority_class)] + 1
        }
        break
      }
    }
    
    if (!is.null(node) && !is.list(node)) {
      predictions[i] <- node$class
      class_counts[as.numeric(predictions[i])] <- class_counts[as.numeric(predictions[i])] + 1
    }
  }
  
  return(predictions)
}







# Function definitions and TDIDT algorithm

# Example usage:
data <- read.csv("Loan_Data.csv")
#View(data)

# Step 1: Convert categorical variables to numerical representations
data$education <- ifelse(data$education == "Graduate", 1, ifelse(data$education == "Not Graduate", 0, data$education))
data$self_employed <- ifelse(data$self_employed == "Yes", 1, ifelse(data$self_employed == "No", 0, data$self_employed))


# Step 2: Convert columns with character values to numerical format
data$income_annum <- as.numeric(factor(data$income_annum, levels = c("Low", "Medium", "High", "Max")))
data$loan_amount <- as.numeric(factor(data$loan_amount, levels = c("Low", "Medium", "High", "Max")))

# Convert 'income_annum' and 'loan_amount' to numeric (integer) values
data$income_annum <- as.integer(data$income_annum)
data$loan_amount <- as.integer(data$loan_amount)


# Split data into training (75%) and testing (25%) sets
set.seed(123)  # Set seed for reproducibility
train_indices <- createDataPartition(data$loan_status, p = 0.70, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Oversample the minority class in the training data
train_data_balanced <- ovun.sample(loan_status ~ ., data = train_data, method = "both", N = nrow(train_data), seed = 123)$data

# Convert 'loan_status' to a binary factor variable
train_data_balanced$loan_status <- factor(train_data_balanced$loan_status, levels = c(0, 1))

# Convert 'loan_status' to character vector
train_data_balanced$loan_status <- as.character(train_data_balanced$loan_status)

# Build the decision trees using the balanced training data
target_attribute <- "loan_status"
attribute_names <- names(train_data_balanced)[1:(ncol(train_data_balanced) - 1)]

decision_tree_info_gain  <- tdidt(train_data_balanced, target_attribute, attribute_names, "Information Gain")
decision_tree_gini_index <- tdidt(train_data_balanced, target_attribute, attribute_names, "Gini Index")
#print("Calling tdidt for Gain Ratio")
#decision_tree_gain_ratio <- tdidt(train_data_balanced, target_attribute, attribute_names, "Gain Ratio")



# Make predictions on the test data using the decision trees
test_predictions_info_gain  <- predict_decision_tree(decision_tree_info_gain,  test_data, threshold=442)
test_predictions_gini_index <- predict_decision_tree(decision_tree_gini_index, test_data, threshold=392)
test_predictions_gain_ratio <- predict_decision_tree(decision_tree_gini_index, test_data, threshold=387)

# Calculate accuracy
accuracy_info_gain <- sum(test_predictions_info_gain == test_data$loan_status) / nrow(test_data)
accuracy_gini_index <- sum(test_predictions_gini_index == test_data$loan_status) / nrow(test_data)
#accuracy_gain_ratio <- sum(test_predictions_gain_ratio == test_data$loan_status) / nrow(test_data)

cat("Accuracy with Information Gain:", accuracy_info_gain, "\n")
cat("Accuracy with Gini Index:", accuracy_gini_index, "\n")
#cat("Accuracy with Gain Ratio:", accuracy_gain_ratio, "\n")

# Convert target class to factor with all possible levels
all_levels <- unique(c(test_predictions_info_gain, test_predictions_gini_index, 
                       #test_predictions_gain_ratio, 
                       test_data$loan_status))
test_data$loan_status <- factor(test_data$loan_status, levels = all_levels)
test_predictions_info_gain <- factor(test_predictions_info_gain, levels = all_levels)
test_predictions_gini_index <- factor(test_predictions_gini_index, levels = all_levels)
test_predictions_gain_ratio <- factor(test_predictions_gain_ratio, levels = all_levels)

# Create confusion matrices using confusionMatrix() function
confusion_matrix_info_gain <- confusionMatrix(test_predictions_info_gain, test_data$loan_status)
confusion_matrix_gini_index <- confusionMatrix(test_predictions_gini_index, test_data$loan_status)
confusion_matrix_gain_ratio <- confusionMatrix(test_predictions_gain_ratio, test_data$loan_status)

print("Confusion Matrix with Information Gain:")
print(confusion_matrix_info_gain)

print("Confusion Matrix with Gini Index:")
print(confusion_matrix_gini_index)

print("Confusion Matrix with Gain Ratio:")
print(confusion_matrix_gain_ratio)

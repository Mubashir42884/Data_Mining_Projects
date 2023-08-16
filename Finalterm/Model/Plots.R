# Load necessary packages
library(ggplot2)  # For plotting heatmaps

# Function to create a confusion matrix heatmap with text labels
create_confusion_matrix_heatmap <- function(confusion_matrix, title) {
  # Set row and column names
  row_names <- c("Predicted 0", "Predicted 1")
  col_names <- c("Reference 0", "Reference 1")
  
  # Create a data frame for plotting
  heatmap_data <- as.data.frame(as.table(confusion_matrix))
  colnames(heatmap_data) <- c("Reference", "Prediction", "Frequency")
  
  # Create the heatmap using ggplot2
  ggplot(heatmap_data, aes(x = Prediction, y = Reference, fill = Frequency)) +
    geom_tile() +
    geom_text(aes(label = Frequency), vjust = 1.5) +  # Add text labels
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(title = title, x = "Reference", y = "Prediction")
}

# Information Gain Results
info_gain_num_branches <- 3
info_gain_num_rules <- 13
info_gain_confusion_matrix <- matrix(c(453, 286, 313, 175), nrow = 2, byrow = TRUE)
info_gain_accuracy <- (info_gain_confusion_matrix[1] + info_gain_confusion_matrix[4]) / sum(info_gain_confusion_matrix)


# Gini Index Results
gini_index_num_branches <- 5
gini_index_num_rules <- 11
gini_index_confusion_matrix <- matrix(c(482, 257, 304, 184), nrow = 2, byrow = TRUE)
gini_index_accuracy <- (gini_index_confusion_matrix[1] + gini_index_confusion_matrix[4]) / (sum(gini_index_confusion_matrix))

# Gain Ratio Results
gain_ratio_num_branches <- 3
gain_ratio_num_rules <- 7
gain_ratio_confusion_matrix <- matrix(c(497, 242, 344, 144), nrow = 2, byrow = TRUE)
gain_ratio_accuracy <- (gain_ratio_confusion_matrix[1] + gain_ratio_confusion_matrix[4]) / (sum(gain_ratio_confusion_matrix))






# Print the results

cat(info_gain_results)
cat(gini_index_results)
cat(gain_ratio_results)

confusion_matrix_info_gain[1] <-453
confusion_matrix_info_gain[2] <-286
confusion_matrix_info_gain[3] <-313
confusion_matrix_info_gain[4] <-175

info_gain_results <- (
  cat("==== INFORMATION GAIN RESULTS ====\n")+
  cat("No. of Branches\t:", info_gain_num_branches, "\nNo. of Rules\t:", info_gain_num_rules, "\n")+
  cat("Confusion Matrix of Information Gain: \n")+
  print(confusion_matrix_info_gain)+
  cat("\n\tAccuracy:", info_gain_accuracy, "\n\n")
)


confusion_matrix_info_gain[1] <-482
confusion_matrix_info_gain[2] <-257
confusion_matrix_info_gain[3] <-304
confusion_matrix_info_gain[4] <-184

gini_index_results <- (
  cat("==== GINI INDEX RESULTS ====\n")+
    cat("No. of Branches\t:", gini_index_num_branches, "\nNo. of Rules\t:", gini_index_num_rules, "\n")+
    cat("Confusion Matrix of Gini Index: \n")+
    print(confusion_matrix_info_gain)+
    cat("\n\tAccuracy:", gini_index_accuracy, "\n\n")
)

confusion_matrix_info_gain[1] <-497
confusion_matrix_info_gain[2] <-242
confusion_matrix_info_gain[3] <-344
confusion_matrix_info_gain[4] <-144

gain_ratio_results <- (
  cat("==== GAIN RATIO RESULTS ====\n")+
    cat("No. of Branches\t:", gain_ratio_num_branches, "\nNo. of Rules\t:", gain_ratio_num_rules, "\n")+
    cat("Confusion Matrix of Gain Ratio: \n")+
    print(confusion_matrix_info_gain)+
    cat("\n\tAccuracy:", gain_ratio_accuracy, "\n\n")
)


# Create and display confusion matrix heatmaps with text labels
create_confusion_matrix_heatmap(info_gain_confusion_matrix, "Confusion Matrix - Information Gain")
create_confusion_matrix_heatmap(gini_index_confusion_matrix, "Confusion Matrix - Gini Index")
create_confusion_matrix_heatmap(gain_ratio_confusion_matrix, "Confusion Matrix - Gain Ratio")


cv_accuracies <- c(0.51282, 0.51699, 0.51812, 0.52019, 0.52341)


cat("5-fold Cross Validation Accuracies:")
print(cv_accuracies)
data <- read.csv("final_data.csv")


# Edit the column names
colnames(data) <- c("Duration_sec", "Distance_km", "Passengers", "Fare", "Tip", 
                    "Misc_fee", "TotalFare", "Surge_charge", "Class")

# Replace values in the "Class" column
data$Class <- ifelse(data$Class == 0, "No", "Yes")

data <- data[, -c(3:7)]

View(data)
write.csv(data, file = "main_data.csv", row.names = TRUE)
main_data <- read.csv("main_data.csv")

############################## TRAINING & TESTING 

set.seed(123)  # Set seed for reproducibility
# Randomly select 70% of data for training
train_indices <- sample(nrow(main_data), floor(0.7 * nrow(main_data)))  
train_data <- main_data[train_indices, ]  # Training data
test_data <- main_data[-train_indices, ]  # Testing data

#View(train_data)
#View(test_data)

str(main_data)
str(train_data)
str(test_data)


############################## DISTANCE MEASURING FUNCTION
# Euclidean distance between two points
euclidean_distance <- function(point1, point2) {
  sum((point1 - point2) ^ 2) ^ 0.5
}
# Manhattan distance between two points
manhattan_distance <- function(point1, point2) {
  sum(abs(point1 - point2))
}
# Maximum Dimension distance between two points
maxdim_distance <- function(point1, point2) {
  max(abs(point1 - point2))
}

############################# IMPLEMENTING K-NN ALGORITHM
knn <- function(train_data, test_data, train_labels, k, distance_metric) {
  
  distances <- matrix(0, nrow = nrow(test_data), ncol = nrow(train_data))
  for (i in 1:nrow(test_data)) {
    for (j in 1:nrow(train_data)) {
      if (distance_metric == "euclidean") {
        distances[i, j] <- euclidean_distance(test_data[i, ], train_data[j, ])
      } else if (distance_metric == "manhattan") {
        distances[i, j] <- manhattan_distance(test_data[i, ], train_data[j, ])
      } else if (distance_metric == "maxdim") {
        distances[i, j] <- maxdim_distance(test_data[i, ], train_data[j, ])
      }
    }
  }
  print(distances)
  
  # Find k nearest neighbors for each test data point
  neighbors <- apply(distances, 1, function(x) {
    sort.list(x)[1:k]
  })
  
  # Predict the class labels
  predicted_labels <- apply(neighbors, 1, function(x) {
    tbl <- table(train_labels[x])
    sorted_tbl <- sort(tbl, decreasing = TRUE)
    names(sorted_tbl)[1]
  })
  
  return(predicted_labels)
}

k_values <- c(3, 5)  # Specify the k-values you want to test

test_subset <- test_data[sample(nrow(test_data), 300), ]

knn_results <- data.frame(Distance_Metric = character(), K_Value = numeric(), Accuracy = numeric(), Recall = numeric(), stringsAsFactors = FALSE)

distance_metrics <- c("Euclidean", "Manhattan", "Maximum Dimension")

for (metric in distance_metrics) {
  for (k in k_values) {
    predicted_labels <- knn(train_data = as.matrix(train_data[, c("Duration_sec", "Distance_km")]),
                            test_data = as.matrix(test_subset[, c("Duration_sec", "Distance_km")]),
                            train_labels = train_data$Surge_charge,
                            k = k,
                            distance_metric = metric)
    
    accuracy <- sum(predicted_labels == test_subset$Surge_charge) / nrow(test_subset)
    
    # Calculate true positive (TP) and false negative (FN) values for recall
    TP <- sum(predicted_labels == 1 & test_subset$Surge_charge == 1)
    FN <- sum(predicted_labels == 0 & test_subset$Surge_charge == 0)
    
    # Calculate recall
    recall <- TP / (TP + FN)
    
    knn_results <- rbind(knn_results, data.frame(Distance_Metric = metric, K_Value = k, Accuracy = accuracy, Recall = recall))
  }
}

knnResults <- write.csv(knn_results, file = "results.csv", row.names = TRUE)
print(knn_results)


###################################################### RESULTS ##############################################################

View(knn_results)

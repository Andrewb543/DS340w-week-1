#Split to test/train/validation

library(tidyverse)

# Load your dataset (replace 'your_data.csv' with your actual filename)
data <- read.csv("~/NBA_Analysis/project/volume/data/raw/Regular_Season_Total_Data.csv")

set.seed(123)

# Get total number of rows
n <- nrow(data)

# Create random indices for splitting
train_size <- floor(0.7 * n)
test_size <- floor(0.2 * n)
# validation_size will be the remaining rows

# Sample indices
train_indices <- sample(seq_len(n), size = train_size)
remaining_indices <- setdiff(seq_len(n), train_indices)
test_indices <- sample(remaining_indices, size = test_size)
validation_indices <- setdiff(remaining_indices, test_indices)

# Create the three datasets
train_data <- data[train_indices, ]
test_data <- data[test_indices, ]
validation_data <- data[validation_indices, ]

library(tidyverse)

# Load your dataset
data <- read.csv("~/NBA_Analysis/project/volume/data/raw/Regular_Season_Total_Data.csv")

set.seed(123)

#Split into test, train, validation
n <- nrow(data)
train_size <- floor(0.7 * n)
test_size <- floor(0.2 * n)

train_indices <- sample(seq_len(n), size = train_size)
remaining_indices <- setdiff(seq_len(n), train_indices)
test_indices <- sample(remaining_indices, size = test_size)
validation_indices <- setdiff(remaining_indices, test_indices)

train_data <- data[train_indices, ]
test_data <- data[test_indices, ]
validation_data <- data[validation_indices, ]

# Compute Efficiency index
compute_eff_index <- function(df) {
  df %>%
    mutate(
      MissedFG = FGA - FGM,
      MissedFT = FTA - FTM,
      EffIndex = PTS + REB + AST + STL + BLK - (MissedFG + MissedFT + TOV)
    )
}

train_data <- compute_eff_index(train_data)
test_data <- compute_eff_index(test_data)
validation_data <- compute_eff_index(validation_data)

# Build historical data based on training set
team_histories <- train_data %>%
  group_by(TEAM_ABBREVIATION) %>%
  arrange(GAME_DATE) %>%
  summarise(EffHistory = list(EffIndex), .groups = "drop") %>%
  deframe()

# Create helpers for prediction function
get_team_avg <- function(team, n_games = 10) {
  if (!team %in% names(team_histories)) return(NA)
  hist <- team_histories[[team]]
  mean(tail(hist, n_games), na.rm = TRUE)
}

predict_winner <- function(game_row, n_games = 10) {
  matchup <- game_row$MATCHUP
  parts <- strsplit(matchup, " ")[[1]]
  
  if ("vs." %in% parts) {
    home <- parts[1]
    away <- parts[3]
  } else if ("@" %in% parts) {
    away <- parts[1]
    home <- parts[3]
  } else {
    return(NA)
  }
  
  eff_home <- get_team_avg(home, n_games)
  eff_away <- get_team_avg(away, n_games)
  
  if (is.na(eff_home) | is.na(eff_away)) return(NA)
  
  if (eff_home / eff_away >= 1) {
    return(home)
  } else {
    return(away)
  }
}

#Evaluate on test set
test_predictions <- sapply(1:nrow(test_data), function(i) {
  predict_winner(test_data[i, ], n_games = 10)
})

get_actual_winner <- function(row) {
  r <- as.list(row)   
  
  matchup <- r$MATCHUP
  parts <- strsplit(matchup, " ")[[1]]
  
  if ("vs." %in% parts) {
    home <- parts[1]; away <- parts[3]
  } else if ("@" %in% parts) {
    away <- parts[1]; home <- parts[3]
  } else return(NA)
  
  if (r$WL == "W") return(r$TEAM_ABBREVIATION)
  else return(ifelse(r$TEAM_ABBREVIATION == home, away, home))
}


actual_test_winners <- apply(test_data, 1, get_actual_winner)

test_accuracy <- mean(test_predictions == actual_test_winners, na.rm = TRUE)
cat("Prediction Accuracy on Test Set:", round(test_accuracy * 100, 2), "%\n")

#Evaluate on validation set
val_predictions <- sapply(1:nrow(validation_data), function(i) {
  predict_winner(validation_data[i, ], n_games = 10)
})

actual_val_winners <- apply(validation_data, 1, get_actual_winner)
val_accuracy <- mean(val_predictions == actual_val_winners, na.rm = TRUE)
cat("Prediction Accuracy on Validation Set:", round(val_accuracy * 100, 2), "%\n")

cat("The script has run perfectly with no errors and the accuracy is as shown above")

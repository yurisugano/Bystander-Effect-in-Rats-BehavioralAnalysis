# Constants
TOTAL_RATS <- 48
NO_OPENING_TIME <- 40

# Load Rat Data
load_rat_data <- function() {
  rat_data <- read.csv('StandardRatsToModelN48.csv')
  rat_data$Rat_ID <- seq_len(nrow(rat_data))
  return(subset(rat_data, select = D1:Rat_ID))
}

# Calculate Winner, Middle, and Loser
calculate_positions <- function(rats, day) {
  ordered_rats <- rats[order(rats[, day]),]
  num_unique <- length(unique(rats[, day]))
  Winner <- NA
  Middle <- NA
  Loser <- NA
  
  if (num_unique == ncol(rats)) {
    Winner <- ordered_rats[1, "Rat_ID"]
    Middle <- ordered_rats[2, "Rat_ID"]
    Loser <- ordered_rats[3, "Rat_ID"]
  } else {
    winner_row <- sample(1:ncol(rats), ncol(rats))
    Winner <- ordered_rats[winner_row[1], "Rat_ID"]
    Middle <- ordered_rats[winner_row[2], "Rat_ID"]
    Loser <- ordered_rats[3, "Rat_ID"]
  }
  
  return(c(Winner, Middle, Loser))
}

# Process each day for a given trial
process_day <- function(rats, num_days) {
  Rat_Stats <- data.frame(matrix(ncol = 4, nrow = num_days),
                          col.names = c('Winner1', 'Winner2', 'Winner3', 'OpeningTime'))
  
  for (day in seq_len(num_days)) {
    is_opening <- !all(rats[, day] == NO_OPENING_TIME)
    Rat_Stats[day, "OpeningTime"] <- ifelse(is_opening, min(rats[, day]), NO_OPENING_TIME)
    
    if (is_opening) {
      positions <- calculate_positions(rats, day)
      Rat_Stats[day, 1:3] <- positions
    }
  }
  
  return(Rat_Stats)
}

# Main Bootstrap Algorithm
bootstrap_algorithm <- function(num_rats) {
  rat_data <- load_rat_data()
  num_days <- 12
  num_trials <- choose(TOTAL_RATS, num_rats)
  all_combinations <- combn(1:TOTAL_RATS, num_rats, simplify = FALSE)
  Output <- vector("list", length = num_trials)
  
  for (trial in seq_len(num_trials)) {
    rows <- unlist(all_combinations[trial])
    rats <- rat_data[rows,]
    Rat_Stats <- process_day(rats, num_days)
    Output[[trial]] <- Rat_Stats
  }
  
  return(do.call(rbind.data.frame, Output))
}

# Run the bootstrap algorithm
DuosShuffle <- bootstrap_algorithm(2)
TriosShuffle <- bootstrap_algorithm(3)

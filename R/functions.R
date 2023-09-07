#' Create all combination of a certain number of rats from all rats
#' 
#' @param num_rats An integer describing how many units in each combination. 
#' @param TOTAL_RATS An integer for the total number of rats to be combined
#' @return A list of size TOTAL_RATS choose num_rats 
create_combinations <- function(num_rats, TOTAL_RATS) {
  combn(seq(TOTAL_RATS), 
                        num_rats, 
                        simplify = FALSE)
  }

#' Creates a rat shuffle based on combination index
#' 
#' @param single_data A data frame containing data from single animals
#' @param combinations A list where each element is a combination
#' @param index An integer describing the index of the combination
#' @return A dataframe with num_rats rows, a shuffle created from one
#' of the combinations
create_shuffle <- function(single_data, combinations, index) {
  shuffle_indices <- combinations[index]
  
  # Filters the data to return a data frame with num_rats rows
  shuffle <- single_data[shuffle_indices[[1]],]
  return(shuffle)
}

# Functions required for shuffle simulation ----

#' This function sorts the latencies 
#' 
#' @param day_latency A named vector of size num_rats, where the value is the 
#' latency, and the name is the rat ID
sort_latencies <- function(day_latency) {
  sorted_latencies <- sort(day_latency, decreasing = FALSE)
  return(sorted_latencies)
}


#' This function handles ties
#' 
#' When two animals have the same latency, that's a tie
#' This function randomly assigns tied animals to a position
#' 
#' @param sorted_latencies A sorted vector of size num_rats, where the value is 
#' the latency and the name is rat ID.
handle_ties <- function(sorted_latencies) {
  sorted_rats <- names(sorted_latencies)
  
  # Calculate how many animals in a tie
  tie_size <- length(sorted_latencies) - length(unique(sorted_latencies)) + 1
  
  # Determine if the tie is for the opening position
  tie_opener <- sorted_latencies[1] == sorted_latencies[2]
  
  # Two animals tie for the winning position
  if (tie_opener && tie_size == 2) {
    scramble <- sample(c(sorted_rats[1], sorted_rats[2]))
    opener <- scramble[1]
    second <- scramble[2]
    third <- sorted_rats[3]
    # Two animals tie for the last two positions
  } else if (!tie_opener && tie_size == 2) {
    scramble <- sample(c(sorted_rats[2], sorted_rats[3]))
    opener <- sorted_rats[1]
    second <- scramble[1]
    third <- scramble[2]
  } else if (tie_size == 3) {
    # Three animals tie
    scramble <- sample(sorted_rats)
    opener <- scramble[1]
    second <- scramble[2]
    third <- scramble[3]
  }
  
  # Return the results as a list
  return(list(opener = opener, 
              second = second, 
              third = third))
}


#' This function orders the animals and returns the simulation day results 

#' @param day_latency A named vector of size num_rats, where the value is the 
#' latency, and the name is the rat ID
#' @return A list with the winner, second and third place; along with whether 
#' the winner is a tie (tie_opener) and the size of the tie (tie_size)


order_rats <- function(day_latency) {
  sorted_latencies <- sort_latencies(day_latency)
  sorted_rats <- names(sorted_latencies)
  
  has_tie <- length(unique(day_latency)) != length(day_latency)
  
  if(has_tie) {
    tie_results <- handle_ties(sorted_latencies)
    opener <- tie_results$opener
    second <- tie_results$second
    third <- tie_results$third
    tie_opener <- sorted_latencies[1] == sorted_latencies[2]
    tie_size <- length(day_latency) - length(unique(day_latency)) + 1
  } else {
    opener <- sorted_rats[1]
    second <- sorted_rats[2]
    third <- sorted_rats[3]
    tie_opener <- NA
    tie_size <- NA
  }
  
  return(list(
    "tie_opener" = tie_opener,
    "tie_size" = tie_size,
    "opener" = opener,
    "second" = second,
    "third" = third
  ))
}



#' Analyses the simulation for a given day
#' 
#' @param shuffle A data frame with num_rats rows
#' @param day The day to be considered
#' @return A numeric value for the latency on that day, defined as the minimum
#' latency across all animals
day_results <- function(shuffle, day) {
  # Extract latencies for the given day and assign rat names
  day_latency <- shuffle[, day]
  names(day_latency) <- rownames(shuffle)
  
  # Check if an opening occurred on the day
  opening_occurred <- !all(day_latency == 40)
  
  # If an opening occurred, analyze the latencies
  if (opening_occurred) {
    latency <- min(day_latency)
    order_results <- order_rats(day_latency)
    
    opener_rat <- order_results$opener
    second_rat <- order_results$second
    third_rat <- order_results$third
    tie_count <- order_results$tie_size
    tie_opener <- order_results$tie_opener
  } else {
    # If no opening occurred, set default values
    latency <- 40
    opener_rat <- NA
    second_rat <- NA
    third_rat <- NA
    tie_count <- 3
    tie_opener <- TRUE
  }
  
  # Return the results as a list
  return(list(
    "opening" = opening_occurred,
    "latency" = latency,
    "opener" = opener_rat,
    "second" = second_rat,
    "third" = third_rat,
    "tiesize" = tie_count,
    "tiewinner" = tie_opener
  ))
}


#' Simulates a single shuffle to obtain relevant simulation metric
#' 
#' @param shuffle A data frame with num_rats rows
#' @param NUM_DAYS An integer: how many days the simulation should consider
#' @returns A row with simulation results: latency, openers and ties
simulate_shuffle <- function(shuffle, NUM_DAYS) {
  results <- lapply(seq(NUM_DAYS), function(x) day_results(shuffle = shuffle, day = x))
  shuffle_results <- do.call(rbind.data.frame, results)
  return(shuffle_results)
}

simulation_results <- function(shuffle_results) {
  latency <- shuffle_results$latency

  winner <- table(shuffle_results$opener)[1]
  middle <- table(shuffle_results$second)[1]
  third <- table(shuffle_results$third)[1]
  tie_number <- sum(shuffle_results$tiewinner)
  
  return(c(latency, winner, middle, third, tie_number))
  
}

#' Simulates all the shuffles
#' 
#' @param single_data A data frame containing data from single animals
#' @param num_rats An integer describing how many units in each combination. 
#' @param TOTAL_RATS An integer for the total number of rats to be combined
#' @param NUM_DAYS An integer: how many days the simulation should consider
#' @return A dataframe with num_rats rows, a shuffle created from one
#' of the combinations
shuffles_simulation <- function(single_data, num_rats, TOTAL_RATS, NUM_DAYS) {
  combinations <- create_combinations(num_rats, TOTAL_RATS)
  num_combinations <- length(combinations)
  # Creates a list with all shuffles
  all_shuffles <- lapply(seq(num_combinations), 
                         function(x) create_shuffle(single_data,
                                                    combinations, x))
  
  # Simulated shuffle results
  simulated_shuffles <- lapply(all_shuffles, 
                               function(x) simulate_shuffle(shuffle = x,
                                                            NUM_DAYS = NUM_DAYS))
  
  results_list <- lapply(simulated_shuffles, simulation_results)
  results_df <- do.call(rbind, results_list)
  colnames(results_df) <- c(paste0("latency_", 1:12), "winner",
                            "middle", "third", "tie_number")
  
  return(results_df)
}

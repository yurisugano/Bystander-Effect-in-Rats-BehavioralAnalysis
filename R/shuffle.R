library(dplyr)

# Load functions from functions.R
source("functions.R")

# Load data
data <- read.csv("../data/data.csv")

# Assign unique IDs
data$ID <- seq(nrow(data))

# Filter single animals for simulation
single_data <- data %>%
  filter(condition == "single") %>%
  select(c(day1:day12, ID))

# Remove data from memory
rm(data)

simulation_duos <- shuffles_simulation(single_data, 
                                       num_rats = 2, 
                                       TOTAL_RATS = 48,
                                       NUM_DAYS = 12)

simulation_trios <- shuffles_simulation(single_data,
                                        num_rats = 3,
                                        TOTAL_RATS = 48,
                                        NUM_DAYS = 12)


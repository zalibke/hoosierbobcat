# SIMPLE INDIANA BOBCAT POPULATION MODEL
# WHAT HAPPENS TO POPULATIONS OF REPRODUCING FEMALES UNDER DIFFERENT HARVEST SCENARIOS?
# ZANE LIBKE

library(ggplot2)
library(patchwork)

# params
initial_population <- 600  # Starting TOTAL female population
carrying_capacity <- 1200   # Carrying capacity for females (K)
growth_rate <- 0.5          # Intrinsic growth rate
incidental_harvest <- 100    # Incidental harvest/year
time_steps <- 30          # Number of years to model

# simple Logistic Growth
simulate_population <- function(initial_population, carrying_capacity, growth_rate, annual_harvest, incidental_harvest, time_steps) {
  population <- numeric(time_steps)
  population[1] <- initial_population
  
  for (t in 2:time_steps) {
    # Apply logistic growth
    growth <- growth_rate * population[t-1] * (1 - population[t-1] / carrying_capacity)
    
    # Apply harvest
    total_harvest <- annual_harvest + incidental_harvest
    total_harvest <- total_harvest/2 #### ASSUMING HALF OF ALL HARVESTS ARE REPRODUCING FEMALES
    population[t] <- population[t-1] + growth - total_harvest
    
    # Prevent negative populations
    if (population[t] < 0) population[t] <- 0
  }
  
  return(data.frame(Time = 1:time_steps, Population = population, Harvest = annual_harvest))
}

# Harvest scenarios
harvest_values <- c(0, 50, 100, 150, 200, 250, 300)


### FIX THIS !! MAKE A FUNCTION TO simulate and make graph etc... make sure to include correct labelling on graphs
simpop_makegraph <- function(carrying_capacity, incidental_harvest, initial_population, growth_rate){
  all_population_data <- do.call(rbind, lapply(harvest_values, function(harvest) {
    simulate_population(initial_population, carrying_capacity, growth_rate, harvest, incidental_harvest, time_steps)
  }))
  
  # Combine data and add harvest as a factor
  all_population_data$Harvest <- factor(all_population_data$Harvest, levels = harvest_values)
  
  # Plot high threshold K
  p1 <- ggplot(all_population_data, aes(x = Time, y = Population, color = Harvest)) +
    geom_line(size = 1) +
    labs(title = "Female K = 1200",
         x = "Time (years)",
         y = "# Reproducing Females",
         color = "Annual Harvest") +
    theme_minimal() +
    ylim(0, NA)
}




####### HIGH THRESHOLD CARRYING CAPACITY #########

carrying_capacity = 1200

all_population_data <- do.call(rbind, lapply(harvest_values, function(harvest) {
  simulate_population(initial_population, carrying_capacity, growth_rate, harvest, incidental_harvest, time_steps)
}))

# Combine data and add harvest as a factor
all_population_data$Harvest <- factor(all_population_data$Harvest, levels = harvest_values)

# Plot high threshold K
p1 <- ggplot(all_population_data, aes(x = Time, y = Population, color = Harvest)) +
  geom_line(size = 1) +
  labs(title = "Female K = 1200",
       x = "Time (years)",
       y = "# Reproducing Females",
       color = "Annual Harvest") +
  theme_minimal() +
  ylim(0, NA)
p1


######## LOW THRESHOLD CARRYING CAPACITY ##########

carrying_capacity <- 539

all_population_data <- do.call(rbind, lapply(harvest_values, function(harvest) {
  simulate_population(initial_population, carrying_capacity, growth_rate, harvest, incidental_harvest, time_steps)
}))

# Combine data and add harvest as a factor
all_population_data$Harvest <- factor(all_population_data$Harvest, levels = harvest_values)

# Plot low threshold K
p2 <- ggplot(all_population_data, aes(x = Time, y = Population, color = Harvest)) +
  geom_line(size = 1) +
  labs(title = "Female K = 539",
       x = "Time (years)",
       y = "# Reproducing Females",
       color = "Annual Harvest") +
  theme_minimal() +
  ylim(0, NA)
p2


######## MID THRESHOLD CARRYING CAPACITY ##########

carrying_capacity <- 870

all_population_data <- do.call(rbind, lapply(harvest_values, function(harvest) {
  simulate_population(initial_population, carrying_capacity, growth_rate, harvest, incidental_harvest, time_steps)
}))

# Combine data and add harvest as a factor
all_population_data$Harvest <- factor(all_population_data$Harvest, levels = harvest_values)

# Plot low threshold K
p3 <- ggplot(all_population_data, aes(x = Time, y = Population, color = Harvest)) +
  geom_line(size = 1) +
  labs(title = "Female K = 870",
       x = "Time (years)",
       y = "# Reproducing Females",
       color = "Annual Harvest") +
  theme_minimal() +
  ylim(0, NA)
p3



####### COMBINE THEM AND PLOT #######

combined_plot <- p1 + p3 + p2
#ggsave("../results/normal_high-avg-lowK.png", combined_plot, width = 15, height = 5)
ggsave("../results/inc100_high-avg-lowK.png", combined_plot, width = 15, height = 5)




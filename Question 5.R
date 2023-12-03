


## This function was revised from a personal project 
## that simulates pitcher outcomes/baserunning. 
## I would love the chance to share that project down the line

library(moderndive)
library(tidyverse)

sim_PA <- function(PA){
  if (PA$events %in% c("BB")){
    if (!PA$start_on_1b & !PA$start_on_2b & !PA$start_on_3b) {
      PA$end_on_1b <- TRUE
      PA$end_on_2b <- FALSE
      PA$end_on_3b <- FALSE
      PA$outs_created <- 0
      PA$runs_scored <- 0
    }
    if (PA$start_on_1b & !PA$start_on_2b & !PA$start_on_3b) {
      PA$end_on_1b <- TRUE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- FALSE
      PA$outs_created <- 0
      PA$runs_scored <- 0
    }
    if (!PA$start_on_1b & PA$start_on_2b & !PA$start_on_3b) {
      PA$end_on_1b <- FALSE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 1
    }
    # 1b false, 2b false, 3b true
    if (!PA$start_on_1b & !PA$start_on_2b & PA$start_on_3b) {
      PA$end_on_1b <- FALSE
      PA$end_on_2b <- FALSE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 1
    }
    # 1b true, 2b true, 3b false
    if (PA$start_on_1b & PA$start_on_2b & !PA$start_on_3b) {
      PA$end_on_1b <- TRUE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 0
    }
    # 1b true, 2b false, 3b true
    if (PA$start_on_1b & !PA$start_on_2b & PA$start_on_3b) {
      PA$end_on_1b <- TRUE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 0
    }
    # 1b false, 2b true, 3b true
    if (!PA$start_on_1b & PA$start_on_2b & PA$start_on_3b) {
      PA$end_on_1b <- FALSE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 0
    }
    # Full
    if (PA$start_on_1b & PA$start_on_2b & PA$start_on_3b) {
      PA$end_on_1b <- TRUE
      PA$end_on_2b <- TRUE
      PA$end_on_3b <- TRUE
      PA$outs_created <- 0
      PA$runs_scored <- 1
    }
  }
  if(PA$events %in% c("K")){
    PA$end_on_1b <- PA$start_on_1b
    PA$end_on_2b <- PA$start_on_2b
    PA$end_on_3b <- PA$start_on_3b
    PA$outs_created <- 1
    PA$runs_scored <- 0
  }
  if (PA$events == "HR") {
    base_runners = PA$start_on_1b + PA$start_on_2b + PA$start_on_3b
    PA$runs_scored <- 1 + base_runners # 1 for the batter, plus any base runners
    PA$end_on_1b <- FALSE
    PA$end_on_2b <- FALSE
    PA$end_on_3b <- FALSE
    PA$outs_created <- 0
  }
  
  PA <- PA %>%
    select(events, end_on_1b, end_on_2b, end_on_3b, outs_created, runs_scored )
  
  return(PA)
}

sim_innings <- function(number_of_innings, event_probs) {
  list_of_innings <- list()
  
  for (num_innings in 1:number_of_innings) {
    inning <- tibble(
      events = character(),
      start_on_1b = logical(),
      start_on_2b = logical(),
      start_on_3b = logical(),
      outs_when_up = integer(),
      end_on_1b = logical(),
      end_on_2b = logical(),
      end_on_3b = logical(),
      outs_created = integer(),
      runs_scored = integer()
    )
    
    while (sum(inning$outs_created) < 3) {
      # Random seed based on system time and iteration to ensure uniqueness
      set.seed(nrow(inning) + (num_innings * as.numeric(substr(format(Sys.time(), "%OS5"), 4, 7))))
      
      
      # Sample an event based on the provided probabilities
      event <- sample(c("BB", "K", "HR"), 1, prob = event_probs)
      
      # Initialize the first PA or retrieve the last PA's ending state
      if(nrow(inning) == 0) {
        PA <- tibble(
          events = event,
          start_on_1b = FALSE,
          start_on_2b = FALSE,
          start_on_3b = FALSE,
          outs_when_up = 0
        )
      } else {
        prev_PA <- inning[nrow(inning), ]
        PA <- tibble(
          events = event,
          start_on_1b = as.logical(prev_PA$end_on_1b),
          start_on_2b = as.logical(prev_PA$end_on_2b),
          start_on_3b = as.logical(prev_PA$end_on_3b),
          outs_when_up = as.integer(sum(inning$outs_created))
        )
      }
      
      # Simulate the plate appearance based on the event
      PA <- sim_PA(PA)
      
      inning <- bind_rows(inning, PA)
    }
    
    list_of_innings[[num_innings]] <- inning
  }
  return(list_of_innings)
}

# This is leaving starts_on_xx NA, but it doesn't really matter much in this
# context as the loop is functioning as intended. 
event_probs <- c(0.25, 0.66, 0.09) # Probabilities for BB, K, HR respectively

tic()
simulated_innings <- sim_innings(10000, event_probs)
toc()

inning_summary <- map_dfr(simulated_innings, ~ tibble(
  HR = sum(.x$events == "HR", na.rm = TRUE),
  BB = sum(.x$events == "BB", na.rm = TRUE),
  K = sum(.x$events == "K", na.rm = TRUE),
  Runs = sum(.x$runs_scored, na.rm = TRUE)
), .id = "Inning")

# Convert the Inning column to integer
inning_summary$Inning <- as.integer(inning_summary$Inning)

# 9 innings/game * 162 = 1458innings
bootstrap <- inning_summary %>%
  rep_sample_n(size = 1458, reps = 15000, replace = TRUE)%>%
  group_by(replicate) %>%
  summarize(runs  = sum(Runs), K = sum(K), BB = sum(BB),HR = sum(HR), 
            total_batters = K + BB + HR, 
            `K%`  = K / total_batters, 
            `BB%` = BB /total_batters, 
            `HR%` = HR / total_batters, 
            runs_per_inning = runs / 1458, 
            )

# Calculate the mean and standard error of the mean (SEM)
mean_runs <- mean(bootstrap$runs_per_inning, na.rm = TRUE)
sem_runs <- sd(bootstrap$runs_per_inning, na.rm = TRUE) / sqrt(nrow(bootstrap))

# Calculate the 95% confidence interval
ci_upper <- mean_runs + qt(0.975, df = nrow(bootstrap) - 1) * sem_runs
ci_lower <- mean_runs - qt(0.975, df = nrow(bootstrap) - 1) * sem_runs

# Create the histogram and shade the confidence interval
# Create the histogram and shade the confidence interval
ggplot(bootstrap, aes(x = runs_per_inning)) +
  geom_histogram(fill = "dodgerblue", color = "white", bins = 30) +
  geom_vline(xintercept = mean_runs, color = "darkred", linetype = "dashed", size = 1.2) +
  labs(title = "Bootsrapped Distribution of Simulated Runs per Inning",
       x = "Runs per Inning",
       y = "Count",
       caption = "BB% = 0.25, K% = 0.66, HR = 0.09") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    axis.line = element_line(size = 0.5, color = "grey"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10),
    legend.position = "none"
  ) +
  annotate("text", x = mean_runs, y = Inf, label = paste0("Mean: ", round(mean_runs, 4)), vjust = 2, hjust = -0.2,  color = "darkred", size = 4)






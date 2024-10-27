library(tidyverse)
#Summary table for supplement 

#For each trait the mean, median, min, max, and standard deviation are reported. 

data_ploidy <- read_csv("data/TableS1.csv")

# Convert to long format to make summarizing easier

data_ploidy <- data_ploidy %>% select(apple_id, acidity_17_harv:date_jul_17_harv)

apple_long <- data_ploidy %>%
  pivot_longer(cols=-apple_id, names_to = "trait", values_to = "value")

# Calculate summary statistics for each trait
summary_stats <- apple_long %>%
  group_by(trait) %>%
  summarise(count = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE))

write_csv(summary_stats, "data/TableS2.csv")

#This is included as Table S2 in the paper
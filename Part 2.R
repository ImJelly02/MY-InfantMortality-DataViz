library(dplyr)
library(writexl)

# Section 2: By region
# Load the datasets
death_infant_state <- read.csv("death_infant_state.csv", header=TRUE)
death_neonatal_state <- read.csv("death_neonatal_state.csv", header=TRUE)
death_perinatal_state <- read.csv("death_perinatal_state.csv", header=TRUE)
stillbirth_state <- read.csv("stillbirth_state.csv", header=TRUE)

# View the dataseta
head(death_infant_state)
head(death_neonatal_state)
head(death_perinatal_state)
head(stillbirth_state)

# Remove the 'rate' column from each dataset
death_infant_state<- death_infant_state %>% select(-rate)
death_neonatal_state <- death_neonatal_state %>% select(-rate)
death_perinatal_state <- death_perinatal_state %>% select(-rate)
stillbirth_state <- stillbirth_state %>% select(-rate)

# Subtract stillbirth abs from death_perinatal abs by matching date
death_perinatal_state_clean <- death_perinatal_state %>%
  left_join(stillbirth_state, by = c("state", "date"), suffix = c("_perinatal_state", "_stillbirth_state")) %>%
  mutate(abs_diff_state = abs_perinatal_state - abs_stillbirth_state) %>%
  select(date, state, abs_diff_state)
death_perinatal_state_clean

# Subtract death_neonatal abs from death_infant abs by matching date
death_infant_state_clean <- death_infant_state %>%
  left_join(death_neonatal_state, by = c("state", "date"), suffix = c("_infant_state", "_neonatal_state")) %>%
  mutate(abs_diff_state = abs_infant_state - abs_neonatal_state) %>%
  select(date, state, abs_diff_state)
death_infant_state_clean

# Subtract death_neonatal abs from the modified death_perinatal abs_diff by matching date
death_neonatal_state_clean <- death_neonatal_state %>%
  left_join(death_perinatal_state_clean, by = c("state", "date"), suffix = c("_neonatal_state", "_perinatal_state")) %>%
  mutate(abs_diff_state = abs - abs_diff_state) %>%
  select(date, state, abs_diff_state)
death_neonatal_state_clean 

# Add Age Group labels and prepare each dataset for merging
stillbirth_state_final <- stillbirth_state %>%
  mutate(age_group = "Stillbirth") %>%
  select(date, state, abs, age_group) %>%
  rename(annual_death = abs)

death_perinatal_state_final <- death_perinatal_state_clean %>%
  mutate(age_group = "Early neonatal death") %>%
  select(date, state, abs_diff_state, age_group) %>%
  rename(annual_death = abs_diff_state)

death_neonatal_state_final <- death_neonatal_state_clean %>%
  mutate(age_group = "Late neonatal death") %>%
  select(date, state, abs_diff_state, age_group) %>%
  rename(annual_death = abs_diff_state)

death_infant_state_final <- death_infant_state_clean %>%
  mutate(age_group = "Postneonatal death") %>%
  select(date, state, abs_diff_state, age_group) %>%
  rename(annual_death = abs_diff_state)

# Combine all datasets into one
final_state_dataset <- bind_rows(stillbirth_state_final, death_perinatal_state_final, death_neonatal_state_final, death_infant_state_final)
final_state_dataset

# Export the final dataset to an Excel file
write_xlsx(final_state_dataset, "Infant Death By State Cleaned And Merged.xlsx")

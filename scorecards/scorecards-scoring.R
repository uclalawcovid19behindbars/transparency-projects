library(tidyverse)
library(behindbarstools)
library(kableExtra)

# Load data 
scorecard <- googlesheets4::read_sheet(
    "1fHhRAjwYGVmgoHLUENvcYffHDjEQnpp7Rwt9tLeX_Xk", 
    sheet = "Data", range = "C1:O56") 

# Specify max points!  
MAX_POINTS <- 24 

# Assign scores 
scores <- scorecard %>% 
    slice(-(1:2)) %>% 
    mutate(across(c(machine:history), 
                  ~ case_when(.x == "Yes" ~ 2, 
                              .x == "No" ~ 0)), 
           across(c(cases_residents:tests_staff), 
                  ~ case_when(.x == "Facility-Level" ~ 2,
                              .x == "Statewide" ~ 1, 
                              .x == "" ~ 0)), 
           across(c(machine:tests_staff), 
                  ~ replace(., is.na(.), 0))) %>% 
    rename_with(~paste0(., "_score"), c(machine:tests_staff)) %>% 
    mutate(quality_total = machine_score + regularly_score + defined_score + history_score,
           residents_total = rowSums(select(., ends_with("residents_score"))), 
           staff_total = rowSums(select(., ends_with("_staff_score"))), 
           total = quality_total + residents_total + staff_total, 
           percentage = total / MAX_POINTS * 100, 
           score = case_when(percentage >= 90 ~ "A", 
                             percentage >= 80 & percentage < 90 ~ "B", 
                             percentage >= 70 & percentage < 80 ~ "C", 
                             percentage >= 60 & percentage < 70 ~ "D", 
                             percentage < 60 ~ "F"))

joined <- scorecard %>% 
    left_join(scores, by = "state") %>% 
    slice(-(1:2))

# Plot distribution 
ggplot(joined, aes(x = total)) + 
    geom_histogram(bins = MAX_POINTS, color = "white") + 
    theme_minimal() 

joined %>% 
    group_by(score) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    ggplot(aes(x = score, y = n, label = n)) + 
    geom_bar(stat = 'identity') + 
    geom_text(nudge_y = 2.0) + 
    theme_minimal() 

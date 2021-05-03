rm(list = ls())

library(behindbarstools)
library(tidyverse)
library(janitor)
library(scales)

# ------------------------------------------------------------------------------
# Read and clean data 
# ------------------------------------------------------------------------------

# Read raw data 
raw <- readxl::read_excel("data/raw/Transparency Study_4.26.21.xlsx")

cleaned <- raw %>% 
    # Drop long column names 
    slice(-1) %>% 
    # Drop duplicates for Erie County and Arlington County 
    distinct(county, state, .keep_all = TRUE) %>%
    # Clean discrete columns 
    mutate(
        across(c(source_reports:releases, news_reports, releases_policy:breakdown_other, legal_filing), 
               tolower), 
        across(c(source_reports:releases, news_reports, breakdown:breakdown_vulnerable, legal_filing),
               ~ case_when(
                   str_detect(.x, "yes") | .x %in% c("y", "x") ~ "Y",
                   TRUE ~ "N")),
        releases_policy = case_when(
            str_detect(releases_policy, "releases") ~ "Releases", 
            str_detect(releases_policy, "policy") ~ "Policy", 
            str_detect(releases_policy, "mix") ~ "Mix", 
            TRUE ~ NA_character_), 
        breakdown_other = ifelse(is.na(breakdown_other) | breakdown_other %in% c("n"), "N", "Y"), 
        across(c(breakdown:date_published), 
               ~ case_when(
                   news_reports == "N" ~ NA_character_,
                   TRUE ~ .x))
    ) %>% 
    # Clean numeric variables 
    mutate(capacity = as.numeric(str_remove(capacity_clean, "-")), 
           wave_1_pop_reduction = as.numeric(wave_1_pop_reduction_clean), 
           wave_1_pop_prior = as.numeric(str_remove(wave_1_pop_prior_clean, "N/A"))) %>% 
    # Clean date
    mutate(date_checked = janitor::excel_numeric_to_date(as.numeric(date_checked))) %>% 
    # Create source_reports excluding population
    mutate(source_reports_covid = ifelse(
        cases == "N" & testing == "N" & deaths == "N" & releases == "N", "N", "Y")) %>% 
    # Replace Staunton County
    mutate(county = ifelse(county == "Staunton County", "Staunton City", county)) 

# ------------------------------------------------------------------------------
# Merge NYT COVID data  
# ------------------------------------------------------------------------------

nyt <- read_csv(str_c(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/", 
    "master/prisons/facilities.csv"))

# TODO: Make sure group_by(sum) makes sense! 
nyt_ <- nyt %>% 
    filter(facility_type == "Jail") %>% 
    group_by(facility_county_fips) %>%
    summarise(
        nyt_latest_inmate_population = sum_na_rm(latest_inmate_population), 
        nyt_total_inmate_cases = sum_na_rm(total_inmate_cases), 
        nyt_total_inmate_deaths = sum_na_rm(total_inmate_deaths), 
        nyt_total_officer_cases = sum_na_rm(total_officer_cases), 
        nyt_total_officer_deaths = sum_na_rm(total_officer_deaths)) %>% 
    ungroup() %>% 
    mutate(facility_county_fips = as.numeric(facility_county_fips)) 

fips_ <- cleaned %>% 
    filter(!county %in% c("New York City (including Kings County)")) %>% 
    rowwise() %>% 
    mutate(fips = usmap::fips(state, county)) %>% 
    select(state, county, fips) %>% 
    mutate(fips = as.double(fips))

cleaned_nyt <- cleaned %>% 
    left_join(fips_, by = c("county", "state")) %>% 
    left_join(nyt_, by = c("fips" = "facility_county_fips"))

# ------------------------------------------------------------------------------
# Merge Vera jail population data  
# ------------------------------------------------------------------------------

vera <- read_csv(str_c(
    "https://raw.githubusercontent.com/vera-institute/", 
    "jail-population-data/master/jail_population.csv"))

# First date available 
vera_first <- vera %>% 
    group_by(fips) %>% 
    mutate(first = min(date)) %>% 
    filter(date == first) %>% 
    select(fips, vera_pop_first = jail_population)

# Feb20 population  
vera_feb20 <- vera %>% 
    filter(month(date) == 2 & year(date) == 2020) %>% 
    group_by(fips) %>% 
    mutate(first = min(date)) %>% 
    filter(date == first) %>% 
    select(fips, vera_pop_feb20 = jail_population)

# May20 population 
vera_may20 <- vera %>% 
    filter(month(date) == 5 & year(date) == 2020) %>% 
    group_by(fips) %>% 
    mutate(first = min(date)) %>% 
    filter(date == first) %>% 
    select(fips, vera_pop_may20 = jail_population)

vera_combined <- vera_first %>% 
    full_join(vera_feb20, by = "fips") %>% 
    full_join(vera_may20, by = "fips")

cleaned_nyt_vera <- cleaned_nyt %>% 
    left_join(vera_combined, by = "fips")

# ------------------------------------------------------------------------------
# Merge BJS jail population data  
# ------------------------------------------------------------------------------

bjs <- haven::read_dta("data/raw/37392-0001-Data.dta")

# TODO: Make sure group_by(sum) makes sense! 
cleaned_nyt_vera_bjs <- cleaned_nyt_vera %>% 
    left_join(
        bjs %>% 
            mutate(CNTYCODE = as.double(CNTYCODE)) %>% 
            group_by(CNTYCODE) %>% 
            summarise(bjs_pop = sum_na_rm(ADP), 
                      bjs_capacity = sum_na_rm(RATED)), 
        by = c("fips" = "CNTYCODE")) 

# ------------------------------------------------------------------------------
# Save cleaned file 
# ------------------------------------------------------------------------------

limited <- cleaned_nyt_vera_bjs %>% 
    select(state, county, fips, pop_tier, region, date_checked, 
           source_reports_covid, 
           source_reports:releases, news_reports, 
           releases_policy:breakdown_other, legal_filing, 
           capacity, wave_1_pop_reduction, wave_1_pop_prior, 
           starts_with("nyt_"), starts_with("vera_"), starts_with("bjs_")) %>% 
    mutate(fips = stringr::str_pad(fips, 5, pad = "0")) %>% 
    mutate(pop_coalesce = coalesce(wave_1_pop_prior, 
                                   vera_pop_feb20, 
                                   vera_pop_first, 
                                   nyt_latest_inmate_population, 
                                   bjs_pop)) 

write_csv(limited, "data/interim/releases-transparency-cleaned.csv", na = "")

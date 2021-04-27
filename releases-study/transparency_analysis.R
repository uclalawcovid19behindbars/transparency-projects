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
        across(c(breakdown:legal_filing), 
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
        cases == "N" & testing == "N" & deaths == "N" & releases == "N", "N", "Y"
        ))

# ------------------------------------------------------------------------------
# Summary stats and crosstabs 
# ------------------------------------------------------------------------------
# Breakdown of regions and population tiers 
get_crosstab <- function(df, metric){
    df %>% 
        tabyl(!!sym(metric)) %>% 
        adorn_totals("row") %>%
        adorn_pct_formatting()
}

get_crosstab(cleaned, "pop_tier")
get_crosstab(cleaned, "region")
get_crosstab(cleaned, "dashboard")

cleaned %>% 
    tabyl(region, pop_tier)

# ------------------------------------------------------------------------------
# Plots 
# ------------------------------------------------------------------------------

# Crosstab plotting function 
get_plot <- function(df, group_var, pct_var) {
    if (group_var == "pop_tier") {x_lab <- "Population Tier"}
    else if (group_var == "region") {x_lab <- "Region"}
    
    df %>% 
        group_by(!!sym(group_var), !!sym(pct_var)) %>% 
        summarise(n = n()) %>% 
        pivot_wider(names_from = !!sym(pct_var), values_from = n) %>% 
        mutate(pct = Y / sum(N, Y)) %>% 
        ggplot(aes(x = fct_rev(!!sym(group_var)), y = pct, 
                   label = paste0(percent(pct, accuracy = 1), " (n = ", Y, ")"))) + 
        geom_point(size = 4.0, color = "#4C6788") + 
        geom_segment(aes(x = fct_rev(!!sym(group_var)), 
                         xend = fct_rev(!!sym(group_var)), 
                         y = 0, 
                         yend = pct), 
                     size = 2.0, color = "#4C6788") + 
        geom_text(size = 5, position = position_nudge(y = 0.055, x = 0.15), color = "black", family = "Helvetica") +
        coord_flip() + 
        theme_behindbars(base_size = 16, base_color = "black") + 
        scale_y_continuous(label = percent, limit = c(0, 0.5)) + 
        labs(x = x_lab) 
}

# Pertinent data by tier 
get_plot(cleaned, "pop_tier", "source_reports_covid") + 
    labs(title = "% of counties with source reporting data by population tier")

# Pertinent data by region 
get_plot(cleaned, "region", "source_reports_covid") + 
    labs(title = "% of counties with source reporting data by region")

# Pertinent data by metric 
cleaned %>% 
    select(dashboard:releases) %>% 
    pivot_longer(cols = c(dashboard:releases), names_to = "metric", values_to = "value") %>% 
    group_by(metric, value) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = value, values_from = n) %>% 
    mutate(metric = str_to_title(metric), 
           pct = Y / sum(N, Y)) %>% 
    ggplot(aes(x = fct_rev(metric), y = pct, 
               label = paste0(percent(pct, accuracy = 1), " (n = ", Y, ")"))) + 
    geom_point(size = 4.0, color = "#4C6788") + 
    geom_segment(aes(x = fct_rev(metric), 
                     xend = fct_rev(metric), 
                     y = 0, 
                     yend = pct), 
                 size = 2.0, color = "#4C6788") + 
    geom_text(size = 4, position = position_nudge(y = 0.03, x = 0.2), color = "black", family = "Helvetica") +
    coord_flip() + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    scale_y_continuous(label = percent, limit = c(0, 0.3)) + 
    theme(axis.title.y = element_blank(), 
          legend.position = "top") + 
    labs(title = "% of all counties with source reporting data")

# ------------------------------------------------------------------------------
# Summary plots 
# ------------------------------------------------------------------------------

# Pertinent data by metric and tier 
cleaned %>% 
    select(pop_tier, dashboard:releases) %>% 
    pivot_longer(cols = c(dashboard:releases), names_to = "metric", values_to = "value") %>% 
    group_by(pop_tier, metric, value) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = value, values_from = n) %>% 
    mutate(metric = str_to_title(metric), 
           pct = Y / sum(N, Y)) %>% 
    ggplot(aes(x = fct_rev(metric), y = pct, fill = fct_rev(factor(pop_tier)))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    coord_flip() + 
    scale_fill_manual(name = "Population Tier",
                      breaks = c(1, 2, 3),
                      labels = c("Tier 1", "Tier 2", "Tier 3"), 
                      values = c("#D7790F", "#82CAA4", "#4C6788")) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    scale_y_continuous(label = percent, limits = c(0, 0.4)) + 
    theme(axis.title.y = element_blank(), 
          legend.position = "top") + 
    labs(title = "% of counties with source reporting data by population tier")

# Pertinent data by metric and region 
cleaned %>% 
    select(region, dashboard:releases) %>% 
    pivot_longer(cols = c(dashboard:releases), names_to = "metric", values_to = "value") %>% 
    group_by(region, metric, value) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = value, values_from = n) %>% 
    mutate(metric = str_to_title(metric), 
           pct = Y / sum(N, Y)) %>% 
    ggplot(aes(x = fct_rev(metric), y = pct, fill = fct_rev(factor(region)))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    coord_flip() + 
    scale_fill_manual(name = "Region",
                      breaks = c(1, 2, 3, 4),
                      labels = c("Region 1", "Region 2", "Region 3", "Region 4"),
                      values = c("#D7790F", "#82CAA4", "#4C6788", "#AE91A8")) +
    theme_behindbars(base_size = 16, base_color = "black") + 
    scale_y_continuous(label = percent, limits = c(0, 0.3)) + 
    theme(axis.title.y = element_blank(), 
          legend.position = "top") + 
    labs(title = "% of counties with source reporting data by region")

# Releases breakdown plot 
cleaned %>% 
    select(county, state, starts_with("breakdown")) %>% 
    pivot_longer(cols = starts_with("breakdown_"), names_to = "breakdown_cat", values_to = "value") %>% 
    group_by(breakdown_cat, value) %>% 
    summarise(n = n()) %>% 
    filter(value == "Y") %>% 
    ggplot(aes(x = reorder(breakdown_cat, n), y = n, label = n)) + 
    geom_point(size = 4.0, color = "#4C6788") + 
    geom_segment(aes(x = reorder(breakdown_cat, n), 
                     xend = reorder(breakdown_cat, n), 
                     y = 0, 
                     yend = n), 
                 size = 2.0, color = "#4C6788") + 
    geom_text(size = 4, position = position_nudge(y = 0.06, x = 0.3), color = "black", family = "Helvetica") +
    coord_flip() +
    scale_x_discrete(
        labels = c("breakdown_minor_offenses" = "Minor Offenses", 
                   "breakdown_other" = "Other",
                   "breakdown_time_left" = "Short Time Left on Sentence", 
                   "breakdown_vulnerable" = "Vulnerable Populations", 
                   "breakdown_bail" = "On Bail with Inability to Pay", 
                   "breakdown_parole_violations" = "Probation/Parole Tech Violation")) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    theme(axis.title.y = element_blank()) + 
    labs(title = "# of counties based on breakdown of releases")

# ------------------------------------------------------------------------------
# Merge NYT covid data  
# ------------------------------------------------------------------------------

nyt <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/nytimes/covid-19-data/", 
    "master/prisons/facilities.csv"))

# TODO: Make sure group_by(sum) makes sense! 
nyt_ <- nyt %>% 
    filter(facility_type == "Jail") %>% 
    group_by(facility_county_fips) %>% 
    summarise(
        latest_inmate_population = sum_na_rm(latest_inmate_population), 
        total_inmate_cases = sum_na_rm(total_inmate_cases), 
        total_inmate_deaths = sum_na_rm(total_inmate_deaths), 
        total_officer_cases = sum_na_rm(total_officer_cases), 
        total_officer_deaths = sum_na_rm(total_officer_deaths)) %>% 
    ungroup() %>% 
    mutate(facility_county_fips = as.numeric(facility_county_fips)) 
    
fips_ <- cleaned %>% 
    filter(!county %in% c("New York City (including Kings County)", "Staunton County")) %>% 
    rowwise() %>% 
    mutate(fips = usmap::fips(state, county)) %>% 
    select(state, county, fips) %>% 
    mutate(fips = as.double(fips))

cleaned_nyt <- cleaned %>% 
    left_join(fips_, by = c("county", "state")) %>% 
    left_join(nyt_, by = c("fips" = "facility_county_fips"))

out <- cleaned_nyt %>% 
    select(state:region, date_checked, 
           source_reports_covid, 
           source_reports:releases, news_reports, 
           releases_policy:breakdown_other, legal_filing, 
           capacity, wave_1_pop_reduction, wave_1_pop_prior, 
           fips:total_officer_deaths)

write.csv(out, "data/out/releases-transparency-study-raw.csv", row.names = FALSE, na = "")   

# ------------------------------------------------------------------------------
# Merge population data  
# ------------------------------------------------------------------------------

# Merge with Vera jail population data 
vera <- read_csv(stringr::str_c(
    "https://raw.githubusercontent.com/vera-institute/", 
    "jail-population-data/master/jail_population.csv"))

# Get first date from Vera data 
# TODO: what do we actually want here? 
vera_earliest <- vera %>% 
    group_by(fips) %>% 
    mutate(first = min(date)) %>% 
    filter(date == first) %>% 
    select(-first, county_name, state_name)

cleaned_nyt_vera <- cleaned_nyt %>% 
    left_join(vera_earliest, by = "fips")

# Merge with BJS jail population data 
# TODO: duplicate FIPS codes 
bjs <- haven::read_dta("data/raw/37392-0001-Data.dta")

cleaned_nyt_vera_bjs <- cleaned_nyt_vera %>% left_join(
    bjs %>% 
        mutate(CNTYCODE = as.double(CNTYCODE)) %>% 
        group_by(CNTYCODE) %>% 
        summarise(bjs_ADP = sum_na_rm(ADP), 
                  bjs_RATED = sum_na_rm(RATED)), 
    by = c("fips" = "CNTYCODE")) 

# Save cleaned file 
limited <- cleaned_nyt_vera_bjs %>% 
    select(state, county, fips, pop_tier, region, date_checked, 
           source_reports_covid, 
           source_reports:releases, news_reports, 
           releases_policy:breakdown_other, legal_filing, 
           capacity, wave_1_pop_reduction, wave_1_pop_prior, 
           nyt_latest_pop = latest_inmate_population, 
           nyt_total_inmate_cases = total_inmate_cases, 
           nyt_total_inmate_deaths = total_inmate_deaths, 
           nyt_total_officer_cases = total_officer_cases, 
           nyt_total_officer_deaths = total_officer_deaths, 
           vera_jail_population = jail_population, 
           vera_resident_population = resident_population, 
           bjs_ADP, bjs_RATED) 

write_csv(limited, "data/interim/releases-transparency-cleaned.csv", na = "")
    
# ------------------------------------------------------------------------------
# More plots 
# ------------------------------------------------------------------------------

# Reporting pertinent data boxplots 
cleaned_nyt_vera_bjs %>% 
    ggplot(aes(x = source_reports, y = resident_population, fill = source_reports)) + 
    geom_boxplot() +
    scale_fill_bbdiscrete() + 
    scale_color_bbdiscrete() + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    theme(legend.position = "none") + 
    labs(title = "Reporting Pertinent Data by Overall County Population", 
         y = "Overall County Population") + 
    scale_y_continuous(label = comma)

# Wave 1 scatterplot 
ggplot() + 
    geom_jitter(
        cleaned_pop, 
        mapping = aes(x = wave_1_pop_prior, y = wave_1_pop_reduction), size = 1.0) + 
    ggrepel::geom_text_repel(
        size = 5, 
        cleaned_pop %>% filter(wave_1_pop_reduction > 1000), 
        mapping = aes(x = wave_1_pop_prior, y = wave_1_pop_reduction, label = county)) + 
    geom_abline(slope = 0.2, linetype = "dashed") + 
    scale_x_continuous(label = comma) + 
    scale_y_continuous(label = comma) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    theme(axis.title.x = element_text(color = "black")) + 
    labs(title = "Jail Releases by County: Wave 1 (Feb 28, 2020 - Oct 31, 2020)", 
         x = "Population Prior to Releases", 
         y = "Overall Population Reduction /\nTotal Number of Releases")

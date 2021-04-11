rm(list = ls())

library(behindbarstools)
library(tidyverse)
library(janitor)
library(scales)

# ------------------------------------------------------------------------------
# Read and clean data 
# ------------------------------------------------------------------------------

# Read raw data 
raw <- readxl::read_excel("data/Transparency Study.xlsx")

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
    # TODO: Clean non-numeric characters (e.g. "appx.", "~", etc.)
    mutate(
        wave_1_pop_reduction = as.numeric(wave_1_pop_reduction), 
        wave_1_pop_prior = as.numeric(wave_1_pop_prior), 
        wave_1_pop_pct = wave_1_pop_reduction / wave_1_pop_prior
    )

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

cleaned %>% 
    tabyl(region, pop_tier)

# Crosstab plotting function 
get_plot <- function(df, group_var, pct_var) {
    if (group_var == "pop_tier") {x_lab <- "Population Tier"}
    else if (group_var == "region") {x_lab <- "Region"}
    
    df %>% 
        group_by(!!sym(group_var), !!sym(pct_var)) %>% 
        summarise(n = n()) %>% 
        pivot_wider(names_from = !!sym(pct_var), values_from = n) %>% 
        mutate(pct = Y / sum(N, Y)) %>% 
        ggplot(aes(x = fct_rev(!!sym(group_var)), y = pct, label = percent(pct, accuracy = 1))) + 
        geom_bar(stat = "identity", width = 0.7, fill = "#4C6788") + 
        geom_text(size = 5, position = position_nudge(y = -0.03), color = "white", family = "Helvetica") +
        coord_flip() + 
        theme_behindbars(base_size = 14, base_color = "black") + 
        scale_y_continuous(label = percent, limit = c(0, 0.5)) + 
        labs(x = x_lab) 
}

get_plot(cleaned, "region", "source_reports") + 
    labs(title = "Percentage of Counties with Source Reporting Pertinent Data")

# ------------------------------------------------------------------------------
# Plots 
# ------------------------------------------------------------------------------

# Pertinent data by metric and tier 
cleaned %>% 
    select(pop_tier, testing:releases) %>% 
    pivot_longer(cols = c(testing:releases), names_to = "metric", values_to = "value") %>% 
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
    scale_y_continuous(label = percent, limits = c(0, 0.5)) + 
    theme(axis.title.y = element_blank(), 
          legend.position = "top") + 
    labs(title = "Percentage of Counties with Source Reporting Pertinent Data")

# Releases breakdown plot 
cleaned %>% 
    select(county, state, starts_with("breakdown")) %>% 
    pivot_longer(cols = starts_with("breakdown_"), names_to = "breakdown_cat", values_to = "value") %>% 
    group_by(breakdown_cat, value) %>% 
    summarise(n = n()) %>% 
    filter(value == "Y") %>% 
    ggplot(aes(x = reorder(breakdown_cat, n), y = n, label = n)) + 
    geom_bar(stat = "identity", width = 0.7, fill = "#4C6788") + 
    geom_text(size = 5, position = position_nudge(y = -3.0), color = "white", family = "Helvetica") +
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
    labs(title = "Number of Counties Based on Breakdown of Releases")

# ------------------------------------------------------------------------------
# Merge population data  
# ------------------------------------------------------------------------------

# Get FIPS codes for counties 
fips_ <- cleaned %>% 
    filter(!county %in% c("New York City (including Kings County)", "Staunton County")) %>% 
    rowwise() %>% 
    mutate(fips = usmap::fips(state, county)) %>% 
    select(state, county, fips) %>% 
    mutate(fips = as.double(fips))

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

cleaned_pop <- cleaned %>% 
    left_join(fips_, by = c("county", "state")) %>% 
    left_join(vera_earliest, by = "fips")

# Merge with BJS jail population data 
# TODO: duplicate FIPS codes? 
bjs <- haven::read_dta("data/37392-0001-Data.dta")

cleaned_pop_bjs <- cleaned_pop %>% left_join(
    bjs %>% 
        select(CNTYCODE, TOTPOP) %>% 
        mutate(CNTYCODE = as.double(CNTYCODE)) %>% 
        distinct(), 
    by = c("fips" = "CNTYCODE")) %>% 
    mutate(jail_pop_coalesce = coalesce(jail_population, TOTPOP))

# Reporting pertinent data boxplots 
cleaned_pop %>% 
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

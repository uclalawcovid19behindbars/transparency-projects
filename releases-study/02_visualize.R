library(behindbarstools)
library(tidyverse)
library(janitor)
library(scales)

raw <- read.csv("data/interim/releases-transparency-cleaned.csv")

limited <- raw %>% 
    mutate(
        Releases = ifelse(releases == "Y" | news_reports == "Y", "Y", "N"), 
        pop_prior_coalesce = coalesce(wave_1_pop_prior, vera_pop_feb20), 
        pct_red = coalesce(
            (wave_1_pop_prior - wave_1_pop_reduction) / wave_1_pop_prior, 
            (vera_pop_feb20 - vera_pop_may20) / vera_pop_feb20), 
        covid_rate = nyt_total_inmate_cases / pop_coalesce)

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

get_crosstab(limited, "pop_tier")
get_crosstab(limited, "region")
get_crosstab(limited, "dashboard")

limited %>% 
    tabyl(region, pop_tier)

# ------------------------------------------------------------------------------
# Basic crosstab plots  
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
        ggplot(aes(x = fct_rev(as.factor(!!sym(group_var))), y = pct, 
                   label = paste0(percent(pct, accuracy = 1), " (n = ", Y, ")"))) + 
        geom_point(size = 4.0, color = "#4C6788") + 
        geom_segment(aes(x = fct_rev(as.factor(!!sym(group_var))), 
                         xend = fct_rev(as.factor(!!sym(group_var))), 
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
get_plot(limited, "pop_tier", "source_reports_covid") + 
    labs(title = "% of counties with source reporting data by population tier")

# Pertinent data by region 
get_plot(limited, "region", "source_reports_covid") + 
    labs(title = "% of counties with source reporting data by region")

# Pertinent data by metric 
limited %>% 
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

# Pertinent data by metric and tier 
limited %>% 
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
limited %>% 
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

# Releases breakdown  
limited %>% 
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
# Legal filings plots  
# ------------------------------------------------------------------------------

# Stacked baraplot 
limited %>% 
    group_by(pop_tier, source_reports_covid, legal_filing) %>% 
    summarise(n()) %>% 
    mutate(cat = str_c("Source reports: ", source_reports_covid, "\nLegal filing: ", legal_filing)) %>% 
    ggplot(aes(fill = cat, x = as.factor(pop_tier), y = `n()`)) + 
    geom_bar(position = "stack", stat = "identity", width = 0.7) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    theme(legend.position = "bottom", 
          legend.title = element_blank()) + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    scale_fill_bbdiscrete() + 
    labs(title = "Reporting and legal filings by population tier", 
         y = "Number of counties") 

# Facet barplot 
limited %>% 
    group_by(pop_tier, source_reports_covid, legal_filing) %>% 
    summarise(n()) %>% 
    mutate(pop_tier = str_c("Tier ", pop_tier)) %>% 
    mutate(cat = str_c("Source reports: ", source_reports_covid, "\nLegal filing: ", legal_filing)) %>% 
    ggplot(aes(x = fct_rev(as.factor(cat)), y = `n()`, fill = cat, label = `n()`)) + 
    geom_bar(position = "stack", stat = "identity", width = 0.7) + 
    geom_text(size = 4, vjust = -1, hjust = -0.7, color = "black", family = "Helvetica") +
    facet_wrap(~pop_tier) + 
    coord_flip() + 
    scale_fill_bbdiscrete() + 
    scale_y_continuous(limits = c(0, 200), breaks = c(0, 100, 200)) + 
    theme_behindbars(base_size = 16, base_color = "black") + 
    theme(axis.title.y = element_blank(), 
          legend.position = "none") +
    labs(title = "Reporting and legal filings by population tier", 
         y = "Number of counties") 

# ------------------------------------------------------------------------------
# COVID rates plots 
# ------------------------------------------------------------------------------

log_scatter <- function(df, x_var, y_var, color_var){
    df %>% 
        ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(color_var))) + 
        geom_point(size = 1.0, alpha = 0.8) + 
        scale_color_bbdiscrete() + 
        theme_behindbars(base_size = 16, base_color = "black") + 
        theme(axis.title.y = element_text(color = "black"),
              axis.title.x = element_text(color = "black"),
              legend.position = "top") + 
        scale_x_continuous(trans = "log", label = comma) +
        scale_y_continuous(trans = "log", label = comma) 
}

limited %>% 
    arrange(source_reports_covid) %>% 
    rename("Source reports pertinent data" = source_reports_covid) %>% 
    log_scatter(., "pop_coalesce", "nyt_total_inmate_cases", "Source reports pertinent data") + 
    labs(x = "Total jail population (log)", 
         y = "Total cases (log)", 
         title = "Relationship between reporting and COVID rates") 

limited %>% 
    arrange(desc(Releases)) %>% 
    log_scatter(., "pop_coalesce", "nyt_total_inmate_cases", "Releases") + 
    labs(x = "Total jail population (log)", 
         y = "Total cases (log)", 
         title = "Relationship between releases and COVID rates") 

# Reporting pertinent data boxplots 
boxplot <- function(df, x_var, y_var){
    df %>% 
        ggplot(aes(x = !!sym(x_var), y = !!sym(y_var), fill = !!sym(x_var))) + 
        geom_boxplot() +
        scale_fill_bbdiscrete() + 
        scale_y_continuous(label = percent) + 
        theme_behindbars(base_size = 16, base_color = "black") + 
        theme(legend.position = "none", 
              axis.title.x = element_text(color = "black"))
}

limited %>% 
    filter(covid_rate < 1) %>% 
    boxplot(., "source_reports_covid", "covid_rate") + 
    labs(title = "Relationship between reporting and COVID rates", 
         x = "Source reports pertinent data", 
         y = "Cumulative infection rate") 

limited %>% 
    filter(covid_rate < 1) %>% 
    boxplot(., "Releases", "covid_rate") + 
    labs(title = "Relationship between releases and COVID rates", 
         x = "Releases", 
         y = "Cumulative infection rate") 

# ------------------------------------------------------------------------------
# Population plots 
# ------------------------------------------------------------------------------

limited %>% 
    boxplot(., "Releases", "pop_coalesce") + 
    scale_y_continuous(trans = "log", label = comma) + 
    labs(title = "Relationship between releases and population", 
         x = "Releases", 
         y = "County jail population (log)")

limited %>% 
    rename("Source reports pertinent data" = source_reports_covid) %>% 
    boxplot(., "Source reports pertinent data", "pop_coalesce") + 
    scale_y_continuous(trans = "log", label = comma) + 
    labs(title = "Relationship between reporting and population", 
         x = "Source reports pertinent data", 
         y = "County jail population (log)")

# ------------------------------------------------------------------------------
# Population reduction plots 
# ------------------------------------------------------------------------------

limited %>% 
    boxplot(., "Releases", "pct_red") + 
    labs(title = "Relationship between population reduction and releases", 
         x = "Releases", 
         y = "Wave 1 (Feb-May) reduction")

limited %>% 
    rename("Source reports pertinent data" = source_reports_covid) %>% 
    boxplot(., "Source reports pertinent data", "pct_red") + 
    labs(title = "Relationship between reduction and reporting", 
         x = "Source reports pertinent data", 
         y = "Wave 1 (Feb-May) reduction")

limited %>% 
    arrange(source_reports_covid) %>% 
    rename("Source reports pertinent data" = source_reports_covid) %>% 
    log_scatter(., "pop_prior_coalesce", "pct_red", "Source reports pertinent data") + 
    scale_x_continuous(trans = "log", label = comma) +
    scale_y_continuous(label = percent) + 
    labs(x = "Prior jail population (log)", 
         y = "Wave 1 (Feb-May) reduction", 
         title = "Relationship between reduction and reporting") 

limited %>% 
    arrange(source_reports_covid) %>% 
    log_scatter(., "pop_prior_coalesce", "pct_red", "Releases") + 
    scale_x_continuous(trans = "log", label = comma) +
    scale_y_continuous(label = percent) + 
    labs(x = "Prior jail population (log)", 
         y = "Wave 1 (Feb-May) reduction", 
         title = "Relationship between reduction and releases") 

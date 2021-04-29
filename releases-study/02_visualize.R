library(behindbarstools)
library(tidyverse)
library(janitor)
library(scales)

limited <- read.csv("data/interim/releases-transparency-cleaned.csv", na = "")

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

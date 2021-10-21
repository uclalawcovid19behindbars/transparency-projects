library(behindbarstools)
library(tidyverse)
library(janitor)
library(scales)
library(urbnmapr)

limited <- read.csv("data/interim/releases-transparency-cleaned.csv") %>% 
    mutate(fips = as.character(fips)) %>% 
    mutate(fips = stringr::str_pad(fips, 5, pad = "0"))

joined <- urbnmapr::counties %>% 
    left_join(limited, by = c("county_fips" = "fips"))

joined %>%
    mutate(source_reports_covid = replace_na(source_reports_covid, "Not sampled")) %>% 
    ggplot(aes(long, lat, group = group, fill = source_reports_covid)) +
    geom_polygon(color = "white", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) + 
    theme_void(base_size = 14) + 
    scale_fill_manual(
        limits = c("Y", "N", "Not sampled"), 
        labels = c("Reported", "Did not report", "Not sampled"), 
        values = c("#4C6788", "#D7790F", "#e3e3e3")) + 
    theme(legend.title = element_blank(), 
          legend.position = "right", 
          legend.box.spacing = unit(-4.0, "cm")) + 
    labs(title = "Counties with source reporting pertinent data", 
         subtitle = "Pertinent data includes COVID cases, deaths, tests and/or releases")


# Releases Transparency Study 

Contains code and data to support the releases tab in their transparency study. 

`01_clean.R` takes the raw data (originally provided in an Excel sheet, later updated to [this Google Sheet](https://docs.google.com/spreadsheets/d/1sgVo2Wv6EOyjGlKrDjZhwVq6g1HgxAw4VhBNJpTHuXA/edit#gid=1328934180&fvid=975927745)), does some cleaning and joining with other data sources (e.g. NYT general population COVID data, Vera jail population data, BJS jail population data), and then writes the cleaned version to `data/interim/releases-transparency-cleaned.csv`. All data is either pulled from a public GitHub repo, or included in the `data/raw` directory. 

`02_visualize.R` creates plots from the cleaned data, where the initial version of these is available via Google Slides [here](https://docs.google.com/presentation/d/1tfMlQhwA6tC3OZprfble9tSzxUKfrYzON_3uUoHaNew/edit#slide=id.p). `03_map.R` creates the map included in the deck. 
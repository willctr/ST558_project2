# NFL Play-by-Play Explorer (2009-2016) 
A Shiny app for exploring  about 250,000 NFL passes, runs, and sacks from 2009-2016. 
Users can subset the data, download it, and build their own tables and plots.

**Live app:** https://wlcarte2.shinyapps.io/nfl-pbp-explorer/

**Data:** [Kaggle: NFL Play-by-Play 2009-2016](https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016). 
`data_process.R` cleans the raw CSV (not in repo) into `app/nfl_pbp.rds`. 

**Files** 
- `data_process.R`: data cleaning 
- `eda.qmd`: static exploration (tables, summaries, plots) 
-  app/app.R`: the Shiny app


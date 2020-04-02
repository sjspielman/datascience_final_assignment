library(tidyverse)
library(lubridate) ## a tidyverse (but not core tidyverse) package for working with dates more easily
library(viridis)

## Define global variables ----------------------------------------------

## URL where New York Times data is stored and regularly updated
nyt_usa_data_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

## URL where Johns Hopkins University data is stored and regularly updated
jhu_top_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
jhu_confirmed_global_url <- paste0(jhu_top_url, "time_series_covid19_confirmed_global.csv")
jhu_deaths_global_url    <- paste0(jhu_top_url, "time_series_covid19_deaths_global.csv")

## Your preferred viridis theme. Use in code as `scale_color_viridis(option = my_viridis_scheme)`
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
my_viridis_scheme <- "viridis" ## or, one of: magma, plasma, or inferno





## Read in all THREE datasets and tidy/wrangle them into one JHU and one NYT dataset --------------------------





# NOTE: You do NOT need to save any data!! Never use write_csv()!! The two variables you create can be *directly used* in the shiny app, since this file is sourced!! PLEASE DELETE THIS COMMENT BEFORE SUBMITTING THANKS!!!
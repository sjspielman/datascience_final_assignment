library(tidyverse)
library(lubridate) ## a tidyverse (but not core tidyverse) package for working with dates more easily
library(viridis)

## Define global variables ----------------------------------------------

## viridis options, see: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
viridis_scheme_options <- c("viridis", "magma", "plasma", "inferno")

## array of USA states (for use with NYT data)
usa_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District, of, Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New, Hampshire", "New, Jersey", "New, Mexico", "New, York", "North, Carolina", "North, Dakota", "Northern, Mariana, Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto, Rico", "Rhode, Island", "South, Carolina", "South, Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virgin, Islands", "Virginia", "Washington", "West, Virginia", "Wisconsin", "Wyoming")

# array of world countries/regions (for use with JHU data)
world_countries_regions <- 

## URL where New York Times data is stored and regularly updated
nyt_usa_data_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

## URL where Johns Hopkins University data is stored and regularly updated
jhu_top_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
jhu_confirmed_global_url <- paste0(jhu_top_url, "time_series_covid19_confirmed_global.csv")
jhu_deaths_global_url    <- paste0(jhu_top_url, "time_series_covid19_deaths_global.csv")






## Read in all THREE datasets and tidy/wrangle them into one JHU and one NYT dataset --------------------------





# NOTE: You do NOT need to save any data!! Never use write_csv()!! The two variables you create can be *directly used* in the shiny app, since this file is sourced!! PLEASE DELETE THIS COMMENT BEFORE SUBMITTING THANKS!!!
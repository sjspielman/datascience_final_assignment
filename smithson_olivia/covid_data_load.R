library(tidyverse)
library(lubridate) ## a tidyverse (but not core tidyverse) package for working with dates more easily


## Define global variables ----------------------------------------------

## viridis options, see: https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html


## array of USA states (for use with NYT data)
usa_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

# array of world countries/regions (for use with JHU data)
world_countries_regions <- c("Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina", "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burma", "Burundi", "Cabo Verde", "Cambodia", "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Congo (Brazzaville)", "Congo (Kinshasa)", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czechia", "Denmark", "Diamond Princess", "Djibouti", "Dominica", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea", "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany", "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Holy See", "Honduras", "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Korea, South", "Kosovo", "Kuwait", "Kyrgyzstan", "Laos", "Latvia", "Lebanon", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Mauritania", "Mauritius", "Mexico", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco", "Mozambique", "MS Zaandam", "Namibia", "Nepal", "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "North Macedonia", "Norway", "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal", "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", "San Marino", "Saudi Arabia", "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Somalia", "South Africa", "South Sudan", "Spain", "Sri Lanka", "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan*", "Tanzania", "Thailand", "Timor-Leste", "Togo", "Trinidad and Tobago", "Tunisia", "Turkey", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "Uruguay", "US", "Uzbekistan", "Venezuela", "Vietnam", "West Bank and Gaza", "Western Sahara", "Zambia", "Zimbabwe")

## URL where New York Times data is stored and regularly updated
nyt_usa_data_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

## URL where Johns Hopkins University data is stored and regularly updated
jhu_top_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"
jhu_confirmed_global_url <- paste0(jhu_top_url, "time_series_covid19_confirmed_global.csv")
jhu_deaths_global_url    <- paste0(jhu_top_url, "time_series_covid19_deaths_global.csv")


## Read in all THREE datasets and tidy/wrangle them into one JHU and one NYT dataset according to the instructions --------------------------

nyt_raw <- read_csv(nyt_usa_data_url)
nyt_raw %>%
  pivot_longer(cases:deaths, names_to = "covid_type", values_to = "cumulative_number") -> nyt_data

nyt_data %>%
  mutate(cumulative_number = if_else(cumulative_number == 0, 1e-10, cumulative_number)) -> nyt_data2


########## jhu_data ###################

##Make one tibble with JHU data
##Start with the confirmed data url first
##Read in the data
jhu_confirmed_raw <- read_csv(jhu_confirmed_global_url)

##tidying of jhu_confirmed_raw
jhu_confirmed_raw %>%
  ##alter cumulative number to make its log plottable
  #mutate(cumulative_number = if_else(cumulative_number == 0, 1e-10, cumulative_number)) %>%
  #create a new column for cases 
  mutate(covid_type = "cases") %>%
  #rename some column titles
  rename(province_or_state = `Province/State`, 
         country_or_region = `Country/Region`,
         latitude =Lat,
         longitude =Long) %>%
  #maintain date accuracy
  pivot_longer(c(-province_or_state, -country_or_region, -latitude, -longitude, -covid_type),
               names_to = "date",
               values_to = "cumulative number") -> cases_jhu

##Next is the data on number of deaths
##Read in the data
jhu_deaths <- read_csv(jhu_deaths_global_url)

##tidying of the jhu_deaths
jhu_deaths %>%
  ##alter cumulative number to make its log plottable
  #mutate(cumulative_number = if_else(cumulative_number == 0, 1e-10, cumulative_number)) %>%
  #create a new column for deaths
  mutate(covid_type = "deaths") %>%
  #rename some column titles
  #Province and State, and Country and Region
  rename(province_or_state = `Province/State`, 
         country_or_region = `Country/Region`,
         latitude =Lat,
         longitude =Long) %>%
  #fix dates so there is no X in front of them, and separate by /
  #maintain date accuracy - same as before!
  pivot_longer(c(-province_or_state, -country_or_region, -latitude, -longitude, -covid_type),
               names_to = "date",
               values_to = "cumulative number") -> deaths_jhu

##Now we combine the two dataframes
#can use full join or bind rows, up to preference
#name it to the tibble name on rubric
cases_jhu %>%
  full_join(deaths_jhu) -> jhu_data


#recast date column
jhu_data$date <- lubridate::mdy(jhu_data$date)




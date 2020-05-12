nyt_data %>%
  filter( state== "Alabama") %>%
  group_by(date,county, covid_type) %>%
  summarise(total_county_day= sum(cumulative_number)) %>%
  ggplot(aes(x=date, y=total_county_day, color=covid_type, group=county)) +
  geom_point() +
  geom_line() +
  theme_grey()

#nick helpful slack note
theme(axis.text = element_text(size = 12), ## labels on the axis
      axis.title = element_text(size = 14, face = "bold"), ## title of axis
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5))




jhu_data %>%
  filter( Country_or_Region== "Sweden") %>%
  group_by(date, Province_or_State, covid_type) %>%
  summarise(total_county_day= sum(cumulative_number)) %>%
  ggplot(aes(x=date, y=total_county_day, color=covid_type, group=Province_or_State)) +
  geom_point() +
  geom_line() +
  theme_grey()


#lubridate notes
d <- as_date(17498)
## "2017-11-28"

#ymd_hms(), ymd_hm(), ymd_h().
#ymd_hms("2017-11-28T14:02:00") 

#mdy_hms(), mdy_hm(), mdy_h().
#mdy_hms("11/28/2017 1:02:03") 



library(plotly)
iris %>%
  ggplot(aes(x=Species, y=Sepal.Length)) +
  geom_point()-> prac

ggplotly(prac)



fig1 <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
fig1
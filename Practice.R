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
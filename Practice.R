nyt_data %>%
  filter( state== "Alabama") %>%
  group_by(date,county, covid_type) %>%
  summarise(total_county_day= sum(cumulative_number)) %>%
  ggplot(aes(x=date, y=total_county_day, color=covid_type, group=county)) +
  geom_point() +
  geom_line()

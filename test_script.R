nyt_data %>%
  filter(state == "Alabama") %>%
  group_by(date, covid_type) %>%
  summarize(total_county_day = sum(cumulative_number)) %>%
  ggplot(aes(x = date, y = total_county_day, 
             color = covid_type, group = covid_type)) +
  geom_point() +
  geom_line()


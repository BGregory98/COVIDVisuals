state_data %>% 
  group_by(date, state, region)%>%
  filter(state_abs %in% 'AZ') %>%
  summarize(weekly_new_tests = sum(weekly_new_tests),
            weekly_new_cases = sum(weekly_new_cases))%>%
  mutate(weekly_pos_rate = (weekly_new_cases/weekly_new_tests)*100) %>%
  filter(date > as.Date('2020-04-01'))%>%
  ggplot(., aes(x = date, y = weekly_pos_rate, color = state, fill = state)) +
  geom_area(size = 1, alpha = 0.5) +
  #geom_line(aes(y = national_daily_cases)) +
  #scale_color_brewer(palette = 'Spectral', name = 'Region', direction = 1) +
  theme_bw() +
  labs(x = 'Date', y = 'Percent of Tests Positive', title = 'US State COVID-19 Test Positivity Curves') +
  theme(legend.position = 'none', axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 14))+
  theme(plot.title = element_text(size = 16)) +
  facet_wrap(facets = .~state) +
  coord_cartesian(ylim = c(0,55))

p2 <- state_data %>% 
  group_by(date, subregion)%>%
  filter(state_abs %in% .us) %>%
  summarize(weekly_new_tests = sum(weekly_new_tests),
            weekly_new_cases = sum(weekly_new_cases))%>%
  mutate(weekly_pos_rate = (weekly_new_cases/weekly_new_tests)*100) %>%
  filter(date > as.Date('2020-03-15'))%>%
  ggplot(., aes(x = date, y = weekly_pos_rate, color = subregion, fill = subregion)) +
  geom_area(size = 2, alpha = 0.5) +
  #geom_line(aes(y = national_daily_cases)) +
  #scale_color_brewer(palette = 'Spectral', name = 'Region', direction = 1) +
  theme_bw() +
  labs(x = 'Date', y = 'Percent of Weekly Tests Positive', title = 'Regional COVID-19 Test Positivity Curves') +
  theme(legend.position = 'none', axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 14))+
  theme(plot.title = element_text(size = 18)) +
  facet_wrap(facets = .~subregion) +
  coord_cartesian(ylim = c(0,45))

p3 <- plot_usmap(data = state_data_summary, value = 'subregion', alpha = 0.5) +
  theme(legend.position = 'none',
        plot.title = element_text(hjust = 0.5),
  )

multiplot(p2, p3)                  

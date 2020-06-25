state_order <- function(var, var2){
max_state_data <- max_state_data %>%
  arrange(desc({{var}}),desc({{var2}}))
state_order <- as.vector((max_state_data$state))
return (state_order)
}

cbsa_order <- function(var){
  max_cbsa_data <- max_cbsa_data %>%
    arrange(desc({{var}}))
  cbsa_order <- as.vector((max_cbsa_data$cbsa))
  return (cbsa_order)
}

state_order <- state_order(peak_date, current_weekly_avg_daily_cases_10k)
cbsa_order <- cbsa_order(how_long_ago)

state_data %>%
  group_by(date) %>%
  summarize(weekly_avg_daily_cases = sum(weekly_avg_daily_cases, na.rm = T)) %>%
  mutate(state = 'US') %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(x = date, y = state)) +
  geom_tile(aes(fill = weekly_avg_daily_cases)) +
  scale_fill_viridis(option = 'C') +
  theme_nothing()

# Heat map of case growth rate by state
state_data$state<-factor(state_data$state, levels=(state_order))
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state)) +
  # Geom tile base
  geom_tile(aes(fill=pct_max_weekly_avg_daily_cases), color = 'black') +
  # Spectral fill for proportion of peak daily cases
  scale_fill_distiller(palette = "Spectral",
                       name = 'Proportion of Peak\nDaily Cases',
                       na.value = 'lightgrey') +
  # Plot labels
  labs(x = 'Date', y = 'State',
       title = 'COVID-19 Daily Cases by US State',
       subtitle = 'Ordered by Peak Date, Growth Rates Normalized Within States',
       caption = 'benpgregory@gmail.com') +
  # Y axis text size
  theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  # Region labels #
  geom_text(data = . %>% filter(date == (max(state_data$date))), aes(label=region),
             size = 3, nudge_x = -116, vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(state_data$date))),
            aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_date)),
             size = 0.5,
             color = 'white',
             alpha = 1, stroke = 1.5) +
  # Size scale for points
  scale_size_continuous(name = 'Peak Daily Cases\n(per 10k People)',
                        range = c(0,5)) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'right',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))


cbsa_data_summary <- cbsa_data_summary %>%
  arrange(desc(peak_date))

# Heat map of case growth rate by metro area
cbsa_data$cbsa_short<-factor(cbsa_data$cbsa_short, levels=(cbsa_order))
cbsa_data %>%
  filter(date > '2020-03-01')%>%
  filter(pop > 250000) %>%
  ggplot(., aes(date, cbsa_short)) +
  # Geom tile base
  geom_tile(aes(fill=pct_max_weekly_avg_daily_cases), color = 'black') +
  # Spectral fill for proportion of peak daily cases
  scale_fill_distiller(palette = "Spectral",
                       name = 'Proportion of Peak\nDaily Cases',
                       na.value = 'lightgrey') +
  # Plot labels
  labs(x = 'Date', y = 'Metro Area',
       title = 'COVID-19 Daily Cases by US Metro Area',
       subtitle = paste0('Ordered by Peak Date, Growth Rates Normalized Within Metro Areas\nData: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y')),
       caption = 'benpgregory@gmail.com') +
  # Y axis text size
  theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  # Region labels #
  geom_text(data = . %>% filter(date == (max(cbsa_data$date))), aes(label=region),
            size = 3, nudge_x = -116, vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(cbsa_data$date))),
            aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_date)),
             size = 0.5,
             color = 'white',
             alpha = 1, stroke = 1.5) +
  # Size scale for points
  scale_size_continuous(name = 'Peak Daily Cases\n(per 10k People)',
                        range = c(0,5)) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'right',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))

# Heat map of hospitalization by state
state_data$state<-factor(state_data$state, levels=(state_order))
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state)) +
  # Geom tile base
  geom_tile(aes(fill=pct_max_hospitalized), color = 'black') +
  # Spectral fill for proportion of peak daily cases
  scale_fill_distiller(palette = "Spectral",
                       name = 'Proportion of Peak\nHospitalizations',
                       na.value = 'lightgrey') +
  # Plot labels
  labs(x = 'Date', y = 'State',
       title = 'COVID-19 Hospitalizations by US State',
       subtitle = 'Ordered by Peak Date, Hospitalization Rates Normalized Within States',
       caption = 'benpgregory@gmail.com') +
  # Y axis text size
  theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  # Region labels #
  geom_text(data = . %>% filter(date == (max(state_data$date))), aes(label=region),
            size = 3, nudge_x = -116, vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(state_data$date))),
            aes(label=format(peak_hosp_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_hosp_date)),
             size = 1.5,
             color = 'white', shape = 1,
             alpha = 1, stroke = 1.5) +
  # Size scale for points
  scale_size_continuous(name = 'Peak Daily Cases\n(per 10k People)',
                        range = c(0,5)) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'right',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))


# Ridge curves
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state)) +
  geom_ridgeline(aes(height=pct_max_weekly_avg_daily_cases, fill = region),
                 color = 'black', scale = 2, alpha = .4, size = 1)+
  scale_fill_brewer(palette = 'YlGnBu', direction = -1, name = 'Region', na.value = 'grey') +
  theme_minimal() +
  theme(legend.position = 'right') +
  geom_vline(xintercept = max(state_data$date), linetype = 'dashed') +
  geom_point(data = . %>% filter(date == (peak_date)), size = 2,
             fill = 'white', shape = 21, color = 'black',
             alpha = 1, position = position_nudge(y = 2), stroke  = 1.5) +
  scale_size_continuous(range = c(1,5), name = 'Peak Daily Cases\n(per 10k People)')+
  #scale_color_distiller(palette = 'YlOrRd', name = 'Days Since Peak', direction = -1) +
  labs(title = 'United States COVID-19 Growth Curves', y = 'State', x = 'Date',
       subtitle = paste0('States sorted by peak date, data normalized across states\nData: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y')),
       caption = 'benpgregory@gmail.com') +
  geom_text(data = . %>% filter(date == (max(state_data$date))), aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 7, vjust = -0.25) +
  theme(axis.text.y = element_text(vjust = -0.25, size = 10)) +
  theme(axis.text.x = element_text(size = 10), axis.title = element_text(size = 12))

# Ridge curves
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state)) +
  geom_ridgeline(aes(height=pct_max_hospitalized, fill = region),
                 color = 'black', scale = 2, alpha = .3, size = 1)+
  scale_fill_brewer(palette = 'YlGnBu', direction = -1, name = 'Region', na.value = 'grey') +
  theme_minimal() +
  theme(legend.position = 'right') +
  geom_point(data = . %>% filter(date == (peak_hosp_date)),
             fill = 'white', shape = 21, color = 'black',
             alpha = 1, position = position_nudge(y = 2), stroke  = 1.5) +
  scale_size_continuous(range = c(1,4), name = 'Peak Daily Cases\n(per 10k People)')+
  #scale_color_distiller(palette = 'YlOrRd', name = 'Days Since Peak', direction = -1) +
  labs(title = 'United States COVID-19 Growth Curves', y = 'State', x = 'Date',
       subtitle = paste0('States sorted by peak date, data normalized across states\nData: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  geom_text(data = . %>% filter(date == (max(state_data$date))), aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 7, vjust = -0.25) +
  theme(axis.text.y = element_text(vjust = -0.25, size = 10)) +
  theme(axis.text.x = element_text(size = 10))
 
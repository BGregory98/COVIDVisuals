# First create regional cumulative and daily case data
state_data_stacked <- state_data %>%
  group_by(date, region)%>%
  summarize(cases = sum(cases),
            daily_cases = sum(daily_cases))
# Then create national cumulative and daily case data
state_data_total <- state_data %>%
  group_by(date)%>%
  summarize(national_cases = sum(cases),
            national_daily_cases = sum(daily_cases))

# Then join the regional and national data
state_data_stacked <- full_join(state_data_stacked, state_data_total, by = 'date')

# Then calculate the regional proportions of national data
state_data_stacked <- state_data_stacked %>%
  mutate(pct_total_cases = (cases/national_cases)) %>%
  mutate(pct_daily_cases = (daily_cases/national_daily_cases))

# Then calculate weekly averages
state_data_stacked <- state_data_stacked %>%
  group_by(region) %>%
  mutate(weekly_avg_pct_daily_cases = frollmean(pct_daily_cases,7)) %>%
  mutate(weekly_avg_daily_cases = frollmean(daily_cases,7))

# Then calculate populations of each region
regional_pops <- state_data %>%
  group_by(region)
  
temp2 <- state_data %>%
  filter(state_abs %in% (.us)) %>%
  group_by(state_abs)%>%
  summarize(pop = tail(pop,1))

pop <- sum(temp2$pop)

temp <- state_data %>%
  filter(state_abs %in% (.us)) %>%
  group_by(date) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths),
            daily_cases = sum(daily_cases),
            daily_deaths = sum(daily_deaths)) %>%
  mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
  mutate(chgrate = (weekly_avg_daily_cases/(lag(weekly_avg_daily_cases,7)))-1)

curtotal <- tail(temp$cases, 1)
curtotalcap <- round((curtotal/pop)*10000,1)
currate <- round(tail(temp$weekly_avg_daily_cases, 1),0)
chgrate <- round((tail(temp$chgrate, 1))*100,2)

if (chgrate > 0){
  direction <- 'Up '
} else {
  direction <- 'Down '
}

chgrate <- abs(chgrate)

curtotal <- format((curtotal),big.mark=",",scientific=FALSE)
currate <- format((currate),big.mark=",",scientific=FALSE)
curtotalcap <- format((curtotalcap),big.mark=",",scientific=FALSE)

state_data_stacked$region<-factor(state_data_stacked$region, levels=c('South', 'West', 'Midwest', 'Northeast'))
state_data_summary$region<-factor(state_data_summary$region, levels=c('South', 'West', 'Midwest', 'Northeast'))


a1<-ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_pct_daily_cases, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_cases)) +
  scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = 'Proportion of National Daily Cases') +
  theme(legend.position = 'none', axis.text.y = element_text(size = 9))+
    theme(plot.title = element_text(size = 12)))

a2 <- ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_cases, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_cases)) +
  scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
  theme_bw() +
  labs(x = 'Date', y = 'Daily Cases',
       title = 'Daily COVID-19 Cases',
       subtitle = paste0(curtotal, ' Total Cases (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                         currate, ' new cases/day\n', direction, chgrate, '% from 7 days ago\n'),
       caption = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  theme(legend.position = 'right', axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 14))+
  theme(plot.title = element_text(size = 24))

a4 <- ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_cases, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_cases)) +
  scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),max(state_data$date))) +
  theme_bw() +
  labs(x = NULL, y = NULL, title = 'Regional Curves') +
  theme(legend.position = 'none', axis.text.y = element_blank(),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  theme(plot.title = element_text(size = 12)) +
  facet_grid(facets = region~.) +
  theme(
    strip.background = element_blank(),
    strip.text.y = element_blank()
  ))

a3 <- ggplotGrob(plot_usmap(data = state_data_summary, value = 'region') +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5),
                         ))

a2 + annotation_custom(
  grob = a1,
  xmin = max(state_data$date)-43,
  xmax = max(state_data$date)-5,
  ymin = 24500,
  ymax = 33000
) + annotation_custom(
  grob = a3,
  xmin = as.Date('2020-02-29'),
  xmax = as.Date('2020-04-05'),
  ymin = 22500,
  ymax = 33000
) + annotation_custom(
  grob = a4,
  xmin = as.Date('2020-03-03'),
  xmax = as.Date('2020-03-21'),
  ymin = 5000,
  ymax = 23000
)

#######################

# First create regional cumulative and daily case data
state_data_stacked <- state_data %>%
  group_by(date, region)%>%
  summarize(deaths = sum(deaths, na.rm = T),
            daily_deaths = sum(daily_deaths, na.rm = T))
# Then create national cumulative and daily case data
state_data_total <- state_data %>%
  group_by(date)%>%
  summarize(national_deaths = sum(deaths, na.rm = T),
            national_daily_deaths = sum(daily_deaths, na.rm = T))

# Then join the regional and national data
state_data_stacked <- full_join(state_data_stacked, state_data_total, by = 'date')

# Then calculate the regional proportions of national data
state_data_stacked <- state_data_stacked %>%
  mutate(pct_total_deaths = (deaths/national_deaths)) %>%
  mutate(pct_daily_deaths = (daily_deaths/national_daily_deaths))

# Then calculate weekly averages
state_data_stacked <- state_data_stacked %>%
  group_by(region) %>%
  mutate(weekly_avg_pct_daily_deaths = frollmean(pct_daily_deaths,7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths,7))

# Then calculate populations of each region
regional_pops <- state_data %>%
  group_by(region)

temp2 <- state_data %>%
  filter(state_abs %in% (.us)) %>%
  group_by(state_abs)%>%
  summarize(pop = tail(pop,1))

pop <- sum(temp2$pop)

temp <- state_data %>%
  filter(state_abs %in% (.us)) %>%
  group_by(date) %>%
  summarize(deaths = sum(deaths),
            daily_deaths = sum(daily_deaths)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
  mutate(chgrate = (weekly_avg_daily_deaths/(lag(weekly_avg_daily_deaths,7)))-1)

curtotal <- tail(temp$deaths, 1)
curtotalcap <- round((curtotal/pop)*10000,1)
currate <- round(tail(temp$weekly_avg_daily_deaths, 1),0)
chgrate <- round((tail(temp$chgrate, 1))*100,2)

if (chgrate > 0){
  direction <- 'Up '
} else {
  direction <- 'Down '
}

chgrate <- abs(chgrate)

curtotal <- format((curtotal),big.mark=",",scientific=FALSE)
currate <- format((currate),big.mark=",",scientific=FALSE)
curtotalcap <- format((curtotalcap),big.mark=",",scientific=FALSE)

state_data_stacked$region<-factor(state_data_stacked$region, levels=c('South', 'West', 'Midwest', 'Northeast'))
state_data_summary$region<-factor(state_data_summary$region, levels=c('South', 'West', 'Midwest', 'Northeast'))


a1<-ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_pct_daily_deaths, fill = region)) +
                 geom_area(color = 'black') +
                 #geom_line(aes(y = national_daily_deaths)) +
                 scale_fill_brewer(palette = 'Spectral', name = 'Region', direction = -1) +
                 #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                 coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
                 theme_bw() +
                 labs(x = NULL, y = NULL, title = 'Proportion of National Daily Deaths') +
                 theme(legend.position = 'none', axis.text.y = element_text(size = 9))+
                 theme(plot.title = element_text(size = 12)))

a2 <- ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_deaths, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_deaths)) +
  scale_fill_brewer(palette = 'Spectral', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
  theme_bw() +
  labs(x = 'Date', y = 'Daily Deaths',
       title = 'Daily COVID-19 Deaths',
       subtitle = paste0(curtotal, ' Total Deaths (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                         currate, ' new deaths/day\n', direction, chgrate, '% from 7 days ago\n'),
       caption = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  theme(legend.position = 'right', axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 14))+
  theme(plot.title = element_text(size = 24))

a4 <- ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_deaths, fill = region)) +
                   geom_area(color = 'black') +
                   #geom_line(aes(y = national_daily_deaths)) +
                   scale_fill_brewer(palette = 'Spectral', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   coord_cartesian(xlim = c(as.Date('2020-03-05'),max(state_data$date))) +
                   theme_bw() +
                   labs(x = NULL, y = NULL, title = 'Regional Curves') +
                   theme(legend.position = 'none', axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 9),
                         axis.title = element_text(size = 14),
                         axis.ticks.y = element_blank(),
                         axis.line.y = element_blank())+
                   theme(plot.title = element_text(size = 12)) +
                   facet_grid(facets = region~.) +
                   theme(
                     strip.background = element_blank(),
                     strip.text.y = element_blank()
                   ))

a3 <- ggplotGrob(plot_usmap(data = state_data_summary, value = 'region') +
                   scale_fill_brewer(palette = 'Spectral', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5),
                   ))

a2 + annotation_custom(
  grob = a1,
  xmin = max(state_data$date)-40,
  xmax = max(state_data$date),
  ymin = 1500,
  ymax = 2200
) + annotation_custom(
  grob = a3,
  xmin = as.Date('2020-02-29'),
  xmax = as.Date('2020-04-05'),
  ymin = 1250,
  ymax = 2200
) + annotation_custom(
  grob = a4,
  xmin = as.Date('2020-03-03'),
  xmax = as.Date('2020-03-21'),
  ymin = 200,
  ymax = 1250
)

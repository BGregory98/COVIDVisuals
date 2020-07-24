# How long ago was March 1?
dayssince <- as.numeric(today() - as.Date('2020-03-01'))


#### HEAT WAVE STATE CASES ####
# Heat map of case growth rate by state
state_order <- state_order_function(peak_date, current_weekly_avg_daily_cases_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p1 <- state_data %>%
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
            size = 3, nudge_x = (dayssince*-1.038), vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(state_data$date))),
            aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_date)),
             size = 0.5,
             color = 'white',
             alpha = 1, stroke = 1.5) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'bottom',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))

ggsave(filename = paste0(format(today(), '%m%d'), 'HeatWaveStateCases', '.png'), plot = p1, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 10, height = 12, units = "in", dpi = 300)

#### HEAT WAVE STATE DEATHS ####
# Heat map of death growth rate by state
state_order <- state_order_function(peak_death_date, current_weekly_avg_daily_cases_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p2 <- state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state)) +
  # Geom tile base
  geom_tile(aes(fill=pct_max_weekly_avg_daily_deaths), color = 'black') +
  # Spectral fill for proportion of peak daily cases
  scale_fill_distiller(palette = "Spectral",
                       name = 'Proportion of Peak\nDaily Deaths',
                       na.value = 'lightgrey') +
  # Plot labels
  labs(x = 'Date', y = 'State',
       title = 'COVID-19 Daily Deaths by US State',
       subtitle = 'Ordered by Peak Date, Growth Rates Normalized Within States',
       caption = 'benpgregory@gmail.com') +
  # Y axis text size
  theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  # Region labels #
  geom_text(data = . %>% filter(date == (max(state_data$date))), aes(label=region),
            size = 3, nudge_x = (dayssince*-1.038), vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(state_data$date))),
            aes(label=format(peak_death_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_death_date)),
             size = 0.5,
             color = 'white',
             alpha = 1, stroke = 1.5) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'bottom',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))

ggsave(filename = paste0(format(today(), '%m%d'), 'HeatWaveStateDeaths', '.png'), plot = p2, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 10, height = 12, units = "in", dpi = 300)

#### HEAT WAVE STATE HOSPITALIZATIONS ####
# Heat map of hospitalization by state
state_order <- state_order_function(peak_hosp_date, current_hospitalized_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p3 <- state_data %>%
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
            size = 3, nudge_x = (dayssince*-1.038), vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(state_data$date))),
            aes(label=format(peak_hosp_date, "%b %d")),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  geom_point(data = . %>% filter(date == (peak_hosp_date)),
             size = 0.5,
             color = 'white',
             alpha = 1, stroke = 1.5) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'bottom',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))

ggsave(filename = paste0(format(today(), '%m%d'), 'HeatWaveStateHospitalizations', '.png'), plot = p3, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 10, height = 12, units = "in", dpi = 300)

#### HEAT WAVE CBSA CASES ####
# Heat map of case growth rate by metro area
cbsa_order <- cbsa_order_function(peak_date, cwa_daily_cases_10k)
cbsa_data$cbsa_short<-factor(cbsa_data$cbsa_short, levels=(cbsa_order))
p4 <- cbsa_data %>%
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
            size = 3, nudge_x = (dayssince*-1.038), vjust = 0.5, alpha = .75, hjust = 0.5) +
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

ggsave(filename = paste0(format(today(), '%m%d'), 'HeatWaveCBSACases', '.png'), plot = p4, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 12, height = 24, units = "in", dpi = 300)

#### HEAT WAVE CBSA CASES PER CAPITA ####
# Heat map of case growth rate by metro area
cbsa_order <- cbsa_order_function(cwa_daily_cases_10k, cwa_daily_cases_10k)
cbsa_data$cbsa_short<-factor(cbsa_data$cbsa_short, levels=(cbsa_order))
p4 <- cbsa_data %>%
  filter(date > '2020-03-01')%>%
  filter(pop > 250000) %>%
  ggplot(., aes(date, cbsa_short)) +
  # Geom tile base
  geom_tile(aes(fill=weekly_avg_daily_cases_10k), color = 'transparent') +
  # Spectral fill for proportion of peak daily cases
  scale_fill_distiller(palette = "Spectral",
                       name = 'Daily Cases\nper 10k',
                       na.value = 'lightgrey') +
  #scale_fill_viridis(option = 'D') +
  # Plot labels
  labs(x = 'Date', y = 'Metro Area',
       title = 'COVID-19 Daily Cases per Capita by US Metro Area',
       subtitle = paste0('Ordered by Current Growth Rate\nData: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y')),
       caption = 'benpgregory@gmail.com') +
  # Y axis text size
  theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  # Region labels #
  geom_text(data = . %>% filter(date == (max(cbsa_data$date))), aes(label=region),
            size = 3, nudge_x = (dayssince*-1.038), vjust = 0.5, alpha = .75, hjust = 0.5) +
  # Peak date labels
  geom_text(data = . %>% filter(date == (max(cbsa_data$date))),
            aes(label=round(weekly_avg_daily_cases_10k,2)),
            size = 3, nudge_x = 2, vjust = 0.25, hjust = 0)+
  # Peak points sized by peak daily cases per 10k
  #geom_point(data = . %>% filter(date == (peak_date)),
  #           size = 0.5,
  #           color = 'white',
  #           alpha = 1, stroke = 1.5) +
  # Size scale for points
  #scale_size_continuous(name = 'Peak Daily Cases\n(per 10k People)',
  #                      range = c(0,5)) +
  # Plot theme
  theme_minimal() +
  # Legend position
  theme(legend.position = 'right',
        legend.key = element_rect(fill = '#D53E4F', color = 'black'))

ggsave(filename = paste0(format(today(), '%m%d'), 'HeatWaveCBSACasesperCapita', '.png'), plot = p4, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 12, height = 24, units = "in", dpi = 300)

#### TOTAL CASES PER CAPITA MAP ####
# Cases per capita
p5 <- covid_map(inc_vector = c(.us),
          value = 'total_cases_10k_class',
          no_cities = 0,
          length = 65,
          title = 'US Total Covid-19 Cases per Capita',
          legend.title = 'Total Cases\nper 10,000',
          caption = 'benpgregory@gmail.com',
          curve_title = 'Infection Curve',
          palette_type = 'brewer',
          color_palette = 'YlOrRd',
          palette_direction = 1,
          state_border_color = 'black',
          state_border_size = 1,
          full_labels = T,
          label.color = 'white',
          county_border_color = 'darkgrey',
          include_curve = F,
          na.translate = T,
          na.value = 'white',
          legend_text_size = 12,
          title_size = 22,
          subtitle_size = 14,
          subtitle = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y')),
          seed = 102,
          state_label_type = state_abs,
          state_label_size = 4,
          city_point_color = 'white',
          county_border_size = .4)

ggsave(filename = paste0(format(today(), '%m%d'), 'TotalCasesCapita', '.png'), plot = p5, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

#### DAILY CASES PER CAPITA MAP ####
# Current growth rate per million people
p6 <- covid_map(inc_vector = c(.us),
          value = 'current_weekly_cases_10k_class',
          no_cities = 0,
          length = 65,
          title = 'Where are COVID-19 Cases Growing Fastest?',
          legend.title = 'Weekly Cases\nper 10k',
          caption = 'benpgregory@gmail.com',
          curve_title = 'Infection Curve',
          palette_type = 'brewer',
          color_palette = 'OrRd',
          palette_direction = 1,
          state_border_color = 'black',
          state_border_size = 1,
          full_labels = T,
          label.color = 'white',
          county_border_color = 'darkgrey',
          include_curve = F,
          na.translate = F,
          na.value = 'black',
          title_size = 22,
          subtitle_size = 14,
          subtitle = paste0('Data: The New York Times\nCurrent for the Week of ', format(today()-8, "%b %d"), ' to ', format(today()-1, "%b %d")),
          seed = 102,
          state_label_type = state_abs,
          state_label_size = 4,
          city_point_color = 'white',
          county_border_size = .4)

ggsave(filename = paste0(format(today(), '%m%d'), 'DailyCasesCapita', '.png'), plot = p6, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

##### STACKED AREA CHART CASES BY REGION ####
#I really need to make this into a function
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
       caption = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'), '\nbenpgregory@gmail.com')) +
  theme(legend.position = 'none', axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 14))+
  theme(plot.title = element_text(size = 24))

state_data_stacked <- state_data_stacked[complete.cases(state_data_stacked[ , 2]), ]

a4 <- ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_cases, fill = region)) +
                   geom_area(color = 'black') +
                   #geom_line(aes(y = national_daily_cases)) +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   coord_cartesian(xlim = c(as.Date('2020-03-05'),max(state_data$date))) +
                   theme_bw() +
                   labs(x = NULL, y = NULL, title = 'Regional Curves') +
                   theme(legend.position = 'none',
                         #axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 9),
                         axis.title = element_text(size = 14),
                         axis.ticks.y = element_blank(),
                         axis.line.y = element_blank())+
                   theme(plot.title = element_text(size = 12)) +
                   facet_grid(facets = region~.) +
                   theme(
                     strip.background = element_blank(),
                     #strip.text.y = element_blank()
                   ))

a3 <- ggplotGrob(plot_usmap(data = state_data_summary, value = 'region') +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5),
                   ))

p7 <- a2 + annotation_custom(
  grob = a1,
  xmin = max(state_data$date)-70,
  xmax = max(state_data$date)-29,
  ymin = 28000,
  ymax = 60000
) + annotation_custom(
  grob = a3,
  xmin = as.Date('2020-03-20'),
  xmax = as.Date('2020-05-10'),
  ymin = 32000,
  ymax = 64000
) + annotation_custom(
  grob = a4,
  xmin = as.Date('2020-03-01'),
  xmax = as.Date('2020-03-21'),
  ymin = 3150,
  ymax = 64000
)

p7
ggsave(filename = paste0(format(today(), '%m%d'), 'RegionalCurve', '.png'), plot = p7, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

#### JOY PLOT STATE CASES ####
# Ridge curves
state_order <- state_order_function(peak_date, current_weekly_avg_daily_cases_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p8 <- state_data %>%
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

ggsave(filename = paste0(format(today(), '%m%d'), 'JoyplotStatesCases', '.png'), plot = p8, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 10, height = 13, units = "in", dpi = 300)

#### MEAN CENTER OF WEEKLY CASES MAP ####
# Mean Center of Weekly New Cases Map
m1 <- covid_map(inc_vector = c(.us),
                value = 'total_cases_exp_class',
                no_cities = 0,
                length = 65,
                title = 'Total Covid-19 Cases by County',
                legend.title = 'Total Cases',
                curve_title = 'Infection Curve',
                palette_type = 'brewer',
                color_palette = 'PuBu',
                palette_direction = 1,
                state_border_color = 'black',
                state_border_size = .8,
                full_labels = T,
                label.color = 'white',
                county_border_color = 'lightgrey',
                include_curve = F,
                na.translate = F,
                na.value = 'white',
                seed = 800,
                state_label_type = state_abs,
                state_label_size = 4,
                alpha = 0.6)

end_index <- length(wtcent_county_data$date)
date_vector <- as.vector(seq(1, end_index, by=7))

p9 <- m1 +
  geom_point(data = pop_center, aes(x = lon.1, y = lat.1, size = date), fill = 'white',
             stroke = 2, color = '#B82D2A', shape = 8) +
  scale_size_discrete(name = NULL)+
  theme(legend.text = element_text(size = 12))+
  geom_line(data = wtcent_county_data,
            aes(x = lon.1, y = lat.1),
            alpha = .5, size = 2, color = 'black')+
  geom_point(data = wtcent_county_data[c(date_vector,length(wtcent_county_data$date)),],
             aes(x = lon.1, y = lat.1, color = date),
             alpha = 1, size = 3, shape = 21, fill = 'white', stroke = 3)+
  geom_point(data = wtcent_county_data[length(wtcent_county_data$date),],
             aes(x = lon.1, y = lat.1), shape = 21, color = '#440154', size = 10,
             fill = 'transparent', stroke = 2) +
  geom_point(data = wtcent_county_data[length(wtcent_county_data$date),],
             aes(x = lon.1, y = lat.1), shape = 21, color = '#440154', size = 13.5,
             fill = 'transparent', stroke = 2) +
  scale_color_viridis(option = 'D', discrete = T, name = 'Mean Center of\nWeekly Cases', direction = -1) +
  theme(legend.position = 'right') +
  labs(title = 'Mean Center of Weekly COVID-19 Infections Over Time',
       subtitle = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  theme(plot.title = element_text(size = 22, face = 'bold', hjust = 0),
        plot.subtitle = element_text(size = 16))

ggsave(filename = paste0(format(today(), '%m%d'), 'MeanCenterDailyCasesMap', '.png'), plot = p9, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

##### STACKED AREA CHART HOSPITALIZATIONS BY REGION ####
# First create regional cumulative and daily case data
state_data_stacked <- state_data %>%
  group_by(date, region)%>%
  summarize(hospitalized = sum(hospitalized, na.rm = T))
# Then create national cumulative and daily case data
state_data_total <- state_data %>%
  group_by(date)%>%
  summarize(national_hospitalized = sum(hospitalized, na.rm = T))
# Then join the regional and national data
state_data_stacked <- full_join(state_data_stacked, state_data_total, by = 'date')

# Then calculate the regional proportions of national data
state_data_stacked <- state_data_stacked %>%
  mutate(pct_total_hospitalized = (hospitalized/national_hospitalized))

# Then calculate weekly averages
state_data_stacked <- state_data_stacked %>%
  group_by(region) %>%
  mutate(weekly_avg_pct_hospitalized = frollmean(pct_total_hospitalized,7)) %>%
  mutate(weekly_avg_hospitalized = frollmean(hospitalized,7))

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
  summarize(hospitalized = sum(hospitalized, na.rm = T)) %>%
  mutate(weekly_avg_hospitalized = frollmean(hospitalized, 7)) %>%
  mutate(chgrate = (weekly_avg_hospitalized/(lag(weekly_avg_hospitalized,7)))-1)

currate <- round(tail(temp$weekly_avg_hospitalized, 1),0)
curratecap <- round((currate/pop)*10000,1)
chgrate <- round((tail(temp$chgrate, 1))*100,2)

if (chgrate > 0){
  direction <- 'Up '
} else {
  direction <- 'Down '
}

chgrate <- abs(chgrate)

currate <- format((currate),big.mark=",",scientific=FALSE)
curtotalcap <- format((curtotalcap),big.mark=",",scientific=FALSE)

state_data_stacked$region<-factor(state_data_stacked$region, levels=c('South', 'West', 'Midwest', 'Northeast'))
state_data_summary$region<-factor(state_data_summary$region, levels=c('South', 'West', 'Midwest', 'Northeast'))


a1<-ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_pct_hospitalized, fill = region)) +
                 geom_area(color = 'black') +
                 #geom_line(aes(y = national_daily_deaths)) +
                 scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                 #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                 coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
                 theme_bw() +
                 labs(x = NULL, y = NULL, title = 'Proportion of National Hospitalizations') +
                 theme(legend.position = 'none', axis.text.y = element_text(size = 9))+
                 theme(plot.title = element_text(size = 12)))

a2 <- ggplot(state_data_stacked, aes(x = date, y = weekly_avg_hospitalized, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_deaths)) +
  scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
  theme_bw() +
  labs(x = 'Date', y = 'Current Hospitalizations',
       title = 'COVID-19 Hospitalizations',
       subtitle = paste0(currate, ' Current Hospitalizations (', curratecap, ' per 10,000 people)\n', direction, chgrate, '% from 7 days ago\n'),
       caption = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  theme(legend.position = 'none', axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 14))+
  theme(plot.title = element_text(size = 24))

state_data_stacked <- state_data_stacked[complete.cases(state_data_stacked[ , 2]), ]

a4 <- ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_hospitalized, fill = region)) +
                   geom_area(color = 'black') +
                   #geom_line(aes(y = national_daily_deaths)) +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   coord_cartesian(xlim = c(as.Date('2020-03-05'),max(state_data$date))) +
                   theme_bw() +
                   labs(x = NULL, y = NULL, title = 'Regional Curves') +
                   theme(legend.position = 'none',
                         #axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 9),
                         axis.title = element_text(size = 14),
                         axis.ticks.y = element_blank(),
                         axis.line.y = element_blank())+
                   theme(plot.title = element_text(size = 12)) +
                   facet_grid(facets = region~.) +
                   theme(
                     strip.background = element_blank(),
                     #strip.text.y = element_blank()
                   ))

a3 <- ggplotGrob(plot_usmap(data = state_data_summary, value = 'region') +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5),
                   ))

p10 <- a2 + annotation_custom(
  grob = a1,
  xmin = max(state_data$date)-42,
  xmax = max(state_data$date)-8,
  ymin = 41000,
  ymax = 60000
) + annotation_custom(
  grob = a3,
  xmin = as.Date('2020-02-29'),
  xmax = as.Date('2020-04-05'),
  ymin = 40000,
  ymax = 61000
) + annotation_custom(
  grob = a4,
  xmin = as.Date('2020-03-03'),
  xmax = as.Date('2020-03-23'),
  ymin = 3000,
  ymax = 40000
)

p10

ggsave(filename = paste0(format(today(), '%m%d'), 'RegionalHospitalizationCurve', '.png'), plot = p10, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

##### STACKED AREA CHART DEATHS BY REGION ####
# First create regional cumulative and daily case data
state_data_stacked <- state_data %>%
  group_by(date, region)%>%
  summarize(deaths = sum(deaths, na.rm = T),
            daily_deaths = sum(daily_deaths, na.rm  = T))
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
                 scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                 #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                 coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
                 theme_bw() +
                 labs(x = NULL, y = NULL, title = 'Proportion of National Daily Deaths') +
                 theme(legend.position = 'none', axis.text.y = element_text(size = 9))+
                 theme(plot.title = element_text(size = 12)))

a2 <- ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_deaths, fill = region)) +
  geom_area(color = 'black') +
  #geom_line(aes(y = national_daily_deaths)) +
  scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  coord_cartesian(xlim = c(as.Date('2020-03-05'),today())) +
  theme_bw() +
  labs(x = 'Date', y = 'Daily Deaths',
       title = 'Daily COVID-19 Deaths',
       subtitle = paste0(curtotal, ' Total Deaths (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                         currate, ' new deaths/day\n', direction, chgrate, '% from 7 days ago\n'),
       caption = paste0('Data: The New York Times\nCurrent as of ', format(today()-1, '%b %d, %Y'))) +
  theme(legend.position = 'none', axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 14))+
  theme(plot.title = element_text(size = 24))

state_data_stacked <- state_data_stacked[complete.cases(state_data_stacked[ , 2]), ]

a4 <- ggplotGrob(ggplot(state_data_stacked, aes(x = date, y = weekly_avg_daily_deaths, fill = region)) +
                   geom_area(color = 'black') +
                   #geom_line(aes(y = national_daily_deaths)) +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   coord_cartesian(xlim = c(as.Date('2020-03-05'),max(state_data$date))) +
                   theme_bw() +
                   labs(x = NULL, y = NULL, title = 'Regional Curves') +
                   theme(legend.position = 'none',
                         #axis.text.y = element_blank(),
                         axis.text.x = element_text(size = 9),
                         axis.title = element_text(size = 14),
                         axis.ticks.y = element_blank(),
                         axis.line.y = element_blank())+
                   theme(plot.title = element_text(size = 12)) +
                   facet_grid(facets = region~.) +
                   theme(
                     strip.background = element_blank(),
                     #strip.text.y = element_blank()
                   ))

a3 <- ggplotGrob(plot_usmap(data = state_data_summary, value = 'region') +
                   scale_fill_brewer(palette = 'YlGnBu', name = 'Region', direction = -1) +
                   #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
                   theme(legend.position = 'none',
                         plot.title = element_text(hjust = 0.5),
                   ))

p11 <- a2 + annotation_custom(
  grob = a1,
  xmin = max(state_data$date)-40,
  xmax = max(state_data$date),
  ymin = 1500,
  ymax = 2250
) + annotation_custom(
  grob = a3,
  xmin = as.Date('2020-02-29'),
  xmax = as.Date('2020-04-05'),
  ymin = 1300,
  ymax = 2250
) + annotation_custom(
  grob = a4,
  xmin = as.Date('2020-03-03'),
  xmax = as.Date('2020-03-21'),
  ymin = 150,
  ymax = 1250
)

ggsave(filename = paste0(format(today(), '%m%d'), 'RegionalDeathsCurve', '.png'), plot = p11, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

#### TEST POSITIVE RATE CURVES STATES ####
state_order <- state_order_function(current_weekly_pos_rate, current_weekly_avg_daily_cases_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p12 <- state_data %>% 
  group_by(date, state)%>%
  filter(state_abs %in% .us) %>%
  summarize(weekly_new_tests = sum(weekly_new_tests),
            weekly_new_cases = sum(weekly_new_cases),
            current_two_week_chg_pos_rate_class = tail(current_two_week_chg_pos_rate_class,1))%>%
  mutate(weekly_pos_rate = (weekly_new_cases/weekly_new_tests)*100) %>%
  ggplot(., aes(x = date, y = weekly_pos_rate,
                color = current_two_week_chg_pos_rate_class,
  )) +
  geom_line(size = 2, alpha = 1) +
  #geom_line(aes(y = national_daily_cases)) +
  scale_color_manual(values = c('green', 'blue', 'red'), name = 'Two-Week\nChange in Weekly\nPositive Rate') +
  theme_bw() +
  labs(x = 'Date', y = 'Percent of Tests Positive', title = 'US State COVID-19 Test Positive Rate Curves',
       subtitle = 'Sorted by Current Weekly Test Positive Rate') +
  theme(legend.position = 'right', axis.text.y = element_text(size = 9),
        axis.text.x = element_text(size = 9),
        axis.title = element_text(size = 14))+
  theme(plot.title = element_text(size = 16)) +
  facet_wrap(facets = .~state, scales = 'free_y', ) +
  coord_cartesian(ylim = c(0,28), xlim = c(today()-22,today())) +
  geom_vline(xintercept = today()-15, linetype = 'dashed')

ggsave(filename = paste0(format(today(), '%m%d'), 'TestPositiveRateCurvesStates', '.png'), plot = p12, device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 18, height = 11.25, units = "in", dpi = 300)

### STATE PER CAPITA CASES CURVES ###
state_order <- state_order_function(desc(state), current_weekly_avg_daily_cases_10k)
state_data$state<-factor(state_data$state, levels=(state_order))
p13 <- state_data %>%
  #filter(state == state_name) %>%
  ggplot(., aes(date, daily_cases_10k)) +
  geom_col(fill = 'black', alpha = .1) +
  geom_area(aes(date, weekly_avg_daily_cases_10k, fill = current_two_week_chg_weekly_avg_daily_cases_class),
            size = 0.4, alpha = 1, color = 'black') +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  #scale_fill_brewer(palette = 'RdYlGn', direction = -1, name = 'Two-Week Change\nin Daily Cases', na.value = 'grey', drop = F) +
  scale_fill_manual(values = c('< -10%' = '#A6D96A', '-10 to +10%' = '#FFFFB2', '+10 to +25%' = '#FED976', '+25 to +50%' = '#FEB24C',
                               '+50 to +75%' = '#FD8D3C', '+75 to +100%' = '#F03B20', '> +100%' = '#BD0026'), name = 'Two-Week Change\nin Daily Cases') +
  #scale_fill_brewer(palette = 'YlOrRd', direction = 1, name = 'Two-Week Change\nin Daily Cases', na.value = 'grey', drop = F) +
  scale_x_date(labels = date_format("%b")) +
  labs(title = 'US State Per-Capita COVID-19 Curves',
       x = 'Date', y = 'Daily Cases per 10,000 People') +
  theme_bw() +
  facet_wrap(facets = .~state,
             #scales = 'free_y',
             ncol = 4,
             strip.position = 'bottom'
  ) +
  coord_cartesian(xlim = c(as.Date('2020-03-01'),today()), ylim = c(0,6)) +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(size = 6),
        legend.position = 'top') +
  guides(fill = guide_legend(reverse = F, nrow = 1, label.position = 'bottom', hjust = 0))

p12

ggsave(filename = paste0(format(today(), '%m%d'), 'VerticalStateCurves', '.png'), plot = p13,
       device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 9, height = 18, units = "in", dpi = 300)

### METRO AREA PER CAPITA CASES CURVES ###
cbsa_order <- cbsa_order_function(desc(cbsa_short), total_cases_10k)
#cbsa_order <- cbsa_order_function((cwa_daily_cases_10k), total_cases_10k)
cbsa_data$cbsa_short<-factor(cbsa_data$cbsa_short, levels=(cbsa_order))

p14 <- cbsa_data %>%
  filter(pop > 500000) %>%
  ggplot(., aes(date, daily_cases_10k)) +
  geom_col(fill = 'black', alpha = .1) +
  geom_area(aes(date, weekly_avg_daily_cases_10k,
                fill = current_two_week_chg_weekly_avg_daily_cases_class,
  ),
  size = 0.4, alpha = 1, color = 'black') +
  #scale_fill_manual(values = c('D' = 'blue', 'R' = 'red'), name = '2016 Election\nResult') +
  scale_fill_manual(values = c('< -10%' = '#A6D96A', '-10 to +10%' = '#FFFFB2', '+10 to +25%' = '#FED976', '+25 to +50%' = '#FEB24C',
                               '+50 to +75%' = '#FD8D3C', '+75 to +100%' = '#F03B20', '> +100%' = '#BD0026'), name = 'Two-Week Change\nin Daily Cases') +  geom_line(aes(date, weekly_avg_daily_cases_10k), size = 0.4, color = 'black', alpha = 1)+
  
  scale_x_date(labels = date_format("%b")) +
  labs(title = 'US Metro Area Per-Capita COVID-19 Curves',
       x = 'Date', y = 'Daily Cases per 10,000 People') +
  theme_bw() +
  facet_wrap(facets = .~cbsa_short,
             #scales = 'free_y',
             ncol = 6,
             strip.position = 'bottom'
  ) +
  coord_cartesian(xlim = c(as.Date('2020-03-01'),today()),
                  ylim = c(0,8.5)
  ) +
  theme(axis.title = element_text(size = 14),
        plot.title = element_text(size = 20, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 14),
        axis.text.x = element_text(size = 6),
        legend.position = 'top') +
  guides(fill = guide_legend(reverse = F, nrow = 1, label.position = 'bottom'))

p13

ggsave(filename = paste0(format(today(), '%m%d'), 'VerticalCBSACurves', '.png'), plot = p14,
       device = "png", path = 'C:/Users/benpg/Desktop/CovidDataDaily',
       width = 9, height = 21, units = "in", dpi = 450)
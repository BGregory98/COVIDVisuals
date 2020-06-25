################ START HERE ###########################

# All four together
multi_map(inc_vector = c(.us),
          no_cities = 0,
          state_border_color = 'black',
          county_border_color = 'transparent',
          state_labels = F)

#Look at multiplot each state
state_chart('VA', 49)

#NYT Live Data: New Cases and Deaths TODAY by State
new_today('MI')

state_cases_chart('VA', 49)

state_cases_chart_3(inc_vector = c(.us),
                    length = 100,
                    growth_change = 14,
                    title = 'Infection Curve')

state_deaths_chart_3(inc_vector = c(.us),
                     length = 84,
                     growth_change = 7,
                     title = 'Death Curve')


state_deaths_chart('US', 16)
state_growth_chart('VA', 14)
us_map(.us)
growth_chart('MN')
testing_chart('AZ', 50)
positive_rate('VA', 35)
mobility(.northeast_region)
active_cases('NY')

?plot_usmap

cbsa_map <- function(cbsa){

map1 <- plot_usmap(data = county_data_summary, regions = 'county', size = 0.4, values = 'total_cases_10k_class',
           include = county_data_summary$fips[county_data_summary$cbsa == cbsa],)

fipsinfo <- fips_info(c(na.omit(county_data_summary$fips[county_data_summary$cbsa == cbsa],)))
fipsinfo$abbr <- as.factor(fipsinfo$abbr)

state_borders <- plot_usmap(regions = "states",
                            include = levels(fipsinfo$abbr),
                            fill = 'transparent',
                            size = 1,
                            color = 'white',
                            labels = F,
                            label_color = 'white')

ggplot()+
  map1$layers[[1]] +
  state_borders$layers +
  map1$theme +
  coord_equal() +
  scale_fill_viridis(option = 'D', name = 'Total Cases per 10k', discrete = T, drop = T)
}

str(state_movement_data)

state_movement_data <- full_join(state_movement_data, states_regions, by = 'state')

ggplot()+
  geom_line(data = state_movement_data[state_movement_data$state != 'District of Columbia',],
            aes(x = date, y = weekly_avg_retail_recreation,
                group = state, color = region),
            size = 1) +
  geom_text(data = state_movement_data[state_movement_data$date == tail(state_movement_data$date,1),],
            aes(x = date, y = weekly_avg_retail_recreation), label = state) +
  theme(legend.position = 'none') +
  theme_minimal() +
  geom_hline(yintercept = 0)

cbsa_map('Tulsa, OK')

#Curves for individual metro areas
metro_cases_chart('Tulsa, OK')

# 1. New York-Newark-Jersey City, NY-NJ-PA
# 2. Los Angeles-Long Beach-Anaheim, CA
# 3. Chicago-Naperville-Elgin, IL-IN-WI
# 4. Dallas-Fort Worth-Arlington, TX
# 5. Houston-The Woodlands-Sugar Land, TX
# 6. Washington-Arlington-Alexandria, DC-VA-MD-WV
# 7. Miami-Fort Lauderdale-Pompano Beach, FL
# 8. Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
# 9. Atlanta-Sandy Springs-Alpharetta, GA
# 10. Phoenix-Mesa-Chandler, AZ
# 11. Boston-Cambridge-Newton, MA-NH
# 12. San Francisco-Oakland-Berkeley, CA
# 13. Riverside-San Bernardino-Ontario, CA
# 14. Detroit-Warren-Dearborn, MI
# 15. Seattle-Tacoma-Bellevue, WA
# 16. Minneapolis-St. Paul-Bloomington, MN-WI

#Curves for individual counties
county_cases_chart(state = 'Virginia',
                   county = 'Rockbridge')



# Total cumulative cases by county on an exponential scale
covid_map(inc_vector = c(.us),
          value = 'total_cases_exp_class',
          no_cities = 0,
          length = 65,
          title = 'Total Covid-19 Cases by County',
          legend.title = 'Total Cases',
          curve_title = 'Infection Curve',
          palette_type = 'viridis',
          color_palette = 'D',
          palette_direction = 1,
          state_border_color = 'white',
          state_border_size = .8,
          full_labels = T,
          label.color = 'white',
          county_border_color = 'black',
          include_curve = F,
          na.translate = T,
          na.value = 'black',
          seed = 800,
          state_label_type = state_abs,
          state_label_size = 4,
          alpha = 1)

# Cases per capita
covid_map(inc_vector = c(.us),
          value = 'total_cases_10k_class',
          no_cities = 0,
          length = 65,
          title = 'US Total Covid-19 Cases per Capita',
          legend.title = 'Total Cases\nper 10,000',
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

# Current growth rate per million people
covid_map(inc_vector = c(.us),
          value = 'cwa_daily_cases_mil_class',
          no_cities = 0,
          length = 65,
          title = 'Where are COVID-19 Cases Growing Fastest?',
          legend.title = 'Weekly Average\nof Daily Cases\nper Million',
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

# Change in Weekly Avg of Daily Cases over the last four weeks
covid_map(inc_vector = c(.us),
          value = 'current_two_week_chg_daily_cases_mil_class',
          no_cities = 0,
          length = 65,
          title = 'Total Covid-19 Cases per Capita by County',
          legend.title = 'Two-Week Change\nin Weekly Avg of\nDaily Cases\nper Million',
          curve_title = 'Infection Curve',
          palette_type = 'brewer',
          color_palette = 'RdYlBu',
          palette_direction = -1,
          state_border_color = 'black',
          state_border_size = .8,
          full_labels = F,
          label.color = 'white',
          county_border_color = 'lightgrey',
          include_curve = F,
          na.translate = T,
          na.value = 'white',
          seed = 800,
          state_label_type = state_abs,
          state_label_size = 4)

# 2018 Electoral Map
covid_map(inc_vector = c(.us),
          value = 'political_index_class',
          no_cities = 0,
          length = 65,
          title = '2018 General Election Electoral Map by County',
          legend.title = '2018 Election Results',
          curve_title = 'Infection Curve',
          palette_type = 'brewer',
          color_palette = 'RdBu',
          palette_direction = -1,
          state_border_color = 'black',
          state_border_size = .8,
          full_labels = T,
          label.color = 'white',
          county_border_color = 'black',
          include_curve = F,
          na.translate = T,
          na.value = 'grey',
          seed = 800,
          state_label_type = state_abs,
          state_label_size = 4)

# Deaths per capita
covid_map(inc_vector = c(.us),
          value = 'total_deaths_10k_class',
          no_cities = 0,
          length = 65,
          title = 'Total Deaths per Capita',
          legend.title = 'Deaths per\n10,000',
          palette_type = 'brewer',
          color_palette = 'RdPu',
          palette_direction = 1,
          state_border_color = 'black',
          county_border_color = 'lightgrey',
          state_border_size = .8,
          label.color = 'white',
          include_curve = F,
          full_labels = T,
          state_label_type = state_abs,
          state_label_size = 4)

# Movement
covid_map(data = county_movement_data_summary,
          inc_vector = c(.us),
          value = 'current_weekly_avg_retail_recreation_class',
          no_cities = 0,
          length = 65,
          title = 'Change in Mobility to Retail/Recreation Locations from Baseline',
          legend.title = 'Percent Change\nfrom Baseline\n(Median Day from\nJan 3 to Feb 6, 2020)',
          curve_title = 'Infection Curve',
          subtitle = paste0('As of 2020-05-25'),
          color_palette = 'RdYlGn',
          label.color = 'white',
          palette_direction = 1,
          state_border_color = 'black',
          state_border_size = .8,
          county_border_color = 'lightgrey',
          include_curve = F,
          full_labels = T,
          state_label_type = state_abs,
          state_label_size = 4,
          seed = 800)

# Increase in Movement from Minimum
covid_map(data = county_data_summary,
          inc_vector = c(.us),
          value = 'chg_min_retail_recreation_class',
          no_cities = 0,
          length = 65,
          title = 'Increase in Mobility from Minimum',
          legend.title = 'Percent Increase\nfrom Minimum',
          curve_title = 'Infection Curve',
          subtitle = paste0('As of '),
          color_palette = 'RdYlGn',
          palette_direction = 1,
          state_border_color = 'black',
          county_border_color = 'transparent',
          include_curve = F,
          full_labels = T)

# Percent Black
covid_map(data = county_data_summary,
          inc_vector = c(.us),
          value = 'PCT_NONWHITE',
          no_cities = 0,
          length = 65,
          title = 'Percent Nonwhite Population',
          legend.title = 'Percent Nonwhite',
          curve_title = 'Infection Curve',
          palette_type = 'viridis',
          color_palette = 'A',
          palette_direction = 1,
          state_border_color = 'white',
          state_border_size = 1,
          county_border_color = 'black',
          na.translate = T,
          na.value = 'black',
          label.color = 'white',
          state_label_type = state_abs,
          include_curve = F,
          city_point_color = 'white',
          full_labels = T,
          discrete.scale = F)

# When cases peaked
covid_map(data = county_data_summary[county_data_summary$max_weekly_avg_daily_cases > 0,],
          inc_vector = c(.us),
          value = 'how_long_ago_class2',
          no_cities = 0,
          length = 65,
          title = 'When Did Case Covid-19 Case Growth Rate Peak?',
          subtitle = paste0('Time Since the Weekly Average of New Cases per Day Peaked\n(As of ',today(),')'),
          legend.title = 'When Case\nGrowth Peaked',
          legend.position = 'right',
          palette_type = 'brewer',
          color_palette = 'YlOrRd',
          na.translate = T,
          palette_direction = -1,
          discrete.scale = T,
          state_border_color = 'black',
          state_border_size = 1,
          county_border_color = 'lightgrey',
          na.value = 'grey',
          state_label_type = state_abs,
          include_curve = F,
          full_labels = T,
          label.color = 'white',
          city_point_color = 'white')

ggsave(filename = 'RegionalCurves.png', device = 'png', path = 'C:/Users/benpg/Desktop')

display.brewer.all()

brewer.pal(n = 9, name = 'RdYlBu')

#Total new cases in the last X days by county on an exponential scale
new_cases_map(.us, 7)

# Regional Curves
regional_curves(title = NULL,
                no_cities = 0,
                county_border_color = 'transparent',
                value = 'total_cases_10k_class',
                direction = 'Vertical',
                legend.title = 'Total Cases\nper 10,000 People',
                color_palette = 'YlOrRd',
                palette_direction = 1,
                yaxis = TRUE,
                ylim = 20000)

#Where are cases currently growing fastest (state)?
plot_usmap(data = state_data_summary[state_data_summary$total_cases > 50,],
           value = 'current_two_week_chg_weekly_avg_daily_cases_class',
           regions = 'states',
           include = .us,
           color = 'black',
           size = 1,
           labels = T,
           label_color = 'white') + 
  labs(title = 'Two-Week Change in Weekly Average Daily Cases')+
  theme(legend.position = 'right') +
  theme(plot.title = element_text(face = 'bold', size = 15)) +
  scale_fill_brewer(palette="RdYlGn", direction = -1, na.value = 'white')

# Metro area curves
multi_curve(
  # Looking at states or metro areas?
  data = 'State',
  # What variable do we want to sort by?
  var = current_two_week_chg_weekly_avg_daily_cases,
  # How many days back do we want to start the charts?
  length = 84,
  # Limiting to states/metro areas with higher than X population
  pop_limit = 0,
  # Limiting to states/metro areas with higher than X total cases
  case_limit = 0,
  # Limiting to a particular state or region (note that cbsas are only ID'd by their primary state, i.e. New York Metro Area will only show up when NY is called)
  state = c(.us),
  # Are we showing the top 24 (1) or the bottom 24 (-1)
  direction = 1,
  # Standardize the Y axis or no?
  ylim = 6,
  # Variable for grey bars
  variable = daily_cases_10k,
  # Variable for red curve
  variable2 = weekly_avg_daily_cases_10k,
  # How many columns
  cols = 9)

51%%9

max_state_data <- max_state_data %>%
  arrange(desc(peak_hosp10k_date))

state_order <- as.vector((max_state_data$state_full_name))

# Heat map of case growth rate by state
state_data$state_full_name<-factor(state_data$state_full_name, levels=(state_order))
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state_full_name)) +
    geom_tile(aes(fill=hospitalizedCurrently_10k), color = 'black') +
    scale_fill_distiller(palette = "Spectral",
                         name = 'Proportion of Peak\nGrowth Rate',
                         na.value = 'lightgrey') +
    theme_minimal() +
    theme(legend.position = 'right') +
    labs(x = 'Date', y = 'State',
         title = 'COVID-19 Weekly Average of Daily New Cases by US State',
         subtitle = 'Ordered by Peak Date, Growth Rates Normalized Across States') +
    theme(axis.text.y = element_text(size = 10, vjust = 0.25))+
  geom_text(data = . %>% filter(date == (today()-2)), aes(label=region),
            size = 3, nudge_x = -112, vjust = 0.25, alpha = .75)+
  geom_text(data = . %>% filter(date == (today()-1)), aes(label=format(peak_hosp10k_date, "%b %d")),
            size = 3, nudge_x = 4, vjust = 0.25)+
  #geom_label(data = . %>% filter(date == (peak_date-4)),
  #          aes(label=format(peak_date, "%b %d")),
  #          size = 2.5, nudge_x = 0, hjust = 0.5, color = 'white', fill = 'black',
  #          label.r = unit(0, 'lines'), alpha = .3,
  #          label.padding = unit(0.2, 'lines'))+
  geom_point(data = . %>% filter(date == (peak_hosp10k_date)), color = 'white',
             alpha = 0.7, size = 1.2)

# Ridge curves
state_data %>%
  filter(date > '2020-03-01')%>%
  ggplot(., aes(date, state_full_name)) +
  geom_ridgeline(aes(height=pct_max_weekly_avg_daily_cases, fill = region),
                 color = 'black', scale = 2, alpha = .6, size = 1)+
  scale_fill_brewer(palette = 'YlGnBu', direction = -1, name = 'Region', na.value = 'grey') +
  theme_bw() +
  theme(legend.position = 'right') +
  geom_point(data = . %>% filter(date == (peak_date)), color = 'black', fill = 'white', stroke = 1.5,
             alpha = 1, size = 2.5, shape = 21, position = position_nudge(y = 2)) +
  labs(title = 'United States COVID-19 Growth Curves', y = 'State', x = 'Date',
       subtitle = 'States sorted by peak date\nGrowth rates normalized across states') +
  geom_text(data = . %>% filter(date == (today()-1)), aes(label=format(peak_date, "%b %d")),
            size = 3, nudge_x = 7, vjust = -0.25) +
  theme(axis.text.y = element_text(vjust = -0.25)) +
  theme(plot.margin=unit(c(1,1,1,1),"cm")) +
  expand_limits(y = 30)
  #scale_fill_manual(values = cols) +
  

cols <- c("D" = "blue", "R" = "red")

?theme_minimal()

county_data_summary$county[is.na(county_data_summary$rucc_2013)]

county_data$fips<-factor(county_data$fips, levels=(county_order))

class(county_data_summary$rucc_2013)

heat_wave_county <- function(inc_vector){

temp <- county_data %>%
          filter(state_abs %in% c(inc_vector)) %>%
          group_by(fips, county, state_abs) %>%
          summarize(label = paste0(tail(county,1), ', ', tail(state_abs,1)),
                    peak_date = tail(peak_date,1)) %>%
          arrange(desc(peak_date))

county_data %>%
  filter(date > '2020-03-01')%>%
  filter(state_abs %in% c(inc_vector))%>%
  #arrange(desc(peak_date))%>%
  ggplot(., aes(date, fips)) +
  geom_raster(aes(fill=pct_max_weekly_avg_daily_cases)) +
  scale_fill_distiller(palette = "Spectral", name = 'Proportion of Peak\nGrowth Rate') +
  theme_cowplot() +
  #scale_y_discrete(labels = county_name_from_fips())+
  theme(legend.position = 'none') +
  labs(x = 'Date', y = 'County', title = 'COVID-19 Weekly Average of Daily Cases by County',
       subtitle = 'Ordered by Peak Date, Growth Rates Normalized Across Counties') +
  theme(axis.text.y = element_text(size = 6)) +
  scale_y_discrete(labels=as.vector(temp$label))

}

heat_wave_county(c('MS'))


# CBSA Heat Wave
cbsa_data$cbsa<-factor(cbsa_data$cbsa, levels=(cbsa_order))
cbsa_data %>%
  filter(date > '2020-03-01')%>%
  filter(state_abs %in% c(.us))%>%
  filter(pop > 750000) %>%
  ggplot(., aes(date, cbsa)) +
  #facet_grid(facets = region~.) +
  geom_tile(aes(fill=pct_max_weekly_avg_daily_cases), color = 'transparent', size = 0.4) +
  scale_fill_distiller(palette = "Spectral", direction = -1, na.value = 'black', name = 'Proportion of Peak\nGrowth Rate') +
  #scale_fill_viridis(option = "A", direction = 1, na.value = 'black', name = 'Proportion of Peak\nGrowth Rate') +
  theme_minimal() +
  #scale_y_discrete(labels = county_name_from_fips())+
  theme(legend.position = 'right') +
  labs(x = 'Date', y = 'Metro Area', title = 'United States COVID-19 Growth Rate by Metro Area',
       subtitle = 'Metro Areas Ordered by Peak Date, Growth Rates Normalized Across Metro Areas\n(Metro Areas with Populations > 750,000 Included)') +
  theme(axis.text.y = element_text(size = 6, vjust = 0.25)) +
  #theme(axis.text.y = element_text(color = regions)) +
  #geom_text(data = . %>% filter(date == (today()-2)), aes(label=region),
  #          size = 2, nudge_x = -107, vjust = 0.25, alpha = .75)+
  geom_text(data = . %>% filter(date == (today()-1)), aes(label=round(pct_max_weekly_avg_daily_cases,2)),
            size = 2, nudge_x = 3, vjust = 0.25)+
  geom_text(data = . %>% filter(date == (peak_date-4)),
            aes(label=format(peak_date, "%b %d")),
            size = 1.8, nudge_x = 0, hjust = 0.5, vjust = 0.25, color = 'black')+
  geom_point(data = . %>% filter(date == (peak_date)), color = 'white',
             alpha = 0.7, size = 1.2)

county_name_from_fips <- function(fips){
  
  finfo <- fips_info(fips)
  county_name <- finfo$county
  state_ab <- finfo$abbr
  county_state <- paste0(county_name, ', ', state_ab)
  return(county_state)
}
  
county_name_from_fips(21021)

fips('VI')

?theme_cowplot()

ab_to_full('CA')

## Variables to sort by (METRO)
#pop - total_cases - total_deaths - total_cases_10k - total_deaths_10k - case_mort_rate - cwa_growth_rate - cwa_daily_cases
#cwa_daily_cases_10k - current_two_week_chg_weekly_avg_daily_cases - current_one_week_chg_weekly_avg_daily_cases - current_one_week_chg_weekly_avg_daily_cases_10k

## Variables to sort by (STATE)
#pop - total_cases - current_active_cases - current_active_cases_10k - max_active_cases - total_deaths - total_tests - total_hospitalizations - total_recoveries
#cwa_growth_rate - cwa_daily_cases - cwa_daily_cases_10k - cwa_daily_tests - cwa_daily_tests_10k - current_weekly_positive_rate
#case_mortality_rate_lag - current_three_week_growth_rate - current_two_week_chg_pos_rate - current_two_week_chg_weekly_avg_daily_cases - current_one_week_chg_weekly_avg_daily_cases_10k

# Sort states or metro areas by any of the above variables
view_sort(data = 'State',
          variable = current_weekly_positive_rate,
          rows = 10,
          direction = 1)


#WCWADCP Animation
wcwadcp_animation_state('US')

#Curve Animation
curve_animation_state('NR')

# Tests per capita by state
plot_usmap(data = state_data_summary, values = 'total_tests_10k', regions = 'states')+
  scale_fill_viridis(option = 'C', name = NULL) +
  theme(legend.position = 'right') +
  labs(title = 'Total Tests per 10k pop')




# Maps for mobility data
mobility_map(data = 'State',
             state = .us,
             value = 'current_weekly_avg_groc_pharm')

## Variables for mobility data
#current_weekly_avg_retail_recreation - current_weekly_avg_groc_pharm - current_weekly_avg_parks - current_weekly_avg_transit
#current_weekly_avg_workplaces - current_weekly_avg_residential

#current_two_week_chg_retail_recreation - current_two_week_chg_groc_pharm - current_two_week_chg_parks - current_two_week_chg_transit
#current_two_week_chg_workplaces - current_two_week_chg_residential

# Regional Curves
regional_curves(title = NULL,
                no_cities = 0,
                county_border_color = 'transparent',
                direction = 'Vertical',
                value = 'total_cases_10k_class',
                legend.title = 'Total Cases\nper 10,000',
                color_palette = 'YlOrRd',
                palette_direction = 1,
                yaxis = TRUE,
                ylim = 20000,
                length = 91)

# Subregional curves
subregional_curves(title = NULL,
                   no_cities = 0,
                   county_border_color = 'transparent',
                   direction = 'Horizontal',
                   value = 'total_cases_mil_class',
                   legend.title = 'Total Cases',
                   color_palette = 'YlOrRd',
                   palette_direction = 1,
                   axis_text_size = 4,
                   yaxis = TRUE,
                   ylim = 18000)

# Subregional testing curves
subregional_testing_curves(title = NULL,
                   no_cities = 0,
                   county_border_color = 'transparent',
                   direction = 'Horizontal',
                   value = 'total_cases_10k_class',
                   legend.title = 'Total Positives\nper 10,000',
                   color_palette = 'YlGnBu',
                   palette_direction = 1,
                   axis_text_size = 9,
                   yaxis = TRUE,
                   ylim = 45)

state_tests_chart_3(inc_vector = c('DC'),
                    length = 70,
                    yaxis = F,
                    ylim = 40,
                    title = 'United States COVID-19 Test-Positive Trends',
                    axis_text_size = 20)

state_labs <- geom_shadowtext(data = state_centers[state_centers$state != 'District of Columbia',],
                              aes(x = lon.1, y = lat.1, label = state_abs),
                              size = 3.5, alpha = 1,
                              fontface = 'bold',
                              color = 'white',
                              label.r = unit(0, 'lines'), label.size = 0.5,
                              segment.size = 1,
                              point.padding = NA, force = .5, seed = 1000,
                              check_overlap = T)

plot_usmap(data = state_data_summary,
           values = 'total_tests_10k_class',
           size = 1,
           color = 'black') +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 15), legend.position = 'right') +
  theme(plot.title = element_text(size = 24, face = 'bold', hjust = 0.5),
        plot.subtitle = element_text(size = 18, hjust = 0.5),
        plot.caption = element_text(size = 12, hjust = 1.17))+
  labs(title = 'Total COVID-19 Tests per Capita in the United States', subtitle = 'Data: The Covid Tracking Project and\nThe New York Times') +
  scale_fill_brewer(palette = 'RdYlGn', direction = 1, name = 'Total Tests\nper 10,000') +
  state_labs

state_data %>%
  filter(state %in% (.us))%>%
  ggplot(., aes(x = date, y = weekly_test_positive_rate, group = state, color = state)) +
    geom_line(alpha = 1) +
    coord_cartesian(xlim = c(as.Date('2020-05-01'),today()), ylim = c(0,0.25))

#This is the US curve if we exclude the New York metro area
p1 <- county_data %>%
  filter(state_abs %in% .south_region & cbsa != 'Washington-Arlington-Alexandria, DC-VA-MD-WV') %>%
  group_by(date) %>%
  summarize(daily_cases = sum(daily_cases)) %>%
  mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  ggplot(., aes(date, daily_cases, group = 1)) +
  geom_col(fill = 'grey12', alpha = .1) +
  geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, fill = 'red') +
  geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
  scale_x_date(labels = date_format("%m/%d"))+
  labs(title = 'South Region Excluding DC Metro Area', x = 'Date', y = 'Daily Cases') +
  coord_cartesian(ylim = c(0,12000), xlim = c(as.Date('2020-04-01'),today()))+
  theme_cowplot() +
  theme(legend.position = 'none')

p2 <- county_data %>%
  filter(state_abs %in% .south_region) %>%
  group_by(date) %>%
  summarize(daily_cases = sum(daily_cases)) %>%
  mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  ggplot(., aes(date, daily_cases, group = 1)) +
  geom_col(fill = 'grey12', alpha = .1) +
  geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, fill = 'red') +
  geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
  scale_x_date(labels = date_format("%m/%d"))+
  labs(title = 'South Region Including DC Metro Area', x = 'Date', y = 'Daily Cases') +
  coord_cartesian(ylim = c(0,12000), xlim = c(as.Date('2020-04-01'),today()))+
  theme_cowplot() +
  theme(legend.position = 'none')

multiplot(p1, p2, cols = 2)

plot_usmap(data = county_data_summary, value = 'cbsa', include = .us, regions = 'counties') +
  theme(legend.position = 'none')
plot_usmap(data = county_data_summary, value = 'metro_nonmetro', include = .us, regions = 'counties')
plot_usmap(data = county_data, value = 'rucc_2013', include = .us, regions = 'counties') +
  scale_fill_brewer(palette = 'Reds', direction = -1)


#Looking at cases per day in metro versus nonmetro counties
county_data_metrononmetro <- county_data %>%
  group_by(date, metro_nonmetro)%>%
  summarize(daily_cases = sum(daily_cases),
            daily_cases_10k = sum(daily_cases_10k))

county_data_metrononmetro <- county_data_metrononmetro %>%
  group_by(metro_nonmetro) %>%
  mutate(weekly_avg_daily_cases_10k = frollmean(daily_cases_10k, 7))

county_data_metrononmetro <- county_data_metrononmetro[complete.cases(county_data_metrononmetro[ , 2]), ]

ggplot(data = county_data_metrononmetro, aes(x = date, y = weekly_avg_daily_cases_10k, group = metro_nonmetro, color = metro_nonmetro)) +
  geom_line(size = 2)+
  scale_x_date(labels = date_format("%m/%d"))+
  labs(title = 'Curves in Metro areas versus Nonmetro Areas', x = 'Date', y = 'Daily Cases per 10k People') +
  theme_cowplot() +
  theme(legend.position = 'right')

#Looking at cases per day in metro versus nonmetro counties
county_data_rucc_2013 <- county_data %>%
  group_by(date, rucc_2013)%>%
  summarize(daily_cases = sum(daily_cases),
            daily_cases_10k = sum(daily_cases_10k))

county_data_rucc_2013 <- county_data_rucc_2013 %>%
  group_by(rucc_2013) %>%
  mutate(weekly_avg_daily_cases_10k = frollmean(daily_cases_10k, 7)) %>%
  mutate(one_week_chg_wadc10k = (weekly_avg_daily_cases_10k/lag(weekly_avg_daily_cases_10k,7)-1))

county_data_rucc_2013 <- county_data_rucc_2013[complete.cases(county_data_rucc_2013[ , 2]), ]

county_data_rucc_2013$rucc_2013 <- as.factor(county_data_rucc_2013$rucc_2013)

ggplot(data = county_data_rucc_2013[county_data_rucc_2013$date > '2020-03-01',],
       aes(x = date, y = weekly_avg_daily_cases_10k, group = rucc_2013, fill = rucc_2013)) +
  geom_area(size = 2)+
  scale_x_date(labels = date_format("%m/%d"))+
  labs(title = 'Curves in Rural-Urban Continuum', x = 'Date', y = 'Daily Cases per 10k People') +
  theme_cowplot()
#scale_color_brewer(palette = "Reds", direction = -1)


?theme_cow


str(state_movement_data_summary)

display.brewer.all()

dates_dummy <- county_data %>%
  group_by(date) %>%
  summarize(length(state))
dates_for_animation <- dates_dummy$date

anim_length <- c(1:length(dates_for_animation))

for (i in anim_length){
  
  plot_usmap(data = county_data[county_data$date == dates_for_animation[i],],
             value = 'two_week_chg_weekly_avg_daily_cases_class',
             regions = 'counties',
             include = .us,
             color = 'transparent') + 
    labs(title = 'Two Week Change in Weekly Average Daily Cases',
         subtitle = (dates_for_animation[i]))+
    theme(legend.position = 'right') +
    scale_fill_brewer(palette="RdYlGn", direction = -1, na.value = 'grey')
  
  ggsave(filename = paste('frame_', dates_for_animation[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
}



animate(state1 = .us,
        value = 'weekly_avg_daily_cases',
        title = 'Northeast Weekly Average Daily Cases',
        scale_title = '',
        anim_length = 55,
        color = 'B',
        scale_limits = 'Yes',
        navalue = 'black')

qplot(data = county_data, x = weekly_avg_daily_cases)

##### SAVED ANIMATIONS #####
#(1) Northeast Daily Change in Weekly Average Daily Cases per 10k Pop
animate(state1 = .northeast_region,
        value = 'daily_chg_wadcp',
        title = 'Northeast Daily Change in Weekly Average Daily Cases per 10k Pop',
        scale_title = 'Daily Change in Weekly Average Daily Cases per 10k Pop',
        anim_length = 'All',
        color = 'B',
        scale_limits = 'Yes',)

#(2) Northeast Weekly Change in Weekly Average Daily Cases per 10k Pop
animate(state1 = .northeast_region,
        value = 'weekly_chg_wadcp',
        title = 'Northeast Weekly Change in Weekly Average Daily Cases per 10k Pop',
        scale_title = '',
        anim_length = 45,
        color = 'B',
        scale_limits = 'Yes',
        navalue = '#BC3852')

#(3) 'US Weekly Change in Weekly Average Daily Cases per 10k Pop'
animate(state1 = .us,
        value = 'weekly_chg_wadcp',
        title = 'US Weekly Change in Weekly Average Daily Cases per 10k Pop',
        scale_title = '',
        anim_length = 45,
        color = 'B',
        scale_limits = 'Yes',
        navalue = '#110A30')

qplot(data = county_data, x = weekly_chg_wadcp)

#(4) US Weekly Average Daily Cases per 10k Pop
animate(state1 = .us,
        value = 'weekly_avg_daily_cases_10k_for_animation',
        title = 'US Weekly Average Daily Cases per 10k Pop',
        scale_title = paste0('Weekly Average', '\n', 'of Daily Cases', '\n', 'per 10,000 People'),
        anim_length = 60,
        color = 'c',
        scale_limits = 'Yes',
        min = 0,
        max = 15,
        navalue = '#440154',
        borders = 'No')

#(5) Northeast Weekly Average Daily Cases per 10k Pop
animate(state1 = .northeast_region,
        value = 'weekly_avg_daily_cases_10k_for_animation',
        title = 'Northeast US Daily COVID-19 Cases per Capita',
        scale_title = paste0('Weekly Average', '\n', 'of Daily Cases', '\n', 'per 10,000 People'),
        anim_length = 60,
        color = 'A',
        scale_limits = 'Yes',
        min = 0,
        max = 15,
        navalue = '#0E0887',
        borders = 'No')

#(6) Midwest Weekly Average Daily Cases per 10k Pop
animate(state1 = 'IL',
        state2 = 'WI',
        state3 = 'MN',
        state4 = 'IN',
        state5 = 'MI',
        state6 = 'IA',
        state7 = 'MO',
        value = 'weekly_avg_daily_cases',
        title = 'Midwest US Daily COVID-19 Cases per Capita',
        scale_title = paste0('Weekly Average', '\n', 'of Daily Cases', '\n', 'per 10,000 People'),
        anim_length = 60,
        color = 'A',
        scale_limits = 'Yes',
        min = 0,
        max = 1500,
        navalue = 'black',
        borders = 'No')

curve_animation_state_min('NR')


qplot(data = county_data, x = weekly_avg_daily_cases)

#Run animation loop
for (i in anim_length){
  
  plot_usmap(data = county_movement_data[county_movement_data$date == dates_animation[i],],
             value = 'weekly_avg_groc_pharm',
             regions = 'counties',
             include = .us,
             color = 'transparent') + 
    labs(title = 'Percent Change in Mobility to Retail and Recreation from Baseline',
         subtitle = (dates_animation[i]))+
    theme(legend.position = 'right')+
    theme(plot.title = element_text(face = 'bold', size = 18)) +
    theme(plot.subtitle = element_text(size = 15)) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12)) +
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', midpoint = 1, breaks = c(-100, 0, 100),
                         na.value = '#C1C1C1', limits = c(-100,100), name = NULL) +
    theme(panel.background = element_rect(color = '#2C2828', fill = '#2C2828'))
  
  
  ggsave(filename = paste('frame_', dates_animation[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
}

state_data %>%
  filter(state == 'NR' | state == 'SR'| state == 'WR' | state == 'MR') %>%
  ggplot(., aes(x = date, y = daily_cases, group = state, fill = state)) +
  geom_col(aes(x = date, y = daily_cases)) +
  theme_cowplot() +
  labs(x = 'Date', y = 'Daily Confirmed Cases')

ggplot(county_data[county_data$state == 'New York',],
       aes(x = date, y = daily_cases, group = county, fill = county, color = county)) +
  geom_col(alpha = .1) +
  theme_cowplot() +
  labs(x = 'Date', y = 'Daily Cases') +
  theme(legend.position = 'right')

#Map showing when cases peaked by county


plot_usmap(data = max_county_data,
           value = 'how_long_ago_class',
           regions = 'counties',
           include = .us,
           color = 'transparent') + 
  labs(title = 'When Did Case Growth Rate Peak?') +
  theme(legend.position = 'right') +
  scale_fill_brewer(palette="Reds", direction = -1, na.value = 'grey')
 scale_fill_viridis(option = 'D', direction = -1, discrete = T)
#Finally figuring out how to show circles on counties and then hopefully animate them
animdummy <- county_data %>%
  group_by(date) %>%
  summarize(length(fips))

dates_animation <- animdummy$date

anim_length <- c(1:(length(dates_animation)))



for (i in anim_length){
  
  county_data_pretransform <- county_data[county_data$date == dates_animation[i],] %>%
    select(lon, lat, state, county, fips, cbsa, rucc_2013, metro_nonmetro, cases, deaths,
           daily_cases, daily_deaths, weekly_avg_daily_cases, weekly_avg_one_week_chg_weekly_avg_daily_cases_animation)
  
  
  scale1 <- scale_size_continuous(range = c(1, 35),
                                 labels = scales::comma,
                                 name = 'Weekly Average of Daily Cases',
                                 limits = c(0, 1700))
  
  scale2 <- scale_color_gradient2(low = 'green', mid = 'yellow', high = 'red', midpoint = 0, breaks = c(-.5, 0, .5),
                                 na.value = '#C1C1C1', limits = c(-.5,.5), name = NULL)
  
  county_data_transformed <- usmap_transform(county_data_pretransform)
  
  plot_usmap(regions = 'counties') +
    geom_point(data = county_data_transformed, aes(x = lon.1, y = lat.1, size = weekly_avg_daily_cases,
                                                   color = weekly_avg_one_week_chg_weekly_avg_daily_cases_animation),
               alpha = .6) +
    labs(title = 'Weekly Average of Daily Cases by County Over Time',
         subtitle = dates_animation[i]) +
    scale1 +
    scale2 +
    theme(plot.title = element_text(face = 'bold', size = 20)) +
    theme(plot.subtitle = element_text(size = 18)) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12)) +
    theme(legend.position = 'right')
  
  ggsave(filename = paste('frame_', dates_animation[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
  print(dates_animation[i])
}



metro_map <- function(cbsa, value, option){
  
  temp <- county_data_summary %>%
    filter(cbsa == cbsa)
  temp <- as.vector(temp$fips)
  
  plot_usmap(data = county_data_summary, region = 'counties', include = temp, values = value) +
    scale_fill_viridis(option = option)
  
}

state_data

levels(state_data$state)

#Function for mapping
us_map <- function(include = .us, value = 'total_cases_exp_class', title = 'Total Confirmed Cases',
                   no_cities = 10)
  
{
  labels <- city_labels(include, no_cities)
  points <- city_points(include, no_cities)
  if (include != .us){
    plot_usmap(data = county_data_summary, values = value, regions = 'counties',
               include = include,
               color = 'transparent')+
      scale_fill_brewer(palette="YlOrRd", na.value = 'grey', name = 'Total Cases') +
      theme(legend.position = 'right') +
      labs(title = title) +
      labels +
      points
  } else {
    plot_usmap(data = county_data_summary, values = value, regions = 'counties',
               include = include,
               color = 'transparent')+
      scale_fill_brewer(palette="YlOrRd", na.value = 'grey', name = 'Total Cases') +
      theme(legend.position = 'right') +
      labs(title = title) 
  }
}

us_map('VA')

'US' <- c('CT', 'MA', 'ME', 'NH', 'RI', 'VT', 'NJ', 'NY', 'PA', 'IL', 'IN', 'MI', 'OH', 'WI', 'IA', 'KS', 'MN', 'MO', 'NE', 'ND', 'SD', 'DC', 'DE', 'FL', 'GA', 'MD', 'NC', 'SC', 'VA', 'WV', 'AL', 'KY', 'MS', 'TN', 'AR', 'LA', 'OK', 'TX', 'AZ', 'CO', 'ID', 'MT', 'NV', 'NM', 'UT', 'WY', 'AK', 'CA', 'HI', 'OR', 'WA')

#qplot(data = county_data_summary, x = total_cases_10k)

citiesdata<-read.csv("C:/Users/benpg/Desktop/R_Covid/worldcities.csv")
states_regions <- read.csv('C:/Users/benpg/Desktop/R_Covid/states_regions.csv')
citiesdata <- citiesdata %>%
  filter(country == 'United States') %>%
  select(lon, lat, city, country, admin_name, population)

city_data <- usmap_transform(citiesdata)

names(city_data)[names(city_data) == 'admin_name'] <- 'state'
names(city_data)[names(city_data) == 'population'] <- 'pop'

city_data <- city_data %>%
  filter(city != 'Manhattan') %>%
  filter(city != 'Brooklyn') %>%
  filter(city != 'Bronx') %>%
  filter(city != 'Queens') %>%
  filter(city != 'Staten Island')

state_abs <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI',
               'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN',
               'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH',
               'OK', 'OR', 'PA', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA',
               'WV', 'WI', 'WY')
length(state_abs)

dumby <- city_data %>%
  group_by(state) %>%
  summarize(pop = sum(pop))
state <- dumby$state
states_abs <- data.frame(state, state_abs)

city_data <- full_join(city_data, states_abs, by = 'state')

names(states_regions)[names(states_regions) == 'ï..state_abs'] <- 'state_abs'

city_data <- full_join(city_data, states_regions, by = 'state_abs')

#Writing the function to make the daily cases chart for counties
county_cases_chart = function(state_name, county_name){
  
  temp <- county_data_summary %>%
    filter(state == state_name & county == county_name)
  
  curtotal <- temp$total_cases
  curtotalcapita <- round(temp$total_cases_10k,3)
  
  p1 <-  county_data %>%
    filter(state == state_name & county == county_name) %>%
    mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    ggplot(., aes(date, daily_cases, group = 1, fill = county, alpha = .6)) +
    geom_bar(stat='identity', position = 'stack') +
    geom_line(aes(date, weekly_avg_daily_cases), size = 2) +
    theme_cowplot() +
    labs(x = 'Date', y = 'Daily Cases', title = paste0(county_name, ', ', state_name),
         subtitle = paste0(curtotal, ' total confirmed cases (', curtotalcapita, ' per 10,000)'))+
    scale_x_date(labels = date_format("%m/%d"))+
    theme(legend.position = 'none')
  return(p1)
  
}

#Writing the function to make the daily cases chart
state_cases_chart = function(state_name, length = 'All'){
  
  new_today <- new_today(state_name)
  curnew <- new_today$daily_cases
  curnew <- format((curnew),big.mark=",",scientific=FALSE)
  
  temp <- state_data_live %>%
    filter(state == state_name)
  
  temp2 <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
    mutate(., per_10k_rank = rank(-total_cases_10k))
  
  temp2state <- temp2 %>%
    filter(state == state_name)
  
  
  
  currank <- temp2state$per_10k_rank
  curtotal <- tail(temp$cases, 1)
  curtotalcapita <- round(((curtotal / tail(temp$pop, 1))*10000),3)
  curtotal <- format((curtotal),big.mark=",",scientific=FALSE)
  
  if(state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    
    labels <- labs(x = 'Date', y = 'Daily Cases', title = state_name,
                   subtitle = paste0(curtotal, ' total confirmed cases (', curtotalcapita, ' per 10,000) ', currank, ' of 55.', '\n',
                                     curnew, ' cases added so far today'))
    
  } else {
    
    labels <- labs(x = 'Date', y = 'Daily Cases', title = state_name,
                   subtitle = paste0(curtotal, ' total confirmed cases (', curtotalcapita, ' per 10,000) ', '\n',
                                     curnew, ' cases added so far today'))
    
  }
  
  p1 <-  state_data %>%
    filter(state == state_name) %>%
    mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    ggplot(., aes(date, daily_cases, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, fill = 'red') +
    geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
    labels +
    scale_x_date(labels = date_format("%m/%d"))+
    theme_cowplot() +
    theme(legend.position = 'none')
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p1)
}


metro_cases_chart = function(cbsa_name){
  
  temp <- cbsa_data %>%
    filter(cbsa == cbsa_name)
  
  temp2 <- cbsa_data_summary %>%
    mutate(., per_10k_rank = rank(-total_cases_10k))
  
  temp2cbsa <- temp2 %>%
    filter(cbsa == cbsa_name)
  
  currank <- temp2cbsa$per_10k_rank
  curtotal <- tail(temp$cases, 1)
  curtotalcapita <- round(((curtotal / tail(temp$pop, 1))*10000),3)
  
  
  labels <- labs(x = 'Date', y = 'Daily Cases', title = cbsa_name,
                 subtitle = paste0(curtotal, ' total confirmed cases (', curtotalcapita, ' per 10,000) '))
  
  
  p1 <-  cbsa_data %>%
    filter(cbsa == cbsa_name) %>%
    mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    ggplot(., aes(date, daily_cases, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, fill = 'red') +
    geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
    labels +
    scale_x_date(labels = date_format("%m/%d"))+
    theme_cowplot() +
    theme(legend.position = 'none')
  return(p1)
}

state_deaths_chart = function(state_name, length = 'All'){
  
  temp <- state_data_live %>%
    filter(state == state_name)
  
  temp2 <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
    mutate(., per_10k_deaths_rank = rank(-total_deaths_10k))%>%
    mutate(., mort_rate_rank = rank(-case_mortality_rate_lag)) %>%
    mutate(., rec_rate_rank = rank(-case_recovery_rate))
  
  temp2state <- temp2 %>%
    filter(state == state_name)
  
  currank <- temp2state$per_10k_deaths_rank
  currankmort <- temp2state$mort_rate_rank
  currankrec <- temp2state$rec_rate_rank
  
  curtotal <- tail(temp$deaths, 1)
  curmortrate <- (round(tail(temp$case_mortality_rate_lag, 1),4))*100
  #currecrate <- round(tail(temp$case_recovery_rate, 1),2)
  
  curtotalcapita <- round(((curtotal / tail(temp$pop, 1))*10000),3)
  curtotal <- format((curtotal),big.mark=",",scientific=FALSE)
  
  if (state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    
    labels <- labs(x = 'Date', y = 'Daily Deaths',
                   subtitle = paste0(curtotal, ' total confirmed deaths (', curtotalcapita, ' per 10,000) (', currank, ') \n',
                                     curmortrate, '% mortality rate (', currankmort, ')'))
  } else {
    labels <- labs(x = 'Date', y = 'Daily Deaths',
                   subtitle = paste0(curtotal, ' total confirmed deaths (', curtotalcapita, ' per 10,000) \n',
                                     curmortrate, '% mortality rate'))
  }
  
  p8 <-  state_data %>%
    filter(state == state_name) %>%
    mutate(., weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
    ggplot(., aes(date, daily_deaths, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .5, fill = 'black') +
    geom_line(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .8, color = 'black')+
    theme_cowplot() +
    labels +
    scale_x_date(labels = date_format("%m/%d"))+
    theme(legend.position = 'none')
  
  if (length != 'All'){
    p8 <- p8 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p8)
}


#Writing the function to make the growth chart
state_growth_chart = function(state_name, length = 'All'){
  
  temp <- state_data %>%
    filter(state == state_name)
  
  curtotal <- round(tail(temp$weekly_avg_growth_rate, 1), 3)
  
  p2 <- state_data %>%
    filter(state == state_name) %>%
    ggplot(., aes(date, weekly_avg_growth_rate, group = 1, fill = state)) +
    geom_line(size = 1, color = 'red') +
    geom_hline(aes(yintercept = 1), size = 1, linetype = 'dashed') +
    theme_cowplot() +
    labs(x = 'Date', y = 'Growth Rate', subtitle = paste0('Current weekly average growth rate: ', curtotal))+
    scale_x_date(labels = date_format("%m/%d"))+
    theme(legend.position = 'none') +
    coord_cartesian(ylim = c(.5,2))
  
  if (length != 'All'){
    p2 <- p2 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p2)
}

growth_chart = function(state_name, length = 'All') {
  
  state_data_temp <- state_data %>%
    filter(cases > 10) %>%
    group_by(state) %>%
    mutate(days_since_10th_case = c(1:length(cases)))
  
  p4 <- ggplot(state_data_temp, aes(x = days_since_10th_case, y = cases, group = state)) +
    geom_line(alpha = 0.5)+
    geom_line(data = state_data_temp[state_data_temp$state == 'US', ],
              aes(x = days_since_10th_case, y = cases, group = state),
              size = 1.5, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == 'NR', ],
              aes(x = days_since_10th_case, y = cases, group = state),
              color = 'red', size = 1, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == 'SR', ],
              aes(x = days_since_10th_case, y = cases, group = state),
              color = 'blue', size = 1, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == 'MR', ],
              aes(x = days_since_10th_case, y = cases, group = state),
              color = 'yellow', size = 1, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == 'WR', ],
              aes(x = days_since_10th_case, y = cases, group = state),
              color = 'green', size = 1, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == state_name, ],
              aes(x = days_since_10th_case, y = cases, group = state),
              color = 'red', size = 2.5, alpha = 1)+
    scale_y_log10() +
    theme_cowplot() +
    labs(x = 'Days Since 10th Confirmed Case', y = 'Total Confirmed Cases')
  
  if (length != 'All'){
    p4 <- p4 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p4)
}

testing_chart = function(state_name, length = 'All') {
  
  state_data_temp <- state_data %>%
    filter(state == state_name) %>%
    group_by(state) %>%
    mutate(days_since_10th_case = c(1:length(cases)))
  
  temp2 <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
    mutate(., per_10k_rank = rank(-total_tests_10k))
  
  temp2state <- temp2 %>%
    filter(state == state_name)
  
  currank <- temp2state$per_10k_rank
  curtotal <- tail(state_data_temp$tests, 1)
  curtotalcapita <- round(((curtotal / tail(state_data_temp$pop, 1))*10000),3)
  curtotal <- format((curtotal),big.mark=",",scientific=FALSE)
  
  if (state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    
    labels <- labs(x = 'Date', y = 'Daily Tests',
                   subtitle = paste0(curtotal, ' total tests (', curtotalcapita, ' per 10,000) ', currank, ' of 55.'))
    
  } else {
    
    labels <- labs(x = 'Date', y = 'Daily Tests',
                   subtitle = paste0(curtotal, ' total tests (', curtotalcapita, ' per 10,000) '))
    
    
  }
  
  
  p5 <- ggplot(state_data_temp, aes(x = date, y = daily_tests, group = state)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_tests), size = 2, alpha = .5, fill = 'blue') +
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_daily_tests),
              color = 'blue', size = 2.5, alpha = 1)+
    labels +
    theme_cowplot()
  
  if (length != 'All'){
    p5 <- p5 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p5)
}


positive_rate = function(state_name, length = 'All') {
  
  state_data_temp <- state_data %>%
    filter(cases > 10) %>%
    group_by(state) %>%
    mutate(days_since_10th_case = c(1:length(cases)))
  
  temp2 <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
    mutate(., rank = rank(-current_weekly_positive_rate))
  
  temp4 <- state_data_summary %>%
    filter(state == state_name)
  
  temp2state <- temp2 %>%
    filter(state == state_name)
  
  currank <- temp2state$rank
  curtotal <- round((temp4$current_weekly_positive_rate*100),2)
  
  temp2 <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
    mutate(., tests_per_10k_rank = rank(-total_tests_10k))
  
  temp3 <- state_data %>%
    filter(state == state_name)
  
  if(state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    
    labels <- labs(x = 'Date', y = 'Weekly Proportion Tests Positive',
                   subtitle = paste0(curtotal, '% positive test rate this week ', currank, ' of 55.'))
    
  } else {
    
    labels <- labs(x = 'Date', y = 'Weekly Proportion Tests Positive',
                   subtitle = paste0(curtotal, '% positive test rate this week '))
    
  }
  
  p6 <- ggplot(state_data_temp, aes(x = date, y = weekly_test_positive_rate, group = state)) +
    geom_line(data = state_data_temp[state_data_temp$state == 'US', ],
              aes(x = date, y = weekly_test_positive_rate, group = state),
              size = 1.5, linetype = 'dashed')+
    geom_line(data = state_data_temp[state_data_temp$state == state_name, ],
              aes(x = date, y = weekly_test_positive_rate, group = state),
              color = 'blue', size = 2.5, alpha = 1)+
    labels +
    theme_cowplot()
  
  if (length != 'All'){
    p6 <- p6 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)),
                               ylim = c(0,0.25))
  }
  
  return(p6)
}


mobility = function(state_name, length = 'All') {
  
  state_data_temp <- state_movement_data %>%
    filter(state == state_name)
  
  temp2 <- state_movement_data_summary %>%
    mutate(., workplace_rank = rank(current_weekly_avg_workplaces)) %>%
    mutate(., groc_pharm_rank = rank(current_weekly_avg_groc_pharm)) %>%
    mutate(., retail_rec_rank = rank(current_weekly_avg_retail_recreation))
  
  temp4 <- state_movement_data_summary %>%
    filter(state == state_name)
  
  temp2state <- temp2 %>%
    filter(state == state_name)
  
  cur_retrank_rank <- temp2state$retail_rec_rank
  cur_workplace_rank <- temp2state$workplace_rank
  cur_grocpharm_rank <- temp2state$groc_pharm_rank
  
  cur_retrank_avg <- round(temp4$current_weekly_avg_retail_recreation,1)
  cur_workplace_avg <- round(temp4$current_weekly_avg_workplaces,1)
  cur_grocpharm_avg <- round(temp4$current_weekly_avg_groc_pharm,1)
  cur_parks_avg <- round(temp4$current_weekly_avg_parks,1)
  cur_residential_avg <- round(temp4$current_weekly_avg_residential,1)
  cur_transit_avg <- round(temp4$current_weekly_avg_transit,1)
  
  #temp2 <- state_data_summary %>%
  #  filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
  #  mutate(., tests_per_10k_rank = rank(-total_tests_10k))
  
  temp3 <- state_data %>%
    filter(state == state_name)
  
  #if(state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
  
  #  labels <- labs(x = 'Date', y = 'Mobility',
  #                 subtitle = paste0('Workplace (Red): ', cur_workplace_avg, '% (', cur_workplace_rank, ')\n',
  #                                   'Retail and Recreation (Black): ', cur_retrank_avg, '% (', cur_retrank_rank, ')\n',
  #                                   'Grocery and Pharmacy (Blue): ', cur_grocpharm_avg, '% (', cur_grocpharm_rank, ')'))
  
  #} else {
  
  labels <- labs(x = 'Date', y = 'Mobility',
                 subtitle = paste0('Workplace (Red): ', cur_workplace_avg, '%\n',
                                   'Retail and Recreation (Black): ', cur_retrank_avg, '%\n',
                                   'Grocery and Pharmacy (Blue): ', cur_grocpharm_avg, '%\n',
                                   'Parks (Green): ', cur_parks_avg, '%\n',
                                   'Residential (Purple): ',cur_residential_avg, '%\n',
                                   'Transit (Orange): ',cur_transit_avg,'%')) 
  
  #}
  
  
  p7 <- ggplot(state_data_temp, aes(x = date, y = weekly_avg_workplaces, group = state)) +
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_workplaces, group = state),
              size = 1.5, color = 'red', alpha = .5)+
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_groc_pharm, group = state),
              size = 1.5, color = 'blue', alpha = .5)+
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_retail_recreation, group = state),
              size = 1.5, color = 'black', alpha = .5)+
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_parks, group = state),
              size = 1.5, color = 'green', alpha = .5)+
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_transit, group = state),
              size = 1.5, color = 'orange', alpha = .5)+
    geom_line(data = state_data_temp,
              aes(x = date, y = weekly_avg_residential, group = state),
              size = 1.5, color = 'purple', alpha = .5)+
    geom_hline(yintercept = 0, linetype = 'dashed') +
    theme_cowplot() +
    theme(legend.position = 'right') +
    labels +
    coord_cartesian(ylim = c(-100,100)) +
    coord_cartesian(xlim = c(head(temp3$date,1),tail(temp3$date,1)))
  
  if (length != 'All'){
    p7 <- p7 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
  }
  
  return(p7)
}

mobility('WA')

active_cases = function(state_name) {
  
  #state_data_temp <- state_data %>%
  #  filter(cases > 10) %>%
  #  group_by(state) %>%
  #  mutate(days_since_10th_case = c(1:length(cases)))
  
  #temp2 <- state_data_summary %>%
  #  filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
  #  mutate(., rank = rank(-current_weekly_positive_rate))
  
  #temp4 <- state_data_summary %>%
  #  filter(state == state_name)
  
  #temp2state <- temp2 %>%
  #  filter(state == state_name)
  
  #currank <- temp2state$rank
  #curtotal <- round((temp4$current_weekly_positive_rate*100),2)
  
  #temp2 <- state_data_summary %>%
  #  filter(state != 'US' & state != 'NR' & state != 'SR' & state != 'WR' & state != 'MR') %>%
  #  mutate(., tests_per_10k_rank = rank(-total_tests_10k))
  
  #temp3 <- state_data %>%
  #  filter(state == state_name)
  
  if(state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    
    labels <- labs(x = 'Date', y = 'Active Cases',
                   subtitle = 'paste')
    
  } else {
    
    labels <- labs(x = 'Date', y = 'Active Cases',
                   subtitle = paste0(curtotal, '% positive test rate this week '))
    
  }
  
  p8 <- ggplot(state_data, aes(x = date, y = active_cases, group = state)) +
    geom_line(data = state_data[state_data$state == state_name, ],
              aes(x = date, y = recovered , group = state),
              size = 1.5, linetype = 'dashed')+
    geom_line(data = state_data[state_data$state == state_name, ],
              aes(x = date, y = deaths , group = state),
              size = 1.5, linetype = 'dashed', color = 'red')+
    geom_line(data = state_data[state_data$state == state_name, ],
              aes(x = date, y = active_cases, group = state),
              color = 'blue', size = 2.5, alpha = 1)+
    labels +
    theme_cowplot()
  
  return(p8)
}

#active_cases('NH')

state_chart = function(state_name, length = 'All'){
  if(state_name != 'US' & state_name != 'NR' & state_name != 'SR' & state_name != 'MR' & state_name != 'WR'){
    p1 <- state_cases_chart(state_name, length)
    p8 <- state_deaths_chart(state_name, length)
    #p2 <- state_growth_chart(state_name)
    p3 <- covid_map(inc_vector = c(state_name),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    #p4 <- growth_chart(state_name)
    p5 <- testing_chart(state_name, length)
    p6 <- positive_rate(state_name, length)
    p7 <- mobility(state_name, length)
    multiplot(p1, p8, p3, p5, p6, p7, cols = 3)}
  else if (state_name == 'US') {
    p1 <- state_cases_chart('US', length)
    p8 <- state_deaths_chart('US', length)
    # p2 <- state_growth_chart('US')
    p3 <- covid_map(inc_vector = c(.us),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    # p4 <- growth_chart('US')
    p5 <- testing_chart('US', length)
    p6 <- positive_rate('US', length)
    p7 <- mobility('US', length)
    multiplot(p1, p8, p3, p5, p6, p7, cols = 3)
  }
  else if (state_name == 'NR') {
    p1 <- state_cases_chart('NR', length)
    p8 <- state_deaths_chart('NR', length)
    # p2 <- state_growth_chart('NR')
    p3 <- covid_map(inc_vector = c(.northeast_region),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    # p4 <- growth_chart('NR')
    p5 <- testing_chart('NR', length)
    p6 <- positive_rate('NR', length)
    multiplot(p1, p8, p3, p5, p6, cols = 3)
  }
  else if (state_name == 'MR'){
    p1 <- state_cases_chart('MR', length)
    p8 <- state_deaths_chart('MR', length)
    #p2 <- state_growth_chart('MR')
    p3 <- covid_map(inc_vector = c(.midwest_region),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    # p4 <- growth_chart('MR')
    p5 <- testing_chart('MR', length)
    p6 <- positive_rate('MR', length)
    multiplot(p1, p8, p3, p5, p6, cols = 3)
  }
  else if (state_name == 'SR'){
    p1 <- state_cases_chart('SR', length)
    p8 <- state_deaths_chart('SR', length)
    # p2 <- state_growth_chart('SR')
    p3 <- covid_map(inc_vector = c(.south_region),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    #  p4 <- growth_chart('SR')
    p5 <- testing_chart('SR', length)
    p6 <- positive_rate('SR', length)
    multiplot(p1, p8, p3, p5, p6, cols = 3)
  }
  else if (state_name == 'WR'){
    p1 <- state_cases_chart('WR', length)
    p8 <- state_deaths_chart('WR', length)
    # p2 <- state_growth_chart('WR')
    p3 <- covid_map(inc_vector = c(.west_region),
                    value = 'total_cases_exp_class',
                    no_cities = 0,
                    length = 65,
                    title = 'Total Covid-19 Cases by County',
                    legend.title = 'Total Cases',
                    legend.position = 'none',
                    curve_title = 'Infection Curve',
                    color_palette = 'YlOrRd',
                    palette_direction = 1,
                    state_border_color = 'black',
                    county_border_color = 'transparent',
                    include_curve = F,
                    state_labels = F)
    #  p4 <- growth_chart('WR')
    p5 <- testing_chart('WR', length)
    p6 <- positive_rate('WR', length)
    multiplot(p1, p8, p3, p5, p6, cols = 3)
  }
  else {
    print('Ya fucked up son')
  }
}

animate <- function(state1 = .us,
                    value = 'cases_per_10k',
                    title = 'United States Confirmed Covid-19 Cases per 10k pop by County',
                    scale_title = 'Cases per 10k Pop',
                    anim_length = 'All',
                    color = 'B',
                    scale_limits = 'Yes',
                    min = 0,
                    max = 100,
                    navalue = 'black',
                    state2 = '', state3 = '', state4 = '', state5 = '', state6 = '', state7 = '', state8 = '',
                    borders = 'Yes'
){
  
  if (borders == 'Yes'){
    bdrs = 'black'
  }  else if (borders == 'No') {
    bdrs = 'transparent'
  }
  
  #initialize length of animation
  dates_dummy <- county_data %>%
    group_by(date) %>%
    summarize(length(county))
  dates_for_animation <- dates_dummy$date
  
  if(anim_length == 'All'){
    start = 1
  } else {
    
    start = length(dates_for_animation) - anim_length
  }
  
  anim_length <- c(start:length(dates_for_animation))
  
  #Define our color scale
  if(scale_limits == 'Yes'){
    #color scale with limits
    colors <- scale_fill_viridis(name = scale_title, label = scales::comma, option = color,
                                 limits = c(min,max),
                                 na.value = navalue,)
    
  } else if (scale_limits == 'No'){
    #color scale WITHOUT limits
    colors <- scale_fill_viridis(name = scale_title, label = scales::comma, option = color,
                                 na.value = navalue)
  }
  
  #Run animation loop
  for (i in anim_length){
    
    plot_usmap(data = (county_data[county_data$date == dates_for_animation[i],]),
               value = value,
               regions = 'counties',
               include = c(state1, state2, state3, state4, state5, state6, state7, state8),
               color = bdrs) + 
      labs(title = title,
           subtitle = dates_for_animation[i])+
      theme(legend.position = 'right')+
      colors +
      theme(plot.title = element_text(face = 'bold', size = 20)) +
      theme(plot.subtitle = element_text(size = 18)) +
      theme(legend.title = element_text(size = 12)) +
      theme(legend.text = element_text(size = 12))
    # theme(panel.background = element_rect(color = '#1ED760', fill = '#1ED760'))
    
    print(dates_for_animation[i])
    
    ggsave(filename = paste('frame_', dates_for_animation[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
    
  }
}#save function here



calc_anim_length <- function(data = state_data){
  dates_dummy <- data %>%
    group_by(date) %>%
    summarize(length(state))
  dates_for_animation <- dates_dummy$date
  
  anim_length <- c(1:length(dates_for_animation))
  
  return(anim_length)
  
}

calc_dates_for_animation <- function(data = state_data){
  dates_dummy <- data %>%
    group_by(date) %>%
    summarize(length(state))
  dates_for_animation <- dates_dummy$date
  
  anim_length <- c(1:length(dates_for_animation))
  
  return(dates_for_animation)
  
}

curve_animation_state <- function(state, title){
  
  anim_length <- calc_anim_length()
  
  for (i in anim_length){
    
    temp_data <- state_data[state_data$state %in% state,] %>%
      group_by(date)%>%
      summarise(daily_cases = sum(daily_cases),
                cases = sum(cases))%>%
      mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
      filter(date == c(date[1]:date[i]))
    
    temp_data$weekly_avg_daily_cases[is.na(temp_data$weekly_avg_daily_cases)] = 0
    
    curtotalcases <- tail(temp_data$cases, n=1)
    curgrowthrate <- tail(temp_data$weekly_avg_daily_cases, n=1)
    
    curgrowthrate <- round(curgrowthrate, 0)
    
    curtotalcases <- format((curtotalcases),big.mark=",",scientific=FALSE)
    curgrowthrate <- format((curgrowthrate),big.mark=",",scientific=FALSE)
    
    
    state_data[state_data$state %in% state,] %>%
      group_by(date)%>%
      summarise(daily_cases = sum(daily_cases))%>%
      mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
      filter(date == c(date[1]:date[i])) %>%
      ggplot(., aes(date, daily_cases, group = 1)) +
      geom_col(fill = 'grey12', alpha = .1) +
      geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, color = 'red') +
      # geom_line(aes(date, fiveday_chg_wadcp), size = 1.5, alpha = .8, color = 'blue', linetype = 'dashed') +
      geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .25, fill = 'red') +
      theme_cowplot() +
      labs(x = 'Date', y = 'Daily Cases', title = title,
           subtitle = paste0(curtotalcases, ' total confirmed cases, growing at ', curgrowthrate, ' cases per day.' )) +
      scale_x_date(labels = date_format("%m/%d"))+
      theme(legend.position = 'none') +
      theme(plot.title = element_text(face = 'bold', size = 30)) +
      theme(plot.subtitle = element_text(size = 22))  +
      theme(axis.title = element_text(size = 18)) +
      theme(axis.text = element_text(size = 15))
    #geom_dl(aes(label = weekly_avg_daily_cases), method = list(dl.combine("first.points", "last.points"), cex = 0.8))
    
    ggsave(filename = paste('frame_', anim_length[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
  }
}

#curve_animation_state(.us, 'United States Infection Curve')

curve_animation_state_min <- function(state){
  
  anim_length <- calc_anim_length()
  
  for (i in anim_length){
    
    temp_data <- state_data[state_data$state == state,] %>%
      mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
      filter(date == c(date[1]:date[i]))
    
    temp_data_2 <- state_data[state_data$state == state,]
    
    temp_data$weekly_avg_daily_cases[is.na(temp_data$weekly_avg_daily_cases)] = 0
    
    curtotalcases <- tail(temp_data$cases, n=1)
    curgrowthrate <- tail(temp_data$weekly_avg_daily_cases, n=1)
    
    curgrowthrate <- round(curgrowthrate, 0)
    
    max_x = max(temp_data_2$date)
    min_x = min(temp_data_2$date)
    max_y = max(temp_data_2$weekly_avg_daily_cases, na.rm = T)
    
    
    temp_data %>%
      ggplot(., aes(date, weekly_avg_daily_cases, group = 1)) +
      #geom_col(fill = 'grey12', alpha = .1) +
      geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .5, color = 'red') +
      # geom_line(aes(date, fiveday_chg_wadcp), size = 1.5, alpha = .8, color = 'blue', linetype = 'dashed') +
      #geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .25, fill = 'red') +
      theme_cowplot() +
      labs(x = 'Date', y = 'Daily Cases', title = state,
           subtitle = paste0(curtotalcases, ' total confirmed cases, growing at ', curgrowthrate, ' cases per day.' )) +
      scale_x_date(labels = date_format("%m/%d"))+
      theme(legend.position = 'none') +
      theme(plot.title = element_text(face = 'bold', size = 18)) +
      theme(plot.subtitle = element_text(size = 15)) +
      coord_cartesian(xlim = c(min_x,max_x), ylim = c(0,max_y))
    
    
    
    ggsave(filename = paste('frame_', anim_length[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
  }
}


wcwadcp_animation_state <- function(state){
  
  anim_length <- calc_anim_length()
  
  for (i in anim_length){
    
    temp_data <- state_data[state_data$state == state,] %>%
      mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
      filter(date == c(date[1]:date[i]))
    
    temp_data$weekly_avg_daily_cases[is.na(temp_data$weekly_avg_daily_cases)] = 0
    
    curtotalcases <- tail(temp_data$cases, n=1)
    curgrowthrate <- tail(temp_data$weekly_avg_daily_cases, n=1)
    
    curgrowthrate <- round(curgrowthrate, 0)
    
    
    state_data[state_data$state == state,] %>%
      mutate(., weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
      mutate(., weekly_avg_daily_cases_10k = weekly_avg_daily_cases/pop) %>%
      mutate(., weekly_chg_wadcp = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k, 7)) %>%
      filter(date == c(date[1]:date[i])) %>%
      ggplot(., aes(date, daily_cases, group = 1)) +
      geom_line(aes(date, weekly_chg_wadcp), size = 1.5, alpha = .8, color = 'blue', linetype = 'dashed') +
      geom_hline(aes(yintercept = 0), size = 1, linetype = 'dashed') +
      theme_cowplot() +
      labs(x = 'Date', y = 'Weekly Change in the Weekly Average of Daily Cases per 10k Pop', title = state)+
      scale_x_date(labels = date_format("%m/%d"))+
      theme(legend.position = 'none') +
      theme(plot.title = element_text(face = 'bold', size = 18)) +
      theme(plot.subtitle = element_text(size = 15))
    
    ggsave(filename = paste('frame_', anim_length[i], '.png'), device = 'png', path = 'C:/Users/benpg/Desktop/Animation')
  }
}

cbsa_make_list <- function(variable, pop_limit, case_limit, state, direction = 1){
  cbsa_temp <- cbsa_data_summary %>%
    filter(state_abs %in% (state)) %>%
    filter(pop > pop_limit & total_cases > case_limit)
  
  if (direction == 1){
    cbsa_temp <- cbsa_temp %>%
      arrange(desc({{ variable }}))
  } else if (direction == -1){
    cbsa_temp <- cbsa_temp %>%
      arrange({{ variable }})
  }
  
  list <- cbsa_temp$cbsa
  list <- list[1:51]
  list <- as.vector(list)
  return(list)
}


state_make_list <- function(variable, pop_limit, case_limit, state, direction = 1){
  state_temp <- state_data_summary %>%
    filter(state != 'US' & state != 'NR' & state != 'MR' & state != 'SR' & state != 'WR' & state != 'GU' & state != 'PR') %>%
    filter(state %in% (state)) %>%
    filter(pop > pop_limit & total_cases > case_limit)
  
  
  if (direction == 1){
    state_temp <- state_temp %>%
      arrange(desc({{ variable }}))
  } else if (direction == -1){
    state_temp <- state_temp %>%
      arrange({{ variable }})
  }
  
  list <- state_temp$state
  list <- list[1:51]
  list <- as.vector(list)
  return(list)
}


cbsa_curve <- function(cbsa_name, length, ylim, variable, variable2){
  
  cbsa_data %>%
    filter(cbsa == cbsa_name) %>%
    ggplot(., aes(date, {{variable}})) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, {{variable2}}), size = 2, alpha = .5, fill = 'red') +
    geom_line(aes(date, {{variable2}}), size = 2, alpha = .8, color = 'red')+
    scale_x_date(labels = date_format("%m/%d")) +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(x = NULL, y = NULL, title = cbsa_name) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text = element_text(size = 8)) +
    coord_cartesian(xlim = c((tail(cbsa_data$date,1))-length,tail(cbsa_data$date,1))) +
    if (ylim != FALSE){
      coord_cartesian(ylim = c(0,ylim), xlim = c((tail(cbsa_data$date,1))-length,tail(cbsa_data$date,1)))
    }
}



state_curve <- function(state_name, length, ylim, variable, variable2){
  
  state_data %>%
    filter(state == state_name) %>%
    ggplot(., aes(date, {{variable}})) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, {{variable2}}), size = 2, alpha = .5, fill = 'red') +
    geom_line(aes(date, {{variable2}}), size = 2, alpha = .8, color = 'red')+
    scale_x_date(labels = date_format("%m/%d")) +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(x = NULL, y = NULL, title = ab_to_full(state_name)) +
    theme(plot.title = element_text(size = 10)) +
    theme(axis.text = element_text(size = 8)) +
    coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1))) +
    if (ylim != FALSE){
      coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    }
}

state_curve(state_name = 'VA',
            length = 14,
            ylim = 6,
            variable = daily_cases_10k,
            variable2 = weekly_avg_daily_cases_10k)

multi_var_cbsa_curve <- function(var, rank, length, pop_limit, case_limit, state, direction = 1, ylim,
                                 variable = daily_cases, variable2 = weekly_avg_daily_cases){
  
  list_for_loop <- cbsa_make_list({{ var }}, pop_limit, case_limit, state, direction)
  cbsa_curve(list_for_loop[rank], length, ylim, {{ variable }}, {{ variable2 }})
  
}


multi_var_state_curve <- function(var, rank, length, pop_limit, case_limit, state, direction = 1, ylim,
                                  variable = daily_cases, variable2 = weekly_avg_daily_cases){
  
  list_for_loop <- state_make_list({{ var }}, pop_limit, case_limit, state, direction)
  state_curve(list_for_loop[rank], length, ylim, {{ variable }}, {{ variable2 }})
  
}


multi_curve <- function(data, var, length, pop_limit, case_limit, state, direction = 1, ylim = FALSE,
                        variable = daily_cases, variable2 = weekly_avg_daily_cases, cols){
  
  if (data == 'Metro Area'){
    
    for (i in c(1:51)) {
      
      name <- paste0('plot', i)
      plot <- multi_var_cbsa_curve({{ var }}, i, length, pop_limit, case_limit, state, direction, ylim,
                                   {{ variable }}, {{ variable2 }})
      assign(name, plot)
      
    }
  } else if (data == 'State'){
    
    for (i in c(1:51)) {
      
      name <- paste0('plot', i)
      plot <- multi_var_state_curve({{ var }}, i, length, pop_limit, case_limit, state, direction, ylim,
                                    {{ variable }}, {{ variable2 }})
      assign(name, plot)
      
    }
  } else {
    print('ya fucked up son')
  }
  
  multiplot(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11, plot12,
            plot13, plot14, plot15, plot16, plot17, plot18, plot19, plot20, plot21, plot22, plot23, plot24,
            plot25, plot26, plot27, plot28, plot29, plot30, plot31, plot32, plot33, plot34, plot35, plot36,
            plot37, plot38, plot39, plot40, plot41, plot42, plot43, plot44, plot45, plot46, plot47, plot48,
            plot49, plot50, plot51,
            cols = cols)
}

view_sort <- function(data, variable, rows, direction){
  
  if (data == 'State'){
    
    predata <- state_data_summary
    if (direction == 1){
      
      temp <- predata %>%
        filter(state != 'US' & state != 'NR' & state != 'MR' & state != 'SR' & state != 'WR') %>%
        arrange(desc({{ variable }})) %>%
        select(state, {{variable}})
      
    } else if (direction == -1) {
      
      temp <- predata %>%
        filter(state != 'US' & state != 'NR' & state != 'MR' & state != 'SR' & state != 'WR') %>%
        arrange({{ variable }}) %>%
        select(state, {{variable}})
      
    }
    
  } else if (data == 'Metro Area') {
    
    predata <- cbsa_data_summary
    if (direction == 1){
      
      temp <- predata %>%
        arrange(desc({{ variable }})) %>%
        select(cbsa, {{variable}})
      
    } else if (direction == -1){
      
      temp <- predata %>%
        arrange({{ variable }}) %>%
        select(cbsa, {{variable}})
      
    }
    
  } else {
    print('ya fucked up son')
  }
  
  temp <- as.data.frame(temp)
  
  head(temp, rows)  
}

mobility_map <- function(data, state, value){
  
  if (data == 'State'){
    predata <- state_movement_data_summary
    reg <- 'states'
    borders <- 'black'
  } else if (data == 'County') {
    predata <- county_movement_data_summary
    reg <- 'counties'
    borders <- 'transparent'
  }
  
  if (value == 'current_weekly_avg_retail_recreation' | value == 'current_two_week_chg_retail_recreation'){
    loc <- 'Retail and Recreation'
  } else if (value == 'current_weekly_avg_groc_pharm' | value == 'current_two_week_chg_groc_pharm'){
    loc <- 'Grocery and Pharmacy'
  } else if (value == 'current_weekly_avg_parks' | value == 'current_two_week_chg_parks'){
    loc <- 'Parks'
  } else if (value == 'current_weekly_avg_transit' | value == 'current_two_week_chg_transit'){
    loc <- 'Transit Stations'
  } else if (value == 'current_weekly_avg_workplaces' | value == 'current_two_week_chg_workplaces'){
    loc <- 'Workplaces'
  } else if (value == 'current_weekly_avg_residential' | value == 'current_two_week_chg_residential'){
    loc <- 'Residences'
  }
  
  if (value == 'current_weekly_avg_retail_recreation' | value == 'current_weekly_avg_groc_pharm' |
      value == 'current_weekly_avg_parks' | value == 'current_weekly_avg_transit' |
      value == 'current_weekly_avg_workplaces' | value == 'current_weekly_avg_residential'){
    period <- 'Baseline'
  } else if (value == 'current_two_week_chg_retail_recreation' | value == 'current_two_week_chg_groc_pharm' |
             value == 'current_two_week_chg_parks' | value == 'current_two_week_chg_transit' |
             value == 'current_two_week_chg_workplaces' | value == 'current_two_week_chg_residential'){
    period <- 'Two Weeks Ago'
  }
  
  if (state == .us){
    place <- 'US'
  } else {
    place <- state
  }
  
  title <- paste0(place, ' Percent Change in Mobility to ', loc, ' from ', period)
  
  plot_usmap(data = predata,
             value = value,
             regions = reg,
             include = state,
             color = borders) + 
    labs(title = title)+
    theme(legend.position = 'right')+
    theme(plot.title = element_text(face = 'bold', size = 15)) +
    theme(plot.subtitle = element_text(size = 15)) +
    theme(legend.title = element_text(size = 12)) +
    theme(legend.text = element_text(size = 12)) +
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'green', name = NULL)
}

#Function to create a county data dataframe showing the number of new cases in the last X days
new_cases <- function(days){
  
  dates_dummy <- state_data %>%
    group_by(date) %>%
    summarize(length(state))
  
  dates <- dates_dummy$date
  end <- length(dates)
  start <- end - days 
  
  temp <- county_data %>%
    filter(date > dates[start]) %>%
    group_by(state, county, fips, pop, lat, lon, cbsa, rucc_2013, metro_nonmetro) %>%
    summarize(total_cases = sum(daily_cases),
              total_deaths = sum(daily_deaths),
              total_cases_10k = (total_cases / tail(pop, 1))*10000,
              total_cases_mil = (total_cases / tail(pop, 1))*1000000,
              total_deaths_10k = (total_deaths / tail(pop, 1))*10000,
              total_deaths_mil = (total_deaths / tail(pop, 1))*1000000,
              case_mort_rate = (total_deaths/total_cases)*100,
              current_case_mort_rate_lag = tail(case_mort_rate_lag,1),
              cwa_growth_rate = tail(weekly_avg_growth_rate, 1),
              cwa_daily_cases = tail(weekly_avg_daily_cases, 1),
              cwa_daily_cases_mil = (cwa_daily_cases / tail(pop, 1))*1000000,
              current_two_week_chg_weekly_avg_daily_cases = tail(two_week_chg_weekly_avg_daily_cases,1),
              current_one_week_chg_weekly_avg_daily_cases = tail(one_week_chg_weekly_avg_daily_cases,1),
              current_two_week_chg_daily_cases_10k = tail(two_week_chg_daily_cases_10k,1),
              current_two_week_chg_daily_cases_mil = tail(two_week_chg_daily_cases_mil,1),
              current_four_week_chg_daily_cases_mil = tail(four_week_chg_daily_cases_mil,1))
  
  temp$current_two_week_chg_weekly_avg_daily_cases_class <- cut(temp$current_two_week_chg_weekly_avg_daily_cases,
                                                                               breaks = c(-Inf,-1,-0.50,-.10,.10,0.50,1,Inf),
                                                                               labels = c('< -100%', '-100 to -50%', '-50 to -10%', '-10 to +10%', '+10 to +50%', '+50 to +100%', '> +100%'))
  
  temp$current_one_week_chg_weekly_avg_daily_cases_class <- cut(temp$current_one_week_chg_weekly_avg_daily_cases,
                                                                               breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                                               labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                                          '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                                          '+75 to +100%', '> +100%'))
  
  temp$total_cases_10k_class <- cut(temp$total_cases_10k, breaks  = c(-Inf,20,40,60,80,100,120,150,Inf),
                                                   labels = c('< 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100', '100 - 120', '120 - 150', '> 150'))
  
  temp$total_cases_10k_class_2 <- cut(temp$total_cases_10k, breaks  = c(-Inf,20,40,60,80,100,120,Inf),
                                                     labels = c('< 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100', '100 - 120', '> 120'))
  
  
  temp$total_cases_exp_class <- cut(temp$total_cases, breaks = c(0,10,100,1000,10000,Inf),
                                                   labels = c('< 10', '10 - 100', '100 - 1,000', '1,000 - 10,000', '> 10,000'))
  
  temp$total_cases_mil_exp_class <- cut(temp$total_cases_mil, breaks = c(0,10,100,1000,10000,Inf),
                                                       labels = c('< 10', '10 - 100', '100 - 1,000', '1,000 - 10,000', '> 10,000'))
  
  temp$cwa_growth_rate_class <- cut(temp$cwa_growth_rate, breaks = c(-5,0.5,1,1.5,2,2.5,3,3.5,4,Inf),
                                                   labels = c('< 0.5','0.5 - 1.0', '1.0 - 1.5', '1.5 - 2.0', '2.0 - 2.5', '2.5 - 3.0', '3.0 - 3.5', '3.5 - 4.0', '> 4.0'))
  
  temp$cwa_daily_cases_mil_class <- cut(temp$cwa_daily_cases_mil, breaks = c(-Inf,50,100,150,200,250,300,Inf),
                                                       labels = c('< 50', '50 - 100', '100 - 150', '150 - 200', '200 - 250', '250 - 300', '> 300'))
  
  temp$current_two_week_chg_daily_cases_10k_class <- cut(temp$current_two_week_chg_daily_cases_10k,
                                                                        breaks = c(-Inf,-1,-0.75,-0.5,-0.25,0.25,0.5,0.75,1,Inf),
                                                                        labels = c('< -1', '-1 to -0.75', '-0.75 to -0.5', '-0.5 to -0.25', '-0.25 to 0.25', '0.25 to 0.5',
                                                                                   '0.5 to 0.75', '0.75 to 1', '> 1'))
  
  temp$total_deaths_10k_class <- cut(temp$total_deaths_10k,
                                                    breaks = c(-Inf,2,4,6,8,10,12,15,Inf),
                                                    labels = c('< 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12', '12 - 15', '> 15'))
  
  temp$total_deaths_mil_class <- cut(temp$total_deaths_mil,
                                                    breaks = c(-Inf,1,10,100,1000,Inf),
                                                    labels = c('< 1', '1 - 10', '10 - 100', '100 - 1,000', '> 1,000'))
  
  temp$current_two_week_chg_daily_cases_mil_class <- cut(temp$current_two_week_chg_daily_cases_mil,
                                                                        breaks = c(-Inf, -200, -150, -100, -50, 50, 100, 150, 200, Inf),
                                                                        labels = c('< -200', '-200 to -150', '-150 to -100', '-100 to -50',
                                                                                   '-50 to 50', '50 to 100', '100 to 150',
                                                                                   '150 to 200', '> 200'))
  
  temp$total_cases_mil_class <- cut(temp$total_cases_mil,
                                                   breaks = c(-Inf,2000,4000,6000,8000,10000,12000,Inf),
                                                   labels = c('< 2,000', '2,000 - 4,000', '4,000 - 6,000',
                                                              '6,000 - 8,000', '8,000 - 10,000', '10,000 - 12,000', '> 12,000'))
  
  
  temp$current_four_week_chg_daily_cases_mil_class <- cut(temp$current_four_week_chg_daily_cases_mil,
                                                                         breaks = c(-Inf,-100,-75,-50,-25,25,50,75,100,Inf),
                                                                         labels = c('< -100', '-100 to -75',
                                                                                    '-75 to -50', '-50 to -25', '-25 to +25',
                                                                                    '+25 to +50', '+50 to +75', '+75 to +100',
                                                                                    '> +100'))
  
  temp <- as.data.frame(temp)
  
  return(temp)
}

new_cases(4)

#Function to map new cases in the last x days
new_cases_map <- function(state, days){
  
  temp <- new_cases(days)
  
  covid_map(data = temp,
            inc_vector = c(state),
            value = 'total_cases_exp_class',
            no_cities = 0,
            length = 65,
            title = paste0('New Covid-19 Cases over the past ',days,' days'),
            legend.title = 'Total Cases',
            curve_title = 'Infection Curve',
            palette_type = 'viridis',
            color_palette = 'C',
            palette_direction = 1,
            state_border_color = 'white',
            state_border_size = 1,
            full_labels = T,
            discrete.scale = T,
            label.color = 'white',
            county_border_color = 'black',
            include_curve = F,
            na.translate = T,
            na.value = 'black',
            legend_text_size = 12,
            title_size = 22,
            subtitle_size = 14,
            subtitle = 'Data: The New York Times',
            seed = 102,
            state_label_type = state_abs,
            state_label_size = 3,
            city_point_color = 'white',
            county_border_size = .4)
  
}

new_cases_map(.us, 3)

anim_length <- calc_anim_length()

new_today<- function(state_name){
  
  max_date <- max(state_data_live$date)
  
  temp <- state_data_live %>%
    filter(state == state_name) %>%
    filter(date == max_date) %>%
    select(state, daily_cases, daily_deaths)
  
  temp <- as.data.frame(temp)
  
  print(paste0('Data for today (', max_date, ') so far:'))
  #print(temp)
  
  return(temp)
}

city_labels <- function(include, no_cities){
  
  if (include == .northeast_region){
    city_temp <- city_data %>%
      filter(region == 'northeast')
  } else if (include == .midwest_region){
    city_temp <- city_data %>%
      filter(region == 'midwest')
  } else if (include == .south_region){
    city_temp <- city_data %>%
      filter(region == 'south')
  } else if (include == .west_region){
    city_temp <- city_data %>%
      filter(region == 'west')
  } else {
    city_temp <- city_data %>%
      filter(state_abs == include)
  }
  
  city_temp <- city_temp %>%
    arrange(desc(pop))
  
  city_temp <- city_temp[1:no_cities,]
  
  labels <- ggrepel::geom_label_repel(data = city_temp,
                                      aes(x = lon.1, y = lat.1, label = city),
                                      size = 3, alpha = 0.6,
                                      label.r = unit(0.5, 'lines'), label.size = 0.5,
                                      segment.color = 'black', segment.size = 1,
                                      seed = 1000)
  
  return(labels)
}

city_points <- function(include, no_cities){
  
  if (include == .northeast_region){
    city_temp <- city_data %>%
      filter(region == 'northeast')
  } else if (include == .midwest_region){
    city_temp <- city_data %>%
      filter(region == 'midwest')
  } else if (include == .south_region){
    city_temp <- city_data %>%
      filter(region == 'south')
  } else if (include == .west_region){
    city_temp <- city_data %>%
      filter(region == 'west')
  } else {
    city_temp <- city_data %>%
      filter(state_abs == include)
  }
  
  city_temp <- city_temp %>%
    arrange(desc(pop))
  
  city_temp <- city_temp[1:no_cities,]
  
  points <- geom_point(data = city_temp,
                       aes(x = lon.1, y = lat.1))
  
  return(points)
}

covid_map <- function(data = county_data_summary,
                      inc_vector = .us,
                      value = 'total_cases_exp_class',
                      no_cities = 10,
                      title = 'Total Covid-19 Cases by County',
                      subtitle = '',
                      length = 'All',
                      legend.title = 'Total Cases',
                      legend.position = 'right',
                      curve_title = 'Infection Curve',
                      palette_type = 'brewer',
                      color_palette = 'YlOrRd',
                      palette_direction = 1,
                      state_border_color = 'black',
                      county_border_color = 'transparent',
                      include_curve = TRUE,
                      label.color = 'white',
                      interstates = FALSE,
                      na.translate = FALSE,
                      na.value = 'grey',
                      seed = 1000,
                      title_size = 18,
                      title_present = TRUE,
                      state_border_size = .4,
                      legend_text_size = 10,
                      legend_title_size = 15,
                      caption = '',
                      caption.hjust = 0,
                      caption.size = 10,
                      subtitle_size = 10,
                      subtitle.hjust = 0,
                      title.hjust = 0,
                      facets = FALSE,
                      full_labels = FALSE,
                      state_label_type = state_abs,
                      state_label_size = 3,
                      city_point_color = 'white',
                      county_border_size = .4,
                      city_labels = T,
                      discrete.scale = T,
                      alpha = 1){
  
  # Filter city data
  city_temp <- city_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    arrange(desc(pop))
  
  # Filter state center data
  state_centers_temp <- state_centers %>%
    filter(state_abs %in% (inc_vector)) %>%
    arrange(desc(area))
  
  # Create base county map
  map1 <- plot_usmap(data = data,
                     value = value,
                     regions = 'counties',
                     include = inc_vector,
                     color = county_border_color,
                     size = county_border_size,
                     alpha = alpha)
  
  # Create map for state borders
  state_borders <- plot_usmap(regions = "states",
                              include = inc_vector,
                              exclude = 'DC',
                              fill = 'transparent',
                              size = state_border_size,
                              color = state_border_color,
                              labels = F,
                              label_color = label.color)
  
  # Create labels for cities
  labels <- ggrepel::geom_label_repel(data = city_temp[0:no_cities,],
                                      aes(x = lon.1, y = lat.1, label = city),
                                      size = 2.8, alpha = 0.7,
                                      label.r = unit(.25, 'lines'), label.size = 0.5,
                                      segment.color = 'black', segment.size = 1,
                                      seed = seed)
  
  # Create points for cities
  points <- geom_point(data = city_temp[0:no_cities,],
                       aes(x = lon.1, y = lat.1), size = 2,
                       shape = 21, fill = city_point_color,
                       color = 'black', stroke = 1.5)
  
  # Create labels for states
  #state_labs <- ggrepel::geom_label_repel(data = state_centers_temp[0:no_states,],
  #                                        aes(x = lon.1, y = lat.1, label = state),
  #                                        size = 3, alpha = 0.5,
  #                                        fontface = 'bold',
  #                                        color = label.color,
  #                                        label.r = unit(0, 'lines'), label.size = 0.5,
  #                                       segment.size = 1,
  #                                        point.padding = NA, force = .5, seed = seed)
  state_labs <- geom_shadowtext(data = state_centers_temp,
                                aes(x = lon.1, y = lat.1, label = {{state_label_type}}),
                                size = state_label_size, alpha = 1,
                                fontface = 'bold',
                                color = label.color,
                                label.r = unit(0, 'lines'), label.size = 0.5,
                                segment.size = 1,
                                point.padding = NA, force = .5, seed = seed,
                                check_overlap = T)
  
  # Figure out what kinda color scales we want
  if (palette_type == 'brewer'){
    colors <- scale_fill_brewer(palette=color_palette, direction = palette_direction,
                                na.value = na.value, name = legend.title, na.translate = na.translate, drop = FALSE)
  } else if (palette_type == 'viridis') {
    if (discrete.scale == F){
      colors <- scale_fill_viridis(option = color_palette, direction = palette_direction,
                                   na.value = na.value, name = legend.title,
                                   discrete = F)
    } else if (discrete.scale == T){
      colors <- scale_fill_viridis(option = color_palette, direction = palette_direction,
                                   na.value = na.value, name = legend.title,
                                   na.translate = na.translate, drop = FALSE, discrete = T)
    }
  }
  
  # Final map assembly
  final_map <- ggplot()+
    map1$layers[[1]] +
    state_borders$layers +
    map1$theme +
    coord_equal() +
    labels +
    points +
    labs(title = title, subtitle = subtitle, caption = caption)+
    theme(legend.position = legend.position) +
    theme(legend.text = element_text(size = legend_text_size)) +
    theme(legend.title = element_text(size = legend_title_size)) +
    theme(plot.title = element_text(face = 'bold', size = title_size, hjust = title.hjust)) +
    theme(plot.caption = element_text(hjust = caption.hjust, size = caption.size))+
    theme(plot.subtitle = element_text(size = subtitle_size, hjust = subtitle.hjust))+
    colors
  #guides(fill = guide_legend(reverse = T))
  
  
  if (title_present == FALSE) {
    final_map <- final_map +
      theme(title = element_blank())
  }
  
  if (full_labels == TRUE){
    final_map <- final_map + 
      state_labs
  }
  
  #What to plot
  if (include_curve == TRUE){
    # Daily cases curve
    curve <- state_cases_chart_2(inc_vector = inc_vector, length = length, title = curve_title)
    
    #Multiplot
    mplot <- multiplot(curve, final_map, cols = 2)
    mplot
  } else {
    final_map
  }
  
}

#Writing the function to make the daily cases chart
state_cases_chart_3 = function(inc_vector,
                               length = 'All',
                               growth_change = 14,
                               title = 'Infection Curve',
                               yaxis = FALSE,
                               axis_text_size = 12,
                               ylim = 20000){
  
  #empty_list <- vector(mode = "list", length = length(inc_vector))
  #
  #for (state in inc_vector){
  #  fips <- fips(state)
  #  fipsinfo <- fips_info(fips)
  #  st <- fipsinfo$full
  #  append(empty_list, st)
  #}
  
  #print(empty_list)
  
  #livetemp <- state_data_live %>%
  #  filter(state %in% (inc_vector)) %>%
  #  group_by(date) %>%
  #  summarize(daily_cases = sum(daily_cases),
  #            cases = sum(cases))
  
  temp2 <- state_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(state_abs)%>%
    summarize(pop = tail(pop,1))
  
  pop <- sum(temp2$pop)
  
  #new_today <- tail(livetemp$daily_cases,1)
  #new_today <- format((new_today),big.mark=",",scientific=FALSE)
  
  temp <- state_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              daily_cases = sum(daily_cases),
              daily_deaths = sum(daily_deaths)) %>%
    mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
    mutate(chgrate = (weekly_avg_daily_cases/(lag(weekly_avg_daily_cases,growth_change)))-1)
  
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
  
  
  p1 <- ggplot(temp, aes(date, daily_cases, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .65, fill = 'red') +
    geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
    scale_x_date(labels = date_format("%m/%d"))+
    labs(x = 'Date', y = 'Daily Cases') +
    theme(axis.text = element_text(size = axis_text_size)) +
    #geom_vline(xintercept = (tail(state_data$date,1))-growth_change, size = 1, linetype = 'dashed') +
    scale_color_viridis(option = 'A') +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(title = title, subtitle = paste0(curtotal, ' Total Cases (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                                          currate, ' new cases/day\n', direction, chgrate, '% from ',
                                          growth_change, ' days ago\n'))
  
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    if (yaxis == TRUE){
      p1 <- p1 + coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    } else p1 <- p1
  }
  
  
  
  return(p1)
}

#Writing the function to make the daily cases chart
state_deaths_chart_3 = function(inc_vector,
                               length = 'All',
                               growth_change = 14,
                               title = 'Infection Curve',
                               yaxis = FALSE,
                               axis_text_size = 12,
                               ylim = 20000){
  
  #empty_list <- vector(mode = "list", length = length(inc_vector))
  #
  #for (state in inc_vector){
  #  fips <- fips(state)
  #  fipsinfo <- fips_info(fips)
  #  st <- fipsinfo$full
  #  append(empty_list, st)
  #}
  
  #print(empty_list)
  
  #livetemp <- state_data_live %>%
  #  filter(state %in% (inc_vector)) %>%
  #  group_by(date) %>%
  #  summarize(daily_cases = sum(daily_cases),
  #            cases = sum(cases))
  
  temp2 <- state_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(state_abs)%>%
    summarize(pop = tail(pop,1))
  
  pop <- sum(temp2$pop)
  
  #new_today <- tail(livetemp$daily_cases,1)
  #new_today <- format((new_today),big.mark=",",scientific=FALSE)
  
  temp <- state_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              daily_cases = sum(daily_cases),
              daily_deaths = sum(daily_deaths)) %>%
    mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
    mutate(chgrate = (weekly_avg_daily_deaths/(lag(weekly_avg_daily_deaths,growth_change)))-1)
  
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
  
  
  p1 <- ggplot(temp, aes(date, daily_deaths, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .65, fill = 'black') +
    geom_line(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .8, color = 'black')+
    scale_x_date(labels = date_format("%m/%d"))+
    labs(x = 'Date', y = 'Daily Cases') +
    theme(axis.text = element_text(size = axis_text_size)) +
    #geom_vline(xintercept = (tail(state_data$date,1))-growth_change, size = 1, linetype = 'dashed') +
    scale_color_viridis(option = 'A') +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(title = title, subtitle = paste0(curtotal, ' Total Deaths (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                                          currate, ' new deaths/day\n', direction, chgrate, '% from ',
                                          growth_change, ' days ago\n'))
  
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    if (yaxis == TRUE){
      p1 <- p1 + coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    } else p1 <- p1
  }
  
  
  
  return(p1)
}
state_cases_chart_3(.us)

#Writing the function to make the daily cases chart
state_cases_chart_2 = function(inc_vector,
                               length = 'All',
                               growth_change = 14,
                               title = 'Infection Curve',
                               yaxis = FALSE,
                               axis_text_size = 12,
                               ylim = 20000){
  
  livetemp <- state_data_live %>%
    filter(state %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(daily_cases = sum(daily_cases),
              cases = sum(cases))
  
  temp2 <- state_data %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(state_abs)%>%
    summarize(pop = tail(pop,1))
  
  pop <- sum(temp2$pop)
  
  new_today <- tail(livetemp$daily_cases,1)
  new_today <- format((new_today),big.mark=",",scientific=FALSE)
  
  temp <- state_data %>%
    filter(state %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              daily_cases = sum(daily_cases),
              daily_deaths = sum(daily_deaths)) %>%
    mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
    mutate(chgrate = (weekly_avg_daily_cases/(lag(weekly_avg_daily_cases,growth_change)))-1)
  
  curtotal <- tail(livetemp$cases, 1)
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
  
  
  p1 <- ggplot(temp, aes(date, daily_cases, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_cases), size = 2, alpha = .65, fill = 'red') +
    geom_line(aes(date, weekly_avg_daily_cases), size = 2, alpha = .8, color = 'red')+
    scale_x_date(labels = date_format("%m/%d"))+
    labs(x = 'Date', y = 'Daily Cases') +
    theme(axis.text = element_text(size = axis_text_size)) +
    #geom_vline(xintercept = (tail(state_data$date,1))-growth_change, size = 1, linetype = 'dashed') +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(title = title, subtitle = paste0(curtotal, ' Total Cases (', curtotalcap, ' per 10,000 People)\nGrowing at ',
                                          currate, ' new cases/day\n', direction, chgrate, '% from ',
                                          growth_change, ' days ago'))
  #new_today, ' cases added so far today'))
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    if (yaxis == TRUE){
      p1 <- p1 + coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    } else p1 <- p1
  }
  
  
  
  return(p1)
}

#Writing the function to make the daily deaths chart
state_deaths_chart_2 = function(inc_vector,
                                length = 'All',
                                growth_change = 14,
                                title = 'Death Curve',
                                yaxis = FALSE,
                                axis_text_size = 12,
                                ylim = 20000){
  
  livetemp <- state_data_live %>%
    filter(state_abs %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(daily_deaths = sum(daily_deaths),
              deaths = sum(deaths))
  
  new_today <- tail(livetemp$daily_deaths,1)
  new_today <- format((new_today),big.mark=",",scientific=FALSE)
  
  temp <- state_data %>%
    filter(state %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(cases = sum(cases),
              deaths = sum(deaths),
              daily_cases = sum(daily_cases),
              daily_deaths = sum(daily_deaths)) %>%
    mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
    mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
    mutate(chgrate = (weekly_avg_daily_deaths/(lag(weekly_avg_daily_deaths,growth_change)))-1)
  
  curtotal <- tail(livetemp$deaths, 1)
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
  
  p1 <- ggplot(temp, aes(date, daily_deaths, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .65, fill = 'black') +
    geom_line(aes(date, weekly_avg_daily_deaths), size = 2, alpha = .8, color = 'black')+
    scale_x_date(labels = date_format("%m/%d"))+
    labs(x = 'Date', y = 'Daily Deaths') +
    theme(axis.text = element_text(size = axis_text_size)) +
    #geom_vline(xintercept = (tail(state_data$date,1))-growth_change, size = 1, linetype = 'dashed') +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(title = title, subtitle = paste0(curtotal, ' Total Deaths\nGrowing at ',
                                          currate, ' new deaths/day\n', direction, chgrate, '% from ',
                                          growth_change, ' days ago\n',
                                          new_today, ' deaths added so far today'))
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    if (yaxis == TRUE){
      p1 <- p1 + coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))
    } else p1 <- p1
  }
  
  return(p1)
}

multi_map <- function(inc_vector = .us,
                      no_cities = 10,
                      state_border_color = 'black',
                      county_border_color = 'transparent',
                      state_labels = T){
  # Total cumulative cases by county on an exponential scale
  p1 <- covid_map(inc_vector = c(inc_vector),
                  value = 'total_cases_10k_class',
                  no_cities = no_cities,
                  length = 65,
                  title = 'Total Covid-19 Cases by County',
                  legend.title = 'Total Cases\nper 10 Thousand\nPeople',
                  color_palette = 'YlOrRd',
                  palette_direction = 1,
                  state_border_color = state_border_color,
                  county_border_color = county_border_color,
                  include_curve = F,
                  full_labels = state_labels,
                  legend_text_size = 8,
                  legend_title_size = 10)
  
  # Current growth rate per million people
  p2 <- covid_map(inc_vector = c(inc_vector),
                  value = 'cwa_daily_cases_mil_class',
                  no_cities = no_cities,
                  length = 65,
                  title = 'Infection Rate',
                  legend.title = 'New Cases per\nMillion People\nper Week',
                  color_palette = 'OrRd',
                  palette_direction = 1,
                  state_border_color = state_border_color,
                  county_border_color = county_border_color,
                  include_curve = F,
                  full_labels = state_labels,
                  legend_text_size = 8,
                  legend_title_size = 10)
  
  # Percent Change in Weekly Avg of Daily Cases over the last two weeks
  p3 <- covid_map(inc_vector = c(inc_vector),
                  value = 'current_two_week_chg_daily_cases_mil_class',
                  no_cities = no_cities,
                  length = 65,
                  title = 'Two Week Change in Infection Rate',
                  legend.title = 'Two-Week Change in\nNew Cases per Million\nPeople per Week',
                  color_palette = 'RdYlBu',
                  palette_direction = -1,
                  state_border_color = state_border_color,
                  county_border_color = county_border_color,
                  include_curve = F,
                  full_labels = state_labels,
                  legend_text_size = 8,
                  legend_title_size = 10)
  
  # Percent Change in Weekly Avg of Daily Cases over the last two weeks
  p4 <- covid_map(inc_vector = c(inc_vector),
                  value = 'total_deaths_10k_class',
                  no_cities = no_cities,
                  length = 65,
                  title = 'Total Deaths per Capita',
                  legend.title = 'Deaths per\n10 Thousand\nPeople',
                  color_palette = 'RdPu',
                  palette_direction = 1,
                  state_border_color = state_border_color,
                  county_border_color = county_border_color,
                  include_curve = F,
                  full_labels = state_labels,
                  legend_text_size = 8,
                  legend_title_size = 10)
  
  p5 <- state_cases_chart_3(inc_vector = c(inc_vector),
                            length = 70,
                            title = 'Infection Curve')
  
  p6 <- state_deaths_chart_2(inc_vector = c(inc_vector),
                             length = 70,
                             title = 'Death Curve')
  
  multiplot(p1, p2, p3, p4, p5, p6, cols = 3)
}

regional_curves <- function(title = NULL,
                            no_cities = 5,
                            county_border_color = 'transparent',
                            direction = 'Horizontal',
                            value = 'total_cases_exp_class',
                            legend.title = 'Total Cases',
                            color_palette = 'YlOrRd',
                            palette_direction = 1,
                            yaxis = FALSE,
                            ylim = 20000,
                            length = 65){
  
  NRmap <- covid_map(inc_vector = c(.northeast_region),
                     value = value,
                     no_cities = no_cities,
                     length = length,
                     title = title,
                     legend.title = legend.title,
                     curve_title = 'Northeast Region',
                     color_palette = color_palette,
                     palette_direction = palette_direction,
                     state_border_color = 'black',
                     county_border_color = county_border_color,
                     include_curve = F,
                     full_labels = T,
                     label.color = 'white',
                     state_label_type = state_abs)
  
  NRcurve <- state_cases_chart_2(inc_vector = c(.northeast_region),
                                 length = length,
                                 title = 'Northeast Region',
                                 yaxis = yaxis,
                                 ylim = ylim)
  
  MRmap <- covid_map(inc_vector = c(.midwest_region),
                     value = value,
                     no_cities = no_cities,
                     length = length,
                     title = title,
                     legend.title = legend.title,
                     curve_title = 'Midwest Region',
                     legend.position = 'none',
                     color_palette = color_palette,
                     palette_direction = palette_direction,
                     state_border_color = 'black',
                     county_border_color = county_border_color,
                     include_curve = F,
                     full_labels = T,
                     label.color = 'white',
                     state_label_type = state_abs)
  
  MRcurve <- state_cases_chart_2(inc_vector = c(.midwest_region),
                                 length = length,
                                 title = 'Midwest Region',
                                 yaxis = yaxis,
                                 ylim = ylim)
  
  SRmap <- covid_map(inc_vector = c(.south_region),
                     value = value,
                     no_cities = no_cities,
                     length = length,
                     title = title,
                     legend.title = legend.title,
                     curve_title = 'South Region',
                     legend.position = 'none',
                     color_palette = color_palette,
                     palette_direction = palette_direction,
                     state_border_color = 'black',
                     county_border_color = county_border_color,
                     include_curve = F,
                     full_labels = T,
                     label.color = 'white',
                     state_label_type = state_abs)
  
  SRcurve <- state_cases_chart_2(inc_vector = c(.south_region),
                                 length = length,
                                 title = 'South Region',
                                 yaxis = yaxis,
                                 ylim = ylim)
  
  WRmap <- covid_map(inc_vector = c(.west_region),
                     value = value,
                     no_cities = no_cities,
                     length = length,
                     title = title,
                     legend.title = legend.title,
                     curve_title = 'West Region',
                     color_palette = color_palette,
                     palette_direction = palette_direction,
                     legend.position = 'none',
                     state_border_color = 'black',
                     county_border_color = county_border_color,
                     include_curve = F,
                     full_labels = T,
                     label.color = 'white',
                     state_label_type = state_abs)
  
  WRcurve <- state_cases_chart_2(inc_vector = c(.west_region),
                                 length = length,
                                 title = 'West Region',
                                 yaxis = yaxis,
                                 ylim = ylim)
  
  
  if (direction == 'Horizontal'){
    multiplot(NRcurve, SRcurve, NRmap, SRmap, MRcurve, WRcurve, MRmap, WRmap, cols = 4)
  } else if (direction == 'Vertical'){
    multiplot(NRcurve, SRcurve, MRcurve, WRcurve, NRmap, SRmap, MRmap, WRmap, cols = 2)
  }
}

subregional_curves <- function(title = NULL,
                               no_cities = 5,
                               county_border_color = 'transparent',
                               direction = 'Horizontal',
                               value = 'total_cases_exp_class',
                               legend.title = 'Total Cases',
                               color_palette = 'YlOrRd',
                               palette_direction = 1,
                               axis_text_size = 12,
                               yaxis = FALSE,
                               ylim = 20000){
  
  map1 <- covid_map(inc_vector = c(.new_england),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'New England',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve1 <- state_cases_chart_2(inc_vector = c(.new_england),
                                length = 65,
                                title = 'New England',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  
  map2 <- covid_map(inc_vector = c('NY', 'NJ', 'PA', 'MD', 'DE', 'DC'),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Mid Atlantic',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve2 <- state_cases_chart_2(inc_vector = c('NY', 'NJ', 'PA', 'MD', 'DE', 'DC'),
                                length = 65,
                                title = 'Mid Atlantic',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map3 <- covid_map(inc_vector = c('FL', 'GA', 'SC', 'NC', 'VA', 'WV'),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'South Atlantic',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve3 <- state_cases_chart_2(inc_vector = c('FL', 'GA', 'SC', 'NC', 'VA', 'WV'),
                                length = 65,
                                title = 'South Atlantic',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map4 <- covid_map(inc_vector = c(.east_south_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'East South Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve4 <- state_cases_chart_2(inc_vector = c(.east_south_central),
                                length = 65,
                                title = 'East South Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map5 <- covid_map(inc_vector = c(.east_north_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'East North Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve5 <- state_cases_chart_2(inc_vector = c(.east_north_central),
                                length = 65,
                                title = 'East North Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map6 <- covid_map(inc_vector = c(.west_south_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'West South Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve6 <- state_cases_chart_2(inc_vector = c(.west_south_central),
                                length = 65,
                                title = 'West South Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  
  map7 <- covid_map(inc_vector = c(.west_north_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'West North Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve7 <- state_cases_chart_2(inc_vector = c(.west_north_central),
                                length = 65,
                                title = 'West North Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map8 <- covid_map(inc_vector = c(.mountain),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Mountain',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve8 <- state_cases_chart_2(inc_vector = c(.mountain),
                                length = 65,
                                title = 'Mountain',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map9 <- covid_map(inc_vector = c(.pacific),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Pacific',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve9 <- state_cases_chart_2(inc_vector = c(.pacific),
                                length = 65,
                                title = 'Pacific',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  if (direction == 'Horizontal'){
    multiplot(curve1, curve2, curve3, map1, map2, map3, curve4, curve5, curve6, map4, map5, map6, curve7, curve8, curve9, map7, map8, map9, cols = 6)
  } else if (direction == 'Vertical'){
    multiplot(NRcurve, SRcurve, MRcurve, WRcurve, NRmap, SRmap, MRmap, WRmap, cols = 2)
  }
}


subregional_testing_curves <- function(title = NULL,
                               no_cities = 5,
                               county_border_color = 'transparent',
                               direction = 'Horizontal',
                               value = 'total_cases_exp_class',
                               legend.title = 'Total Cases',
                               color_palette = 'YlOrRd',
                               palette_direction = 1,
                               axis_text_size = 12,
                               yaxis = FALSE,
                               ylim = 20000){
  
  map1 <- covid_map(inc_vector = c(.new_england),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'right',
                    curve_title = 'New England',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    legend_title_size = 12,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve1 <- state_tests_chart_3(inc_vector = c(.new_england),
                                length = 70,
                                title = 'New England',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  
  map2 <- covid_map(inc_vector = c('NY', 'NJ', 'PA', 'MD', 'DE', 'DC'),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Mid Atlantic',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve2 <- state_tests_chart_3(inc_vector = c('NY', 'NJ', 'PA', 'MD', 'DE', 'DC'),
                                length = 70,
                                title = 'Mid Atlantic',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map3 <- covid_map(inc_vector = c('FL', 'GA', 'SC', 'NC', 'VA', 'WV'),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'South Atlantic',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve3 <- state_tests_chart_3(inc_vector = c('FL', 'GA', 'SC', 'NC', 'VA', 'WV'),
                                length = 70,
                                title = 'South Atlantic',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map4 <- covid_map(inc_vector = c(.east_south_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'East South Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve4 <- state_tests_chart_3(inc_vector = c(.east_south_central),
                                length = 70,
                                title = 'East South Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map5 <- covid_map(inc_vector = c(.east_north_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'East North Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve5 <- state_tests_chart_3(inc_vector = c(.east_north_central),
                                length = 70,
                                title = 'East North Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map6 <- covid_map(inc_vector = c(.west_south_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'West South Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve6 <- state_tests_chart_3(inc_vector = c(.west_south_central),
                                length = 70,
                                title = 'West South Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  
  map7 <- covid_map(inc_vector = c(.west_north_central),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'West North Central',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve7 <- state_tests_chart_3(inc_vector = c(.west_north_central),
                                length = 70,
                                title = 'West North Central',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map8 <- covid_map(inc_vector = c(.mountain),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Mountain',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve8 <- state_tests_chart_3(inc_vector = c(.mountain),
                                length = 70,
                                title = 'Mountain',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  map9 <- covid_map(inc_vector = c(.pacific),
                    value = value,
                    no_cities = no_cities,
                    length = 65,
                    title = title,
                    legend.title = legend.title,
                    legend.position = 'none',
                    curve_title = 'Pacific',
                    color_palette = color_palette,
                    palette_direction = palette_direction,
                    state_border_color = 'black',
                    county_border_color = county_border_color,
                    include_curve = F,
                    full_labels = T,
                    label.color = 'white',
                    state_label_type = state_abs)
  
  curve9 <- state_tests_chart_3(inc_vector = c(.pacific),
                                length = 70,
                                title = 'Pacific',
                                yaxis = yaxis,
                                axis_text_size = axis_text_size,
                                ylim = ylim)
  
  if (direction == 'Horizontal'){
    multiplot(curve1, curve2, curve3, map1, map2, map3, curve4, curve5, curve6, map4, map5, map6, curve7, curve8, curve9, map7, map8, map9, cols = 6)
  } else if (direction == 'Vertical'){
    multiplot(NRcurve, SRcurve, MRcurve, WRcurve, NRmap, SRmap, MRmap, WRmap, cols = 2)
  }
}
#Writing the function to make the daily cases chart
state_tests_chart_3 = function(inc_vector,
                               length = 'All',
                               growth_change = 14,
                               title = 'Infection Curve',
                               yaxis = FALSE,
                               axis_text_size = 12,
                               ylim = 20000){
  
  #livetemp <- state_data_live %>%
  #  filter(state %in% (inc_vector)) %>%
  #  group_by(date) %>%
  #  summarize(daily_cases = sum(daily_cases),
  #            cases = sum(cases))
  
  #temp2 <- state_data %>%
  #  filter(state %in% (inc_vector)) %>%
  #  summarize(pop = tail(pop,1))
  
  #pop <- sum(temp2$pop)
  
  #new_today <- tail(livetemp$daily_cases,1)
  #new_today <- format((new_today),big.mark=",",scientific=FALSE)
  
  temp <- state_data %>%
    filter(state %in% (inc_vector)) %>%
    group_by(date) %>%
    summarize(tests = sum(tests),
              cases = sum(cases),
              daily_tests = sum(daily_tests),
              daily_cases = sum(daily_cases)) %>%
    mutate(daily_test_positive_rate = (daily_cases/daily_tests)*100) %>%
    mutate(weekly_avg_daily_test_positive_rate = frollmean(daily_test_positive_rate, 7)) %>%
    mutate(weekly_avg_daily_tests = frollmean(daily_tests, 7))
  
  temp2 <- state_data %>%
    filter(state %in% (inc_vector)) %>%
    summarize(pop = tail(pop,1))
  
  pop <- sum(temp2$pop)
  
  curtests <- tail(temp$tests, 1)
  currate <- round(tail(temp$weekly_avg_daily_test_positive_rate, 1),2)
  curcases <- round(tail(temp$cases, 1),0)
  totrate <- round(((curcases/curtests)*100),2)
  testspcapita <- round(((curtests/pop)*10000),2)
  
  #if (chgrate > 0){
  #  direction <- 'Up '
  #} else {
  #  direction <- 'Down '
  #}
  
  #chgrate <- abs(chgrate)
  
  curtests <- format((curtests),big.mark=",",scientific=FALSE)
  currate <- format((currate),big.mark=",",scientific=FALSE)
  curcases <- format((curcases),big.mark=",",scientific=FALSE)
  testspcapita <- format((testspcapita),big.mark=",",scientific=FALSE)
  
  p1 <- ggplot(temp, aes(date, daily_tests, group = 1)) +
    geom_col(fill = 'grey12', alpha = .1) +
    geom_area(aes(date, weekly_avg_daily_tests), size = 2, alpha = .65, fill = 'blue') +
    geom_line(aes(date, weekly_avg_daily_tests), size = 2, alpha = .8, color = 'blue')+
    scale_x_date(labels = date_format("%b%d"))+
    labs(x = 'Date', y = 'Test Positive Rate') +
    theme(axis.text = element_text(size = axis_text_size)) +
    #geom_vline(xintercept = (tail(state_data$date,1))-growth_change, size = 1, linetype = 'dashed') +
    scale_color_viridis(option = 'A') +
    theme_cowplot() +
    theme(legend.position = 'none') +
    labs(title = title, subtitle = paste0(curtests, ' Total Tests\n',
                                          testspcapita, ' Tests per 10,000')) +
                                          #currate, '% Positive Rate This Week\n',
                                          #totrate, '% Positive Rate Overall')) +
    theme(plot.title = element_text(size = 18), plot.subtitle = element_text(size = 14)) +
    theme(axis.title = element_text(size = 12))
  
  if (length != 'All'){
    p1 <- p1 + coord_cartesian(xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1)))  +
      theme(axis.text = element_text(size = axis_text_size))
    if (yaxis == TRUE){
      p1 <- p1 + coord_cartesian(ylim = c(0,ylim), xlim = c((tail(state_data$date,1))-length,tail(state_data$date,1))) +
        theme(axis.text = element_text(size = axis_text_size))
    } else p1 <- p1
  }
  
  
  
  return(p1)
}

ab_to_full <- function(ab){
  
  fips <- fips(ab)
  finfo <- fips_info(fips)
  full_name <- finfo$full
  return(full_name)
  
}


#state_tests_chart_3(.northeast_region)

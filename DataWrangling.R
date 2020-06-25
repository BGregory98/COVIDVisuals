# Loading in all of our packages
library(tidyverse)
library(cowplot)
library(ggplot2)
library(dplyr)
library(data.table)
library(scales)
library(usmap)
library(viridis)
library(gganimate)
library(RColorBrewer)
library(caTools)
library(lubridate)
library(httr)
library(rgdal)
library(readr)
library(ggrepel)
library(sf)
library(ggmap)
library(shadowtext)
library(directlabels)
library(ggridges)

# Changing away from scientific notation
options(scipen = 999)

#options(device = 'windows')


#Creating multiplot function
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#### STATE DATA ####

#importing data
nytstateurl="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
ctptesturl="https://covidtracking.com/api/v1/states/daily.csv"
nytstateliveurl="https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-states.csv"

predata<-read.csv(url(nytstateurl))
#livepredata<-read.csv(url(nytstateliveurl))
popdata<-read.csv("C:/Users/benpg/Desktop/R_Covid/uspopdata2.csv")
testpredata <- read.csv(url(ctptesturl))

#Changing the name of pop in popdata
colnames(popdata)[2] <- 'pop'

#reformat dates for test data
testpredata$date <- parse_date_time(x = testpredata$date, 'ymd')

#Add live state data to bottom of historical state data
#livepredata <- livepredata %>%
#  select(date, state, fips, cases, deaths)

#predata <- rbind(predata, livepredata)

#join nyt cases data with pop data
data <- full_join(predata, popdata, by=c("state"))
data$state <- as.factor(data$state)
data <- data[,c(1,2,6,3,4,5)]


#remove state variable from test data
testpredata$state <- NULL

#reformatting dates again so they match
data$date <- parse_date_time(x = data$date, 'ymd')
testpredata$date <- parse_date_time(x = testpredata$date, 'ymd')

#join nyt and pop data with test data
data <- full_join(data, testpredata, by=c('fips', 'date'))

#for now I want to take out some of these variables, it's overwhelming
data <- data %>%
  select(date, state, pop, fips, cases, deaths, negative, hospitalizedCurrently, hospitalizedCumulative)

# taking out data where state is missing
data$state <- fct_explicit_na(data$state, na_level = "(Missing)")
data <- data %>%
  filter(state != '(Missing)')


# Changing NA negative tests into 0
data$negative[is.na(data$negative)] = 0
# Adding 'tests' variable which is cases (positive tests) plus negative tests
data$tests <- data$cases + data$negative
# Removing 'positive' variable
data$negative <- NULL
# Removing total test results variable
#data$totalTestResults <- NULL
# (Clumsily) renaming hospitalizedCumulative to hospitalized
#data$hospitalized <- data$hospitalizedCumulative
#data$hospitalizedCumulative <- NULL
# Changing NA recovereds to 0
#data$recovered[is.na(data$recovered)] = 0
# Changing NA hospitalizeds to 0
#data$hospitalized[is.na(data$hospitalized)] = 0

# NOTE: not sure I should be changing all these NAs to 0 considering that some states simply
# aren't reporting this data, so 0 is probably innaccurate.

# Taking out any rows where state is missing
data <- data %>%
  filter(state != '(Missing)')

state_data <- data

# Adding state abbreviations
state5 <- state_data %>%
  group_by(state) %>%
  summarize(dumb = length(cases))

state <- state5$state

state_abs <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'GU', 'HI',
               'ID', 'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN',
               'MS', 'MO', 'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'NI', 'OH',
               'OK', 'OR', 'PA', 'PR', 'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VI', 'VA', 'WA',
               'WV', 'WI', 'WY')

names_abs <- data.frame(state, state_abs)

levels(state_data$state)

state_data <- state_data %>%
  filter(state != '(Missing)')

state_data <- full_join(state_data, names_abs, by=c("state"))

#state_data$state <- NULL

#names(state_data)[names(state_data) == 'state_abs'] <- 'state'

# Get rid of territories for now
state_data <- state_data %>%
  filter(state_abs != 'VI') %>%
  filter(state_abs != 'NI') %>%
  filter(state_abs != 'PR') %>%
  filter(state_abs != 'GU')

# Load in states and regions
states_regions <- read.csv('C:/Users/benpg/Desktop/R_Covid/states_regions.csv')

# Add regions into state_data
state_data <- full_join(state_data, states_regions, by=c("state_abs"))

state_data <- state_data %>%
  select(date, state, state_abs, region, subregion, pop, fips, cases, deaths, tests, hospitalizedCurrently, hospitalizedCumulative)

# At this point I want to try to roughly create a 'hospitalized' variable which, for states that do
# current hospitalizations, will just be current hospitalizations, but for states which only do cumulative
# hospitalizations will be a rough guess of current hospitalizations based on an average hospital stay of 
# 12 days. 

# First, which states don't do current hospitalizations?
# Florida, Hawaii, Idaho, and Kansas

state_data1 <- state_data %>%
  filter(state %in% c('Florida', 'Hawaii', 'Idaho', 'Kansas')) %>%
  group_by(state)%>%
  mutate(daily_hospitalizations = hospitalizedCumulative - lag(hospitalizedCumulative,1))

state_data1$daily_hospitalizations[is.na(state_data1$daily_hospitalizations)] = 0
state_data1$daily_hospitalizations[state_data1$daily_hospitalizations < 0] <- 0

state_data1 <- state_data1 %>%
  group_by(state) %>%
  mutate(hospitalized = daily_hospitalizations + lag(daily_hospitalizations,1) + lag(daily_hospitalizations,2) + lag(daily_hospitalizations,3) + lag(daily_hospitalizations,4) + lag(daily_hospitalizations,5) + lag(daily_hospitalizations,6) + lag(daily_hospitalizations,7) + lag(daily_hospitalizations,8) + lag(daily_hospitalizations,9) + lag(daily_hospitalizations,10) + lag(daily_hospitalizations,11)) %>%
  select(date, state, state_abs, region, subregion, pop, fips, cases, deaths, tests, hospitalized)

state_data1$hospitalized <- as.integer(state_data1$hospitalized) 

state_data2 <- state_data %>%
  filter(state != 'Florida') %>%
  filter(state != 'Hawaii') %>%
  filter(state != 'Idaho') %>%
  filter(state != 'Kansas')

state_data2$hospitalized <- state_data2$hospitalizedCurrently

state_data2 <- state_data2 %>%
  select(date, state, state_abs, region, subregion, pop, fips, cases, deaths, tests, hospitalized)

state_data1 <- as.data.frame(state_data1)
state_data2 <- as.data.frame(state_data2)

state_data <- rbind2(state_data1, state_data2)

state_data

state_data <- state_data %>%
  arrange(date)

#Creating State Data
state_data <- state_data %>%
  group_by(state, state_abs, region, subregion, pop, fips) %>%
  # Active cases is cases minus cases with outcomes, however should also check who's actually tracking recoveries
  #mutate(active_cases = cases - (deaths + recovered)) %>%
  # Daily cases is today's cases minus yesterday's
  mutate(daily_cases = cases - lag(cases, default = 0)) %>%
  # Daily deaths is today's deaths minus yesterday's
  mutate(daily_deaths = deaths - lag(deaths, deafult = 0)) %>%
  # Daily tests is today's tests minus yesterday's (maybe look into how states are recording tests differently?)
  mutate(daily_tests = tests - lag(tests, default = 0))
# This is definitely not super accurate since not all states are doing cumulative hospitalizations
#mutate(daily_hospitalizations = hospitalized - lag(hospitalized, default = 0)) %>%
# And finally, daily recoveries 
#mutate(daily_recoveries = recovered - lag(recovered, default = 0))

# Smoothing out any negatives in daily counts
state_data$daily_tests[state_data$daily_tests < 0] <- 0
state_data$daily_cases[state_data$daily_cases < 0] <- 0
state_data$daily_deaths[state_data$daily_deaths < 0] <- 0
#state_data$daily_recoveries[state_data$daily_recoveries < 0] <- 0

# Adding new variables
state_data <- state_data %>%
  group_by(state, state_abs, region, subregion, pop, fips) %>%
  # Weekly averages of daily counts
  mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
  mutate(weekly_avg_daily_tests = frollmean(daily_tests, 7)) %>%
  #mutate(weekly_avg_daily_hospitalizations = frollmean(daily_hospitalizations, 7))%>%
  #mutate(weekly_avg_daily_recoveries = frollmean(daily_recoveries, 7)) %>%
  mutate(weekly_avg_hospitalized = frollmean(hospitalized, 7)) %>%
  # Growth rate and weekly average of growth rate
  mutate(growth_rate = daily_cases / lag(daily_cases, 1)) %>%
  mutate(weekly_avg_growth_rate = frollmean(growth_rate, 7)) %>%
  # Per capita numbers
  mutate(cases_10k = (cases/pop)*10000) %>%
  mutate(daily_cases_10k = (daily_cases/pop)*10000)%>%
  mutate(deaths_10k = (deaths/pop)*10000) %>%
  mutate(tests_10k = (tests/pop)*10000) %>%
  mutate(hospitalizations_10k = (hospitalized/pop)*10000) %>%
  #mutate(recoveries_10k = (recovered/pop)*10000) %>%
  #mutate(hospitalizedCurrently_10k = (hospitalizedCurrently/pop)*10000)%>%
  # Weekly averages of per capita numbers
  mutate(weekly_avg_daily_cases_10k = (weekly_avg_daily_cases/pop)*10000) %>%
  mutate(weekly_avg_daily_cases_mil = (weekly_avg_daily_cases/pop)*1000000) %>%
  mutate(weekly_avg_daily_deaths_10k = (weekly_avg_daily_deaths/pop)*10000) %>%
  mutate(weekly_avg_daily_tests_10k = (weekly_avg_daily_tests/pop)*10000) %>%
  #mutate(weekly_avg_daily_hospitalizations_10k = (weekly_avg_daily_hospitalizations/pop)*10000) %>%
  #mutate(weekly_avg_daily_recoveries_10k = (weekly_avg_daily_recoveries/pop)*10000) %>%
  # Test positive rates
  mutate(daily_test_positive_rate = daily_cases/daily_tests) %>%
  #mutate(hospitalization_rate = hospitalized/cases) %>%
  # Some additional variables
  mutate(weekly_new_cases = cases - lag(cases, n = 7, default = 0)) %>%
  mutate(weekly_new_tests = daily_tests +lag(daily_tests,1) +lag(daily_tests,2) +lag(daily_tests,3) +lag(daily_tests,4) +lag(daily_tests,5) +lag(daily_tests,6))%>%
  mutate(weekly_test_positive_rate = (weekly_new_cases/weekly_new_tests)*100) %>%
  mutate(one_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,7)) %>%
  mutate(two_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,14)) %>%
  mutate(case_mortality_rate = deaths/cases) %>%
  mutate(case_mortality_rate_lag = deaths/lag(cases,12))%>%
  #mutate(case_recovery_rate = recovered/deaths) %>%
  mutate(two_week_chg_weekly_pos_rate = (weekly_test_positive_rate/lag(weekly_test_positive_rate,14))-1) %>%
  mutate(two_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,14))-1) %>%
  mutate(one_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,7))-1)

#state_data_live <- state_data %>%
#  select(date, state, fips, cases, deaths, pop, daily_cases, daily_deaths, weekly_avg_daily_cases, weekly_avg_daily_deaths,
#         growth_rate, weekly_avg_growth_rate, cases_per_10k, deaths_per_10k, weekly_avg_daily_cases_10k,
#         weekly_avg_daily_deaths_per_10k, weekly_new_cases, case_mortality_rate, case_mortality_rate_lag, three_week_growth_rate,
#         two_week_chg_weekly_avg_daily_cases, one_week_chg_weekly_avg_daily_cases)

#max_date <- max(state_data$date)
#state_data <- state_data %>%
#  filter(date != max_date)

#Sate data summary
state_data_summary <- state_data %>%
  group_by(state, state_abs, fips, region, subregion, pop) %>%
  summarize(total_cases = tail(cases, 1),
            total_cases_10k = tail(cases_10k,1),
            #current_active_cases = tail(active_cases, 1),
            #max_active_cases = max(active_cases),
            total_deaths = tail(deaths, 1),
            total_tests = tail(tests, 1),
            current_hospitalized = tail(hospitalized, 1),
            #total_recoveries = tail(recovered, 1),
            cwa_growth_rate = tail(weekly_avg_growth_rate, 1),
            cwa_daily_cases = tail(weekly_avg_daily_cases, 1),
            cwa_daily_cases_10k = tail(weekly_avg_daily_cases_10k, 1),
            cwa_daily_tests = tail(weekly_avg_daily_tests, 1),
            cwa_daily_tests_10k = tail(tests_10k, 1),
            cwa_positive_rate = tail(weekly_test_positive_rate, 1),
            current_case_mortality_rate_lag = tail(case_mortality_rate_lag,1),
            current_two_week_chg_pos_rate = tail(two_week_chg_weekly_pos_rate,1),
            current_two_week_chg_weekly_avg_daily_cases = tail(two_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases = tail(one_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases_10k = tail(one_week_chg_weekly_avg_daily_cases_10k,1),
            current_two_week_chg_weekly_avg_daily_cases_10k = tail(two_week_chg_weekly_avg_daily_cases_10k,1))

# Lots more variables
state_data_summary <- state_data_summary %>%
  #mutate(current_active_cases_10k = (current_active_cases / pop)*10000)%>%
  mutate(total_deaths_10k = (total_deaths / pop)*10000)%>%
  mutate(total_tests_10k = (total_tests / pop)*10000)%>%
  mutate(current_hospitalized_10k = (current_hospitalized / pop)*10000)%>%
  #mutate(total_recoveries_10k = (total_recoveries / pop)*10000)%>%
  mutate(case_mort_rate = (total_deaths/total_cases)*100)%>%
  mutate(test_positive_rate = (total_cases/total_tests)*100)
#mutate(case_recovery_rate = (total_recoveries/total_cases)*100) %>%
#mutate(pct_chg_from_max_active_cases = (((current_active_cases/max_active_cases)-1)*100))

# Reformatting date again for some reason
state_data$date <- as.Date(state_data$date, '%y-%m-%d')

# Creating all of the class variables for mapping
state_data_summary$current_two_week_chg_pos_rate_class <- cut(state_data_summary$current_two_week_chg_pos_rate,
                                                              breaks = c(-Inf,-0.05,0.05,Inf),
                                                              labels = c('Decreasing','Flat','Increasing'))

state_data_summary$current_two_week_chg_weekly_avg_daily_cases_class <- cut(state_data_summary$current_two_week_chg_weekly_avg_daily_cases,
                                                                            breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                                            labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                                       '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                                       '+75 to +100%', '> +100%'))
state_data_summary$current_one_week_chg_weekly_avg_daily_cases_class <- cut(state_data_summary$current_one_week_chg_weekly_avg_daily_cases,
                                                                            breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                                            labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                                       '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                                       '+75 to +100%', '> +100%'))

state_data_summary$cwa_positive_rate_class <- cut(state_data_summary$cwa_positive_rate,
                                                  breaks = c(-Inf,0.025,0.05,0.075,0.10,0.125,0.150,Inf),
                                                  labels = c('< 2.5%', '2.5% to 5.0%', '5.0% to 7.5%', '7.5% to 10.0%', '10.0% to 12.5%', '12.5% to 15.0%', '> 15.0%'))


state_data_summary$total_tests_10k_class <- cut(state_data_summary$total_tests_10k,
                                                breaks = c(-Inf,250,375,500,625,750,875,1000,Inf),
                                                labels = c('< 250', '250 to 375',
                                                           '375 to 500', '500 to 625',
                                                           '625 to 750', '750 to 875', '875 to 1,000',
                                                           '> 1,000'))


state_data_summary_temp <- state_data_summary %>%
  select(state_abs, current_two_week_chg_pos_rate_class)

state_data <- full_join(state_data, state_data_summary_temp, by = c('state', 'fips', 'region', 'subregion',
                                                                    'state_abs'))

max_state_data <- state_data %>%
  select(date, state, state_abs, fips, cases, hospitalized,
         weekly_avg_daily_cases, weekly_avg_daily_cases_10k) %>%
  filter(cases > 0)%>%
  group_by(state, state_abs, fips) %>%
  summarize(# Weekly Average of Daily Cases
    max_weekly_avg_daily_cases = max(weekly_avg_daily_cases, na.rm = T),
    max_weekly_avg_daily_cases_10k = max(weekly_avg_daily_cases_10k, na.rm = T),
    max_hospitalized = max(hospitalized, na.rm = T),
    max_date_index = match(max(weekly_avg_daily_cases, na.rm = T),weekly_avg_daily_cases),
    max_date_hosp_index = match(max(hospitalized, na.rm = T),hospitalized),
    current_weekly_avg_daily_cases_10k = tail(weekly_avg_daily_cases_10k,1),
    # Start Date
    start_date = head(date, 1)) %>%
  # Peak Date Weekly Average of Daily Cases
  mutate(peak_date = start_date + max_date_index - 1) %>%
  mutate(peak_hosp_date = start_date + max_date_hosp_index - 1) %>%
  mutate(how_long_ago = as.integer(today()-peak_date)) %>%
  select(state, state_abs, fips, max_weekly_avg_daily_cases, max_weekly_avg_daily_cases_10k, max_hospitalized,
         current_weekly_avg_daily_cases_10k, 
         peak_date, peak_hosp_date, how_long_ago)

state_data <- full_join(state_data, max_state_data, by = c('state', 'state_abs', 'fips'))

state_data <- state_data %>%
  mutate(pct_max_weekly_avg_daily_cases = weekly_avg_daily_cases/max_weekly_avg_daily_cases) %>%
  mutate(pct_max_hospitalized = hospitalized/max_hospitalized)

state_data_summary <- full_join(max_state_data, state_data_summary, by = c('state', 'state_abs', 'fips'))

state_data_summary <- state_data_summary %>%
  mutate(pct_max_weekly_avg_daily_cases = cwa_daily_cases/max_weekly_avg_daily_cases)

#qplot(data = state_data_summary, x = total_tests_10k)

# Making State Center Data
prestatecenters <- read.csv("C:/Users/benpg/Desktop/R_Covid/state_centers.csv")
prestatecenters$lat <- sub("(.*) (.*)", "\\1", prestatecenters$coords)
prestatecenters$lon <- sub("(.*) (.*)", "\\2", prestatecenters$coords)
prestatecenters$coords <- NULL
prestatecenters$lon <- gsub("[^0-9.-]", "", prestatecenters$lon)
prestatecenters$lat <- gsub("[^0-9.-]", "", prestatecenters$lat)
prestatecenters$lon <- sub("(.*)", "-\\1", prestatecenters$lon)

prestatecenters$lon <- as.numeric(prestatecenters$lon)
prestatecenters$lat <- as.numeric(prestatecenters$lat)

state_centers <- prestatecenters

state_centers <- state_centers %>%
  select(lon, lat, state_abs)

state_centers <- usmap_transform(state_centers)
state_centers <- full_join(state_centers, states_abs, by = 'state_abs')
states_area <- read.csv("C:/Users/benpg/Desktop/R_Covid/states_area.csv")
state_centers <- full_join(state_centers, states_area, by = 'state_abs')

#### COUNTY DATA ####

#importing county data
nytcountyurl = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
nytcountyliveurl = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us-counties.csv"
predata<-read.csv(url(nytcountyurl))
#livepredata<-read.csv(url(nytcountyliveurl))
popdata<-read.csv("C:/Users/benpg/Desktop/R_Covid/co-est2019-alldata.csv")
csadata <- read.csv("C:/Users/benpg/Desktop/R_Covid/list1_2020.csv")
addcountydata <- read.csv("C:/Users/benpg/Desktop/R_Covid/additional_county_data.csv")
ruralurbancodes <- read.csv("C:/Users/benpg/Desktop/R_Covid/ruralurbancodes2013.csv")

#Add live county data to bottom of historical county data
#livepredata <- livepredata %>%
#  select(date, state, county, fips, cases, deaths)

#predata <- rbind(predata, livepredata)


# I'm not exactly sure what all this is or why I did it but I'm afraid to get rid of it so I'm going to just leave it
NAs <- which(is.na(predata), arr.ind = TRUE)
NAs <- as.data.frame(NAs)

NAs %>%
  filter(col != 4)

rows_with_NAs <- NAs$row

popless_places <- predata[rows_with_NAs,]

popless_places <- popless_places %>%
  filter(county == 'Unknown') %>%
  group_by(state) %>%
  summarize(cases = tail(cases,1))

#Okay so maybe I should just get rid of the unknowns?
#And then the only other NAs are New York City and Kansas City, Missouri
#The pop of NYC proper is 8,398,748
#The pop of Kansas City, Missouri is 491918

popdata <- popdata %>%
  filter(county_num != 0)

popdata <- popdata %>%
  filter(sum_lev == 50)

popdata1 <- popdata %>%
  filter(county_num < 10)
popdata2 <- popdata %>%
  filter(county_num >= 10 & county_num < 100)
popdata3 <- popdata %>%
  filter(county_num > 100)

popdata1$county_num <- paste0("00", popdata1$county_num)
popdata2$county_num <- paste0('0', popdata2$county_num)

popdatafixed <- rbind(popdata1, popdata2, popdata3)

popdatafixed$fips <- paste0(popdatafixed$state_fips, popdatafixed$county_num)

popdatafixed$sum_lev <- NULL
popdatafixed$region <- NULL
popdatafixed$division <- NULL
popdatafixed$state_fips <- NULL
popdatafixed$county_num <- NULL
popdatafixed$state <- NULL
popdatafixed$county <- NULL

str(popdatafixed)
str(predata)

predata$fips <- as.character(predata$fips)

csadata$fips <- as.character(csadata$fips)

data <- full_join(predata, popdatafixed, by=c("fips"))

datanyc <- data %>%
  filter(county == 'New York City')

datakc <- data %>%
  filter(county == 'Kansas City')

datacut <- na.omit(data)

#NYC Fix
databronx <- datanyc
datany <- datanyc
dataqueens <- datanyc
datakings <- datanyc
datarichmond <- datanyc

databronx$county <- 'Bronx'
databronx$fips <- 36005
databronx$pop <- 1432132
databronx$cases <- round(databronx$cases*.22037,0)
databronx$deaths <- round(databronx$deaths*.19634,0)

datany$county <- 'New York County'
datany$fips <- 36061
datany$pop <- 1628701
datany$cases <- round(datany$cases*.13074,0)
datany$deaths <- round(datany$deaths*.19730,0)

dataqueens$county <- 'Queens'
dataqueens$fips <- 36081
dataqueens$pop <- 2278906
dataqueens$cases <- round(dataqueens$cases*.30856,0)
dataqueens$deaths <- round(dataqueens$deaths*.26298,0)

datakings$county <- 'Kings'
datakings$fips <- 36047
datakings$pop <- 2582830
datakings$cases <- round(datakings$cases*.30856,0)
datakings$deaths <- round(datakings$deaths*.29775,0)

datarichmond$county <- 'Richmond'
datarichmond$fips <- 36085
datarichmond$pop <- 476179
datarichmond$cases <- round(datarichmond$cases*.07175,0)
datarichmond$deaths <- round(datarichmond$deaths*.04562,0)


datakc$pop <- 491918

data <- rbind(datacut, databronx, datany, dataqueens, datakings, datarichmond, datakc)

#reformatting dates
data$date<-as.Date((data$date), format="%Y-%m-%d")

data

data <- full_join(data, csadata, by=c("fips"))

addcountydata
addcountydata$lat <- gsub("[^0-9.-]", "", addcountydata$lat)
addcountydata$lon <- gsub("[^0-9.-]", "", addcountydata$lon)
addcountydata <- addcountydata %>%
  select(fips, lat, lon)

addcountydata$lon <- sub("(.*)", "-\\1", addcountydata$lon)

addcountydata$fips <- as.character(addcountydata$fips)

addcountydata$lat <- as.numeric(addcountydata$lat)
addcountydata$lon <- as.numeric(addcountydata$lon)

ruralurbancodes$metro_nonmetro <- sub("(.*) - .*", "\\1", ruralurbancodes$metro_nonmetro)

ruralurbancodes$fips <- as.character(ruralurbancodes$fips)

data <- full_join(data, addcountydata, by=c("fips"))
data <- full_join(data, ruralurbancodes, by=c("fips"))

# Metro Area Data
cbsa_data <- data %>%
  group_by(date, cbsa) %>%
  summarize(cases = sum(cases),
            deaths = sum(deaths))

cbsa_data <- cbsa_data[complete.cases(cbsa_data[ , 1:2]), ]

# Adding population data to Metro Area data
csapopdata <- full_join(csadata, popdatafixed, by='fips')
csapopdata <- na.omit(csapopdata)
csapopdata <- csapopdata %>%
  group_by(cbsa) %>%
  summarize(pop = sum(pop))

cbsa_data <- full_join(cbsa_data, csapopdata, by = 'cbsa')

# Adding a state_abs variable for metro areas based on the 'primary' state that the metro area is located in
cbsa_data$state_abs <- sub(".*, (.{2}).*", "\\1", cbsa_data$cbsa)
cbsa_data$cbsa_short <- sub("([a-z]*)[-,/].*", "\\1", cbsa_data$cbsa)
cbsa_data$cbsa_short <- paste0(cbsa_data$cbsa_short, ', ', cbsa_data$state_abs)
cbsa_data <- full_join(cbsa_data, states_regions, by = 'state_abs')
cbsa_data <- full_join(cbsa_data, states_abs, by = 'state_abs')

# Creating Metro Area data
cbsa_data <- cbsa_data %>%
  group_by(cbsa, cbsa_short, state, state_abs, region, subregion, pop) %>%
  mutate(daily_cases = cases - lag(cases, default = 0)) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = 0))

# Smoothing out any negatives in daily changes
cbsa_data$daily_cases[cbsa_data$daily_cases < 0] <- 0
cbsa_data$daily_deaths[cbsa_data$daily_deaths < 0] <- 0

cbsa_data <- cbsa_data %>%
  group_by(cbsa, cbsa_short, state, state_abs, region, subregion, pop) %>%
  mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
  # Growth rate and weekly average of growth rate
  mutate(growth_rate = daily_cases / lag(daily_cases, 1)) %>%
  mutate(weekly_avg_growth_rate = frollmean(growth_rate, 7)) %>%
  # Per capita numbers
  mutate(cases_10k = (cases/pop)*10000) %>%
  mutate(daily_cases_10k = (daily_cases/pop)*10000)%>%
  mutate(deaths_10k = (deaths/pop)*10000) %>%
  # Weekly averages of per capita numbers
  mutate(weekly_avg_daily_cases_10k = (weekly_avg_daily_cases/pop)*10000) %>%
  mutate(weekly_avg_daily_cases_mil = (weekly_avg_daily_cases/pop)*1000000) %>%
  mutate(weekly_avg_daily_deaths_10k = (weekly_avg_daily_deaths/pop)*10000) %>%
  # Some additional variables
  mutate(one_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,7)) %>%
  mutate(two_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,14)) %>%
  mutate(case_mortality_rate = deaths/cases) %>%
  mutate(case_mortality_rate_lag = deaths/lag(cases,12))%>%
  mutate(two_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,14))-1) %>%
  mutate(one_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,7))-1)

# Metro Area Data Summary
cbsa_data_summary <- cbsa_data %>%
  group_by(cbsa, cbsa_short, state, state_abs, region, subregion, pop) %>%
  summarize(total_cases = tail(cases, 1),
            total_cases_10k = tail(cases_10k,1),
            total_deaths = tail(deaths, 1),
            cwa_growth_rate = tail(weekly_avg_growth_rate, 1),
            cwa_daily_cases = tail(weekly_avg_daily_cases, 1),
            cwa_daily_cases_10k = tail(weekly_avg_daily_cases_10k, 1),
            current_case_mortality_rate_lag = tail(case_mortality_rate_lag,1),
            current_two_week_chg_weekly_avg_daily_cases = tail(two_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases = tail(one_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases_10k = tail(one_week_chg_weekly_avg_daily_cases_10k,1),
            current_two_week_chg_weekly_avg_daily_cases_10k = tail(two_week_chg_weekly_avg_daily_cases_10k,1))


# Making sure there are no infinite values
cbsa_data_summary$current_two_week_chg_weekly_avg_daily_cases[is.infinite(cbsa_data_summary$current_two_week_chg_weekly_avg_daily_cases)] <- NA
cbsa_data_summary$current_one_week_chg_weekly_avg_daily_cases[is.infinite(cbsa_data_summary$current_one_week_chg_weekly_avg_daily_cases)] <- NA

# "Max" metro area data to figure out peaks and the timing of peaks
max_cbsa_data <- cbsa_data %>%
  select(date, cbsa, cbsa_short, cases, weekly_avg_daily_cases) %>%
  filter(cases > 0)%>%
  group_by(cbsa, cbsa_short) %>%
  summarize(max_weekly_avg_daily_cases = max(weekly_avg_daily_cases, na.rm = T),
            max_date_index = match(max(weekly_avg_daily_cases, na.rm = T),weekly_avg_daily_cases),
            start_date = head(date, 1)) %>%
  mutate(peak_date = start_date + max_date_index - 1) %>%
  mutate(how_long_ago = as.integer(today()-peak_date)) %>%
  select(cbsa, cbsa_short, max_weekly_avg_daily_cases, peak_date, how_long_ago)

cbsa_data <- full_join(cbsa_data, max_cbsa_data, by = c('cbsa', 'cbsa_short'))

cbsa_data <- cbsa_data %>%
  mutate(pct_max_weekly_avg_daily_cases = weekly_avg_daily_cases/max_weekly_avg_daily_cases)

cbsa_data_summary <- full_join(cbsa_data_summary, max_cbsa_data, by = c('cbsa', 'cbsa_short'))

cbsa_data_summary <- cbsa_data_summary %>%
  arrange(desc(peak_date), desc(cwa_daily_cases_10k))

cbsa_order <- as.vector((cbsa_data_summary$cbsa_short))

#Creating County Data
data <- full_join(data, states_abs, by = 'state')
data <- full_join(data, states_regions, by = 'state_abs')

data <- data[,c(1,4,9,10,2,3,13,14,15,8,11,12,7,5,6)]

# Adding variables to county data
county_data <- data %>%
  group_by(fips, lat, lon, county, state, state_abs, region, subregion, cbsa, rucc_2013, metro_nonmetro, pop) %>%
  mutate(daily_cases = cases - lag(cases, default = 0)) %>%
  mutate(daily_deaths = deaths - lag(deaths, default = 0))

county_data$daily_cases[county_data$daily_cases < 0] <- 0
county_data$daily_deaths[county_data$daily_deaths < 0] <- 0

county_data <- county_data %>%
  group_by(fips, lat, lon, county, state, state_abs, region, subregion, cbsa, rucc_2013, metro_nonmetro, pop) %>%
  mutate(weekly_avg_daily_cases = frollmean(daily_cases, 7)) %>%
  mutate(weekly_avg_daily_deaths = frollmean(daily_deaths, 7)) %>%
  # Growth rate and weekly average of growth rate
  mutate(growth_rate = daily_cases / lag(daily_cases, 1)) %>%
  mutate(weekly_avg_growth_rate = frollmean(growth_rate, 7)) %>%
  # Per capita numbers
  mutate(cases_10k = (cases/pop)*10000) %>%
  mutate(daily_cases_10k = (daily_cases/pop)*10000)%>%
  mutate(deaths_10k = (deaths/pop)*10000) %>%
  # Weekly averages of per capita numbers
  mutate(weekly_avg_daily_cases_10k = (weekly_avg_daily_cases/pop)*10000) %>%
  mutate(weekly_avg_daily_cases_mil = (weekly_avg_daily_cases/pop)*1000000) %>%
  mutate(weekly_avg_daily_deaths_10k = (weekly_avg_daily_deaths/pop)*10000) %>%
  # Some additional variables
  mutate(one_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,7)) %>%
  mutate(two_week_chg_weekly_avg_daily_cases_10k = weekly_avg_daily_cases_10k - lag(weekly_avg_daily_cases_10k,14)) %>%
  mutate(case_mortality_rate = deaths/cases) %>%
  mutate(case_mortality_rate_lag = deaths/lag(cases,12))%>%
  mutate(two_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,14))-1) %>%
  mutate(one_week_chg_weekly_avg_daily_cases = (weekly_avg_daily_cases/lag(weekly_avg_daily_cases,7))-1)


county_data$two_week_chg_weekly_avg_daily_cases_class <- cut(county_data$two_week_chg_weekly_avg_daily_cases,
                                                             breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                             labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                        '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                        '+75 to +100%', '> +100%'))

county_data$one_week_chg_weekly_avg_daily_cases_class <- cut(county_data$one_week_chg_weekly_avg_daily_cases,
                                                             breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                             labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                        '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                        '+75 to +100%', '> +100%'))

county_data$total_cases_exp_class <- cut(county_data$cases, breaks = c(0,10,100,1000,10000,Inf),
                                         labels = c('< 10', '10 - 100', '100 - 1,000', '1,000 - 10,000', '> 10,000'))

county_data$cwa_daily_cases_mil_class <- cut(county_data$weekly_avg_daily_cases_mil, breaks = c(-Inf,50,100,150,200,250,300,Inf),
                                             labels = c('< 50', '50 - 100', '100 - 150', '150 - 200', '200 - 250', '250 - 300', '> 300'))

#qplot(data = county_data, x = two_week_chg_daily_cases_mil, xlim = c(-200,200))

#county_data$weekly_chg_wadcp[is.na(county_data$weekly_chg_wadcp)] <- 0
#county_data$weekly_chg_wadcp[county_data$weekly_chg_wadcp > 75] <- 75

#county_data$weekly_avg_daily_cases_10k_for_animation <- county_data$weekly_avg_daily_cases_10k

#county_data$weekly_avg_daily_cases_10k_for_animation[county_data$weekly_avg_daily_cases_10k_for_animation > 25] <- 25
#county_data$weekly_avg_daily_cases_10k_for_animation[county_data$weekly_avg_daily_cases_10k_for_animation < 0] <- 0

#county_data$one_week_chg_weekly_avg_daily_cases_animation <- county_data$one_week_chg_weekly_avg_daily_cases

#county_data$one_week_chg_weekly_avg_daily_cases_animation[county_data$one_week_chg_weekly_avg_daily_cases_animation > 1] <- 1
#county_data$one_week_chg_weekly_avg_daily_cases_animation[county_data$one_week_chg_weekly_avg_daily_cases_animation < -1] <- -1

#county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases_animation <- county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases

#county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases_animation[county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases_animation > .5] <- .5
#county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases_animation[county_data$weekly_avg_one_week_chg_weekly_avg_daily_cases_animation < -.5] <- -.5

#county_data$weekly_avg_daily_cases[county_data$weekly_avg_daily_cases > 200] <- 200


#county_data <- county_data %>%
#  filter(fips != 5079) %>%
#  filter(fips != 47007)

#county_data <- county_data[complete.cases(county_data[ , 1:3]),]
#county_data <- county_data[complete.cases(county_data[ , 9:10]),]

county_data_summary <- county_data %>%
  group_by(fips, lat, lon, county, state, state_abs, region, subregion, cbsa, rucc_2013, metro_nonmetro, pop) %>%
  summarize(total_cases = tail(cases, 1),
            total_cases_10k = tail(cases_10k,1),
            total_deaths = tail(deaths, 1),
            total_deaths_10k = tail(deaths_10k,1),
            cwa_growth_rate = tail(weekly_avg_growth_rate, 1),
            cwa_daily_cases = tail(weekly_avg_daily_cases, 1),
            cwa_daily_cases_10k = tail(weekly_avg_daily_cases_10k, 1),
            cwa_daily_cases_mil = tail(weekly_avg_daily_cases_mil, 1),
            current_case_mortality_rate_lag = tail(case_mortality_rate_lag,1),
            current_two_week_chg_weekly_avg_daily_cases = tail(two_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases = tail(one_week_chg_weekly_avg_daily_cases,1),
            current_one_week_chg_weekly_avg_daily_cases_10k = tail(one_week_chg_weekly_avg_daily_cases_10k,1),
            current_two_week_chg_weekly_avg_daily_cases_10k = tail(two_week_chg_weekly_avg_daily_cases_10k,1))

county_data$rucc_2013 <- as.factor(county_data$rucc_2013)


county_data_summary$current_two_week_chg_weekly_avg_daily_cases_class <- cut(county_data_summary$current_two_week_chg_weekly_avg_daily_cases,
                                                                             breaks = c(-Inf,-1,-0.50,-.10,.10,0.50,1,Inf),
                                                                             labels = c('< -100%', '-100 to -50%', '-50 to -10%', '-10 to +10%', '+10 to +50%', '+50 to +100%', '> +100%'))

county_data_summary$current_one_week_chg_weekly_avg_daily_cases_class <- cut(county_data_summary$current_one_week_chg_weekly_avg_daily_cases,
                                                                             breaks = c(-Inf,-1,-0.75,-0.50,-0.25,0,0.25,0.50,0.75,1,Inf),
                                                                             labels = c('< -100%', '-100 to -75%', '-75 to -50%', '-50 to -25%',
                                                                                        '-25 to 0%', '0 to +25%', '+25 to +50%', '+50 to +75%',
                                                                                        '+75 to +100%', '> +100%'))

county_data_summary$total_cases_10k_class <- cut(county_data_summary$total_cases_10k, breaks  = c(-Inf,20,40,60,80,100,120,150,Inf),
                                                 labels = c('< 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100', '100 - 120', '120 - 150', '> 150'))

county_data_summary$total_cases_10k_class_2 <- cut(county_data_summary$total_cases_10k, breaks  = c(-Inf,20,40,60,80,100,120,Inf),
                                                   labels = c('< 20', '20 - 40', '40 - 60', '60 - 80', '80 - 100', '100 - 120', '> 120'))


county_data_summary$total_cases_exp_class <- cut(county_data_summary$total_cases, breaks = c(0,10,100,1000,10000,Inf),
                                                 labels = c('< 10', '10 - 100', '100 - 1,000', '1,000 - 10,000', '> 10,000'))

#county_data_summary$total_cases_mil_exp_class <- cut(county_data_summary$total_cases_mil, breaks = c(0,10,100,1000,10000,Inf),
#                                                 labels = c('< 10', '10 - 100', '100 - 1,000', '1,000 - 10,000', '> 10,000'))

county_data_summary$cwa_growth_rate_class <- cut(county_data_summary$cwa_growth_rate, breaks = c(-5,0.5,1,1.5,2,2.5,3,3.5,4,Inf),
                                                 labels = c('< 0.5','0.5 - 1.0', '1.0 - 1.5', '1.5 - 2.0', '2.0 - 2.5', '2.5 - 3.0', '3.0 - 3.5', '3.5 - 4.0', '> 4.0'))

county_data_summary$cwa_daily_cases_mil_class <- cut(county_data_summary$cwa_daily_cases_mil, breaks = c(-Inf,50,100,150,200,250,300,Inf),
                                                     labels = c('< 50', '50 - 100', '100 - 150', '150 - 200', '200 - 250', '250 - 300', '> 300'))

county_data_summary$current_two_week_chg_weekly_avg_daily_cases_10k_class <- cut(county_data_summary$current_two_week_chg_weekly_avg_daily_cases_10k,
                                                                                 breaks = c(-Inf,-1,-0.75,-0.5,-0.25,0.25,0.5,0.75,1,Inf),
                                                                                 labels = c('< -1', '-1 to -0.75', '-0.75 to -0.5', '-0.5 to -0.25', '-0.25 to 0.25', '0.25 to 0.5',
                                                                                            '0.5 to 0.75', '0.75 to 1', '> 1'))

county_data_summary$total_deaths_10k_class <- cut(county_data_summary$total_deaths_10k,
                                                  breaks = c(-Inf,2,4,6,8,10,12,15,Inf),
                                                  labels = c('< 2', '2 - 4', '4 - 6', '6 - 8', '8 - 10', '10 - 12', '12 - 15', '> 15'))

#county_data_summary$total_deaths_mil_class <- cut(county_data_summary$total_deaths_mil,
#                                                  breaks = c(-Inf,1,10,100,1000,Inf),
#                                                  labels = c('< 1', '1 - 10', '10 - 100', '100 - 1,000', '> 1,000'))

#county_data_summary$current_two_week_chg_daily_cases_mil_class <- cut(county_data_summary$current_two_week_chg_daily_cases_mil,
#                                                                      breaks = c(-Inf, -200, -150, -100, -50, 50, 100, 150, 200, Inf),
#                                                                      labels = c('< -200', '-200 to -150', '-150 to -100', '-100 to -50',
#                                                                                 '-50 to 50', '50 to 100', '100 to 150',
#                                                                                 '150 to 200', '> 200'))

#county_data_summary$total_cases_mil_class <- cut(county_data_summary$total_cases_mil,
#                                                 breaks = c(-Inf,2000,4000,6000,8000,10000,12000,Inf),
#                                                 labels = c('< 2,000', '2,000 - 4,000', '4,000 - 6,000',
#                                                            '6,000 - 8,000', '8,000 - 10,000', '10,000 - 12,000', '> 12,000'))


#county_data_summary$current_four_week_chg_daily_cases_mil_class <- cut(county_data_summary$current_four_week_chg_daily_cases_mil,
#                                                                       breaks = c(-Inf,-100,-75,-50,-25,25,50,75,100,Inf),
#                                                                       labels = c('< -100', '-100 to -75',
#                                                                                  '-75 to -50', '-50 to -25', '-25 to +25',
#                                                                                  '+25 to +50', '+50 to +75', '+75 to +100',
#                                                                                  '> +100'))

max_county_data <- county_data %>%
  select(date, state, county, fips, cases, weekly_avg_daily_cases) %>%
  filter(cases > 0)%>%
  group_by(state, county, fips) %>%
  summarize(max_weekly_avg_daily_cases = max(weekly_avg_daily_cases, na.rm = T),
            max_date_index = match(max(weekly_avg_daily_cases, na.rm = T),weekly_avg_daily_cases),
            start_date = head(date, 1)) %>%
  mutate(peak_date = start_date + max_date_index) %>%
  mutate(how_long_ago = as.integer(today()-peak_date)) %>%
  select(state, county, fips, max_weekly_avg_daily_cases, peak_date, how_long_ago)

max_county_data <- max_county_data %>%
  arrange(desc(peak_date))

county_order <- as.vector((max_county_data$fips))

county_data <- full_join(county_data, max_county_data, by = c('state', 'county', 'fips'))

county_data <- county_data %>%
  mutate(pct_max_weekly_avg_daily_cases = weekly_avg_daily_cases/max_weekly_avg_daily_cases)

max_county_data$how_long_ago_class <- cut(max_county_data$how_long_ago,
                                          breaks = c(-1,5,10,15,20,25,30,Inf),
                                          labels = c('0 - 5 Days Ago','5 - 10 Days Ago',
                                                     '10 - 15 Days Ago','15 - 20 Days Ago',
                                                     '20 - 25 Days Ago','25 - 30 Days Ago',
                                                     'More than 30 Days Ago'))

max_county_data$how_long_ago_class2 <- cut(max_county_data$how_long_ago,
                                           breaks = c(-1,0,1,2,3,7,Inf),
                                           labels = c('Today','Yesterday','Two Days Ago', 'Three Days Ago', 'This Week', 'More than a Week Ago'))

county_data_summary <- full_join(county_data_summary, max_county_data, by = c('state', 'county', 'fips'))

#county_data_summary <- full_join(county_data_summary, county_politics_data, by = 'fips')
county_data <- full_join(county_data, county_politics_data, by = 'fips')


#str(county_politics_data)

#qplot(data = county_data_summary, x = total_cases_mil)

#state_data <- state_data %>%
#  mutate(state_full_name = ab_to_full(state))

#names(tempdata)[names(tempdata) == 'state_abs'] <- 'state'

#state_data_summary <- full_join(state_data_summary, tempdata, by = 'state')
#state_data <- full_join(state_data, tempdata, by = 'state')

#state_data$hospitalizedCurrently[is.na(state_data$hospitalizedCurrently)] = 0

#### Trying to do the weighted center of infections ####
wtcent_county_data <- county_data %>%
  select(date, county, state, fips, cases, lat, lon, daily_cases, weekly_avg_daily_cases) %>%
  mutate(lat_wadc = weekly_avg_daily_cases * lat)%>%
  mutate(lon_wadc = weekly_avg_daily_cases * lon)

wtcent_county_data <- na.omit(wtcent_county_data)

wtcent_county_data <- wtcent_county_data %>%
  group_by(date)%>%
  summarize(lat = round(sum(lat_wadc)/sum(weekly_avg_daily_cases),5),
            lon = round(sum(lon_wadc)/sum(weekly_avg_daily_cases),5))

wtcent_county_data <- wtcent_county_data %>%
  filter(date >= '2020-04-01') %>%
  select(lon, lat, date)

wtcent_county_data <- usmap_transform(wtcent_county_data)

class(wtcent_county_data$date)

wtcent_county_data$date <- format(wtcent_county_data$date, "%b %d")
wtcent_county_data$date <- as.factor(wtcent_county_data$date)

date_order <- as.vector(wtcent_county_data$date)

wtcent_county_data$date<-factor(wtcent_county_data$date, levels=(date_order))

lon <- -92.473743
lat <- 37.373548
date <- 'Mean Center of\nUS Population'
pop_center <- data.frame(lon, lat, date)
pop_center <- usmap_transform(pop_center)

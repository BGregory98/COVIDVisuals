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

m1 +
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

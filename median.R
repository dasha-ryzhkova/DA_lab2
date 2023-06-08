library(DescTools)
install.packages('DescTools')


#lead_time
MedianCI(data$lead_time,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 2500)


#stays_in_weekend_nights
MedianCI(data$stays_in_weekend_nights,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#stays_in_week_nights
MedianCI(data$stays_in_week_nights,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#adults
MedianCI(data$adults,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#children
MedianCI(data$children,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#babies
MedianCI(data$babies,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#all_children
MedianCI(data$all_children,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#previous_cancellations
MedianCI(data$previous_cancellations,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#previous_bookings_not_canceled
MedianCI(data$previous_bookings_not_canceled,
         conf.level = 0.95,
         method = "boot",
         R = 1000)


#booking_changes
MedianCI(data$booking_changes,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)

#days_in_waiting_list
MedianCI(data$days_in_waiting_list,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)


#adr
MedianCI(data$adr,
         conf.level = 0.95,
         sides = 'left',
         na.rm = FALSE,
         method = "boot",
         R = 10000)


#required_car_parking_spaces
MedianCI(data$required_car_parking_spaces,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)

#total_of_special_requests
MedianCI(data$total_of_special_requests,
         conf.level = 0.95,
         na.rm = FALSE,
         method = "boot",
         R = 1000)
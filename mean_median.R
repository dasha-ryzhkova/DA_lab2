library(dplyr)
library(ggplot2)

# area
# arrival_date_month
# lead_time_case

# mean
#lead_time
mean_lead_time <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(lead_time ),
            sd = sd(lead_time ),
            n = n(),
            a = mean(lead_time ) + qnorm(0.025) * sd(lead_time ) / sqrt(n()),
            b = mean(lead_time ) + qnorm(0.975) * sd(lead_time ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_lead_time



# model <- lm(lead_time ~ 1, data = data)
# confint(model, level = 0.95)


#stays_in_weekend_nights
mean_stays_in_weekend_nights <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(stays_in_weekend_nights ),
            sd = sd(stays_in_weekend_nights ),
            n = n(),
            a = mean(stays_in_weekend_nights ) + qnorm(0.025) * sd(stays_in_weekend_nights ) / sqrt(n()),
            b = mean(stays_in_weekend_nights ) + qnorm(0.975) * sd(stays_in_weekend_nights ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_stays_in_weekend_nights


#stays_in_week_nights
mean_stays_in_week_nights <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(stays_in_week_nights ),
            sd = sd(stays_in_week_nights ),
            n = n(),
            a = mean(stays_in_week_nights ) + qnorm(0.025) * sd(stays_in_week_nights ) / sqrt(n()),
            b = mean(stays_in_week_nights ) + qnorm(0.975) * sd(stays_in_week_nights ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_stays_in_week_nights


#lead_time
mean_adults <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(adults ),
            sd = sd(adults ),
            n = n(),
            a = mean(adults ) + qnorm(0.025) * sd(adults ) / sqrt(n()),
            b = mean(adults ) + qnorm(0.975) * sd(adults ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_adults


#children
mean_children<- data %>% 
  group_by(area) %>%
  summarise(mean = mean(children ),
            sd = sd(children ),
            n = n(),
            a = mean(children ) + qnorm(0.025) * sd(children ) / sqrt(n()),
            b = mean(children ) + qnorm(0.975) * sd(children ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_children


#babies
mean_babies <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(babies ),
            sd = sd(babies ),
            n = n(),
            a = mean(babies ) + qnorm(0.025) * sd(babies ) / sqrt(n()),
            b = mean(babies ) + qnorm(0.975) * sd(babies ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_babies


#all_children
mean_all_children <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(all_children ),
            sd = sd(all_children ),
            n = n(),
            a = mean(all_children ) + qnorm(0.025) * sd(all_children ) / sqrt(n()),
            b = mean(all_children ) + qnorm(0.975) * sd(all_children ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_all_children


#previous_cancellations
mean_previous_cancellations <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(previous_cancellations ),
            sd = sd(previous_cancellations ),
            n = n(),
            a = mean(previous_cancellations ) + qnorm(0.025) * sd(previous_cancellations ) / sqrt(n()),
            b = mean(previous_cancellations ) + qnorm(0.975) * sd(previous_cancellations ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_previous_cancellations


#previous_bookings_not_canceled
mean_previous_bookings_not_canceled <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(previous_bookings_not_canceled ),
            sd = sd(previous_bookings_not_canceled ),
            n = n(),
            a = mean(previous_bookings_not_canceled ) + qnorm(0.025) * sd(previous_bookings_not_canceled ) / sqrt(n()),
            b = mean(previous_bookings_not_canceled ) + qnorm(0.975) * sd(previous_bookings_not_canceled ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_previous_bookings_not_canceled


#booking_changes
mean_booking_changes <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(booking_changes ),
            sd = sd(booking_changes ),
            n = n(),
            a = mean(booking_changes ) + qnorm(0.025) * sd(booking_changes ) / sqrt(n()),
            b = mean(booking_changes ) + qnorm(0.975) * sd(booking_changes ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_booking_changes


#days_in_waiting_list
mean_days_in_waiting_list <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(days_in_waiting_list ),
            sd = sd(days_in_waiting_list ),
            n = n(),
            a = mean(days_in_waiting_list ) + qnorm(0.025) * sd(days_in_waiting_list ) / sqrt(n()),
            b = mean(days_in_waiting_list ) + qnorm(0.975) * sd(days_in_waiting_list ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_days_in_waiting_list


#adr
mean_adr <- data %>% 
  group_by(lead_time_case) %>%
  summarise(mean = mean(adr ),
            sd = sd(adr ),
            n = n(),
            a = mean(adr ) + qnorm(0.025) * sd(adr ) / sqrt(n()),
            b = mean(adr ) + qnorm(0.975) * sd(adr ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_adr





#required_car_parking_spaces
mean_required_car_parking_spaces <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(required_car_parking_spaces ),
            sd = sd(required_car_parking_spaces ),
            n = n(),
            a = mean(required_car_parking_spaces ) + qnorm(0.025) * sd(required_car_parking_spaces ) / sqrt(n()),
            b = mean(required_car_parking_spaces ) + qnorm(0.975) * sd(required_car_parking_spaces ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_required_car_parking_spaces


#total_of_special_requests
mean_total_of_special_requests <- data %>% 
  group_by(area) %>%
  summarise(mean = mean(total_of_special_requests ),
            sd = sd(total_of_special_requests ),
            n = n(),
            a = mean(total_of_special_requests ) + qnorm(0.025) * sd(total_of_special_requests ) / sqrt(n()),
            b = mean(total_of_special_requests ) + qnorm(0.975) * sd(total_of_special_requests ) / sqrt(n())) %>% 
  mutate(a_t = mean + qt(0.025, df = n - 1) * sd / sqrt(n),
         b_t = mean + qt(0.975, df = n - 1) * sd / sqrt(n)) %>% 
  as.data.frame()

mean_total_of_special_requests


# mean_ci <- rbind(mean_lead_time, mean_stays_in_weekend_nights, 
#                  mean_stays_in_week_nights, mean_adults, mean_children, 
#                  mean_babies,mean_all_children, mean_previous_cancellations,
#                  mean_previous_bookings_not_canceled, mean_booking_changes,
#                  mean_adr, mean_required_car_parking_spaces,
#                  mean_total_of_special_requests)


#########################
# Median
#########################

# area
# arrival_date_month


# #lead_time
# median_lead_time <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(lead_time),
#             a = median(lead_time) - 1.96 * sd(lead_time) / sqrt(n()),
#             b = median(lead_time) + 1.96 * sd(lead_time) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_lead_time
# 
# 
# #stays_in_weekend_nights
# median_stays_in_weekend_nights <- data %>%
#   group_by(area) %>%
#   summarise(median = median(stays_in_weekend_nights),
#             a = median(stays_in_weekend_nights) - 1.96 * sd(stays_in_weekend_nights) / sqrt(n()),
#             b = median(stays_in_weekend_nights) + 1.96 * sd(stays_in_weekend_nights) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_stays_in_weekend_nights
# 
# 
# #stays_in_week_nights
# median_stays_in_week_nights <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(stays_in_week_nights),
#              a = median(stays_in_week_nights) - 1.96 * sd(stays_in_week_nights) / sqrt(n()),
#              b = median(stays_in_week_nights) + 1.96 * sd(stays_in_week_nights) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_stays_in_week_nights
# 
# 
# #adults
# median_adults <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(adults),
#             a = median(adults) - 1.96 * sd(adults) / sqrt(n()),
#             b = median(adults) + 1.96 * sd(adults) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_adults
# 
# 
# #children
# median_children<- data %>% 
#   group_by(area) %>%
#   summarise(median = median(children),
#             a = median(children) - 1.96 * sd(children) / sqrt(n()),
#             b = median(children) + 1.96 * sd(children) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_children
# 
# 
# #babies
# median_babies <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(babies),
#             a = median(babies) - 1.96 * sd(babies) / sqrt(n()),
#             b = median(babies) + 1.96 * sd(babies) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_babies
# 
# 
# #all_children
# median_all_children <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(all_children),
#             a = median(all_children) - 1.96 * sd(all_children) / sqrt(n()),
#             b = median(all_children) + 1.96 * sd(all_children) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_all_children
# 
# 
# #previous_cancellations
# median_previous_cancellations <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(previous_cancellations),
#             a = median(previous_cancellations) - 1.96 * sd(previous_cancellations) / sqrt(n()),
#             b = median(previous_cancellations) + 1.96 * sd(previous_cancellations) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_previous_cancellations
# 
# 
# #previous_bookings_not_canceled
# median_previous_bookings_not_canceled <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(previous_bookings_not_canceled),
#             a = median(previous_bookings_not_canceled) - 1.96 * sd(previous_bookings_not_canceled) / sqrt(n()),
#             b = median(previous_bookings_not_canceled) + 1.96 * sd(previous_bookings_not_canceled) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_previous_bookings_not_canceled
# 
# 
# #booking_changes
# median_booking_changes <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(booking_changes),
#             a = median(booking_changes) - 1.96 * sd(booking_changes) / sqrt(n()),
#             b = median(booking_changes) + 1.96 * sd(booking_changes) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_booking_changes
# 
# 
# #days_in_waiting_list
# median_days_in_waiting_list <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(days_in_waiting_list),
#             a = median(days_in_waiting_list) - 1.96 * sd(days_in_waiting_list) / sqrt(n()),
#             b = median(days_in_waiting_list) + 1.96 * sd(days_in_waiting_list) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_days_in_waiting_list
# 
# 
# #adr
# median_adr <- data %>% 
#   group_by(lead_time_case) %>%
#   summarise(median = median(adr),
#             a = median(adr) - 1.96 * sd(adr) / sqrt(n()),
#             b = median(adr) + 1.96 * sd(adr) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_adr
# 
# 
# #required_car_parking_spaces
# median_required_car_parking_spaces <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(required_car_parking_spaces),
#             a = median(required_car_parking_spaces) - 1.96 * sd(required_car_parking_spaces) / sqrt(n()),
#             b = median(required_car_parking_spaces) + 1.96 * sd(required_car_parking_spaces) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_required_car_parking_spaces
# 
# 
# #total_of_special_requests
# median_total_of_special_requests <- data %>% 
#   group_by(area) %>%
#   summarise(median = median(total_of_special_requests),
#             a = median(total_of_special_requests) - 1.96 * sd(total_of_special_requests) / sqrt(n()),
#             b = median(total_of_special_requests) + 1.96 * sd(total_of_special_requests) / sqrt(n())) %>% 
# as.data.frame()
# 
# median_total_of_special_requests


# 
# median_ci <- rbind(median_lead_time, median_stays_in_weekend_nights, 
#                    median_stays_in_week_nights, median_adults, median_children, 
#                    median_babies, median_all_children, median_previous_cancellations,
#                    median_previous_bookings_not_canceled, median_booking_changes,
#                    median_adr, median_required_car_parking_spaces,
#                    median_total_of_special_requests)
# mean_ci
# median_ci

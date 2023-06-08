library(dplyr)
library(ggplot2)
library("stringr")

data <- read.csv('hotel_bookings.csv')
data$children[is.na(data$children)] = round(mean(data$children, na.rm = TRUE),0)

data <- mutate(data, all_guests = adults + children + babies)   
data <- mutate(data, all_children = children + babies)  
data <-  filter(data, all_guests != 0) 

data <- data %>% select(-c(agent, company))

data <- mutate(data, stays_in_nights = stays_in_weekend_nights + stays_in_week_nights)  

data %>% group_by(stays_in_nights) %>% filter(stays_in_nights == 0) %>%   print(n = Inf)

data <-  filter(data, stays_in_nights != 0)

data$required_car_parking_spaces[data$required_car_parking_spaces > 2] <- 2

data <-  filter(data, adr > 0)
data <-  filter(data, adr < 400)

data <-  filter(data, babies < 5)
data <-  filter(data, children < 10)

data <-  filter(data, adults < 5)

data <-  filter(data, lead_time  < 700)

data <-  filter(data, country != 'NULL')

colSums(is.na(data))
dim(data)

#data conversion into required formats
data$hotel <- as.factor(data$hotel)
data$is_canceled <- as.factor(data$is_canceled)
data$arrival_date_year <- as.factor(data$arrival_date_year)
data$arrival_date_month <- factor(data$arrival_date_month, 
                                  levels = c("January", "February", "March", "April", "May", 
                                             "June", "July", "August", "September", 
                                             "October", "November", "December"))
data$arrival_date_week_number <- factor(data$arrival_date_week_number)
data$arrival_date_day_of_month <- factor(data$arrival_date_day_of_month)
data$meal <- factor(data$meal)
data$country <- factor(data$country)
data$market_segment <- factor(data$market_segment)
data$distribution_channel <- factor(data$distribution_channel)
data$is_repeated_guest <- factor(data$is_repeated_guest)
data$reserved_room_type <- factor(data$reserved_room_type)
data$assigned_room_type <- factor(data$assigned_room_type)
data$deposit_type <- factor(data$deposit_type)
data$customer_type <- factor(data$customer_type)
data$reservation_status <- factor(data$reservation_status)


north_countries <- c('ATA', 'DNK', 'EST', 'FIN', 'FRO', 'GBR', 'IMN', 'IRL', 'ISL',
                     'LTU', 'LVA', 'NOR', 'SWE')
south_countries <- c('ABW', 'AGO', 'AIA', 'ALB', 'AND', 'ARE', 'ARG', 'ARM', 'ASM',
                     'ATF', 'AUS', 'AZE', 'BDI', 'BEN', 'BFA', 'BGD', 'BGR', 'BHR',
                     'BHS', 'BIH', 'BOL', 'BRA', 'BRB', 'BWA', 'CAF', 'CHL', 'CIV',
                     'CMR', 'COL', 'COM', 'CPV', 'CRI', 'CUB', 'CYM', 'CYP', 'DMA',
                     'DOM', 'DZA', 'ECU', 'EGY', 'ESP', 'ETH', 'FJI', 'GAB', 'GEO',
                     'GHA', 'GIB', 'GLP', 'GNB', 'GRC', 'GTM', 'GUY', 'HND', 'HRV', 
                     'IDN', 'IND', 'IRQ', 'ISR', 'ITA', 'JAM', 'JOR', 'KEN', 'KHM',
                     'KIR', 'KNA', 'KWT', 'LAO', 'LBN', 'LBY', 'LCA', 'LKA', 'MAR',
                     'MCO', 'MDG', 'MDV', 'MEX', 'MKD', 'MLI', 'MLT', 'MMR', 'MNE',
                     'MOZ', 'MRT', 'MUS', 'MWI', 'MYS', 'MYT', 'NAM', 'NCL', 'NGA', 
                     'NIC', 'NZL', 'OMN', 'PAK', 'PAN', 'PER', 'PHL', 'PLW', 'PRI', 
                     'PRT', 'PRY', 'PYF', 'QAT', 'RWA', 'SAU', 'SDN', 'SEN', 'SGP', 
                     'SLE', 'SLV', 'SMR', 'STP', 'SUR', 'SYC', 'SYR', 'TGO', 'THA',
                     'TJK', 'TMP', 'TUN', 'TUR', 'TZA', 'UGA', 'UMI', 'URY', 'VEN',
                     'VGB', 'VNM', 'ZAF', 'ZMB', 'ZWE', 'IRN')
centre_countries <- c('AUT', 'BEL', 'BLR', 'CHE', 'CHN', 'CN', 'CZE', 'DEU', 'DJI',
                      'FRA', 'GGY', 'HKG', 'HUN', 'JEY', 'JPN', 'KAZ', 'KOR', 'LIE',
                      'LUX', 'MAC', 'NLD', 'NPL', 'POL', 'ROU', 'RUS', 'SRB', 'SVK', 
                      'SVN', 'TWN', 'UKR', 'USA', 'UZB')

areas <- c(north_countries, south_countries, centre_countries)
data <- data %>% mutate(area = case_when(
  country %in% north_countries ~ 'North',
  country %in% south_countries ~ 'South',
  country %in% centre_countries ~ 'Centre'
))


other <- c('October', 'November', 'December', 'January', 'February', 'March', 'April')

warm <- c('May', 'June', 'July', 'August', 'September')

seasons <- c(warm, other)

data <- data %>% mutate(season = case_when(
  arrival_date_month %in% warm ~ 'Warm',
  arrival_date_month %in% other ~ 'Cold'
))

data <- data %>% mutate(with_children = case_when(
  all_children > 0 ~ 'Yes',
  all_children == 0  ~ 'No'
))


data <- data %>% mutate(lead_time_case = case_when(
  lead_time < 100 ~ 'Short',
  lead_time < 300  ~ 'Middle',
  lead_time >= 300  ~ 'Long'
))

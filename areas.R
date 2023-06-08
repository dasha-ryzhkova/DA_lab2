library(ggplot2)
library(dplyr)
# library(rcompanion)

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

area <- c(north_countries, south_countries, centre_countries)
data <- data %>% mutate(area = case_when(
    country %in% north_countries ~ 'North',
    country %in% south_countries ~ 'South',
    country %in% centre_countries ~ 'Centre'
  ))

dframe <- data.frame(data$hotel, data$area, data$arrival_date_month)


# North Countries 
df <- dframe  %>% filter(data.area == 'North')
df1 <- df %>%
  group_by(data.arrival_date_month) %>%
  summarise(n = n())

df
df1
ggplot(df, aes(x=data.arrival_date_month, fill=data.hotel)) + 
  geom_histogram(stat="count") +
  labs(x = "Month", title = "North countries") +
  theme(text = element_text(size = 15))



# South Countries && City Hotel
df <- dframe  %>% filter(data.area == 'South')
df1 <- df %>%
  group_by(data.arrival_date_month) %>%
  summarise(n = n())

df
df1
ggplot(df, aes(x=data.arrival_date_month, fill=data.hotel)) + 
  geom_histogram(stat="count") +
  labs(x = "Month", title = "South countries") +
  theme(text = element_text(size = 15))


# Centre Countries && City Hotel
df <- dframe  %>% filter(data.area == 'Centre')
df1 <- df %>%
  group_by(data.arrival_date_month) %>%
  summarise(n = n())

df
df1
ggplot(df, aes(x=data.arrival_date_month, fill=data.hotel)) + 
  geom_histogram(stat="count") +
  labs(x = "Month", title = "Centre countries") +
  theme(text = element_text(size = 15))





ggplot(data, aes(x=arrival_date_month, fill=area)) + 
  geom_histogram(stat="count") +
  labs(x = "Month", title = "Popularity for Countries") +
  theme(text = element_text(size = 15))


df <- data %>% filter(is.na(area))












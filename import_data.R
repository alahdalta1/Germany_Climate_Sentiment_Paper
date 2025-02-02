


# Run scripts in order:
# 1. import_data.R
# 2. main.R
# 3. run.R



packages <- c("readxl", "tidyr", "dplyr", "rgdal", "sf", "ggplot2", "scales")

lapply(packages, library, character.only = TRUE)

source("functions_plotting.R")




# Load data ################################################################################

# Load aggregated data
file_aggr_data <- "Data/LIWCaggregatedwithclimate.csv"
aggr_data <- read.csv(file_aggr_data) 
aggr_data <- as_tibble(aggr_data)

aggr_data$FID_1 <- aggr_data$FID_1 + 1

data <- aggr_data

data$NTw <- data$n_only_negative + data$n_both
data$TTw <- data$n_total
data$emoTw <- data$n_total - data$n_neutral

data <- data %>% relocate(NTw, TTw, emoTw)

data$week_date <- as.Date(data$week_date)


# Load previous version with climate for all weeks
out <- load("Data/climate_by_FID_and_week.RData")

data_climate <- data_climate %>% filter(Week %in% unique(data$Week))
data_climate$FID_1 <- data_climate$FID_1 + 1
# Set number of tweets to zero since the values that are missing in LIWCaggregatedwithclimate.csv
# are probably missing because there were no tweets in those week-region-pairs
data_climate <- data_climate %>% mutate(
  NTw = 0, TTw = 0, emoTw = 0, 
  n_only_positive = 0, n_only_negative = 0, n_neutral = 0, 
  n_both = 0, n_more_positive = 0, n_more_negative = 0, n_total = 0
)

data_week_date <- data %>% select(week_date, Week) %>% distinct() %>% arrange(Week)

data_climate <- left_join(data_climate, data_week_date) %>% relocate(FID_1, NAME_2, Week, week_date)


# week-region-pairs with data available
data_available <- data %>% select(NAME_2, Week) %>% arrange(NAME_2, Week)

# All combinations of week-region-pairs
unique_weeks <- data %>% pull(Week) %>% unique() %>% sort()
unique_names <- data %>% pull(NAME_2) %>% unique() %>% sort()
data_all_combinations <- cross_join(tibble(Week=unique_weeks), tibble(NAME_2=unique_names)) %>% 
  relocate(NAME_2) %>% arrange(NAME_2, Week)

# week-region-pairs that are in data_all_combination but which are not in data_available (so missing week-region-pairs)
data_missing <- setdiff(data_all_combinations, data_available)
data_missing <- data_missing %>% 
  left_join(data %>% filter(NAME_2 %in% data_missing$NAME_2) %>% select(NAME_2, FID_1) %>% distinct()) %>% 
  arrange(FID_1, Week)

# add these missing week-region-pairs to the data
data_missing_climate <- left_join(data_missing, data_climate)
data <- bind_rows(data, data_missing_climate)



# Compute dates 
data <- data %>% rename(date = week_date) 
data <- data %>% mutate(calendar_week = as.numeric(strftime(date, format = "%V")),
                        calendar_month = as.numeric(strftime(date, format = "%m")),
                        year = as.numeric(format(date, "%Y")),
                        unique_month = calendar_month + (year-2019)*12)




# Load map
load("Data/map.RData")

map$FID_1 <- map$FID_1 + 1

data <- data %>% arrange(FID_1, Week)
map <- map %>% arrange(FID_1)



# Compute total number of tweets each week over all regions

data_by_weeks <- data %>% group_by(Week) %>% summarise(TTw = sum(TTw))

plot_scatter_lines(data_by_weeks, "Week", "TTw")

# Remove weeks with few tweets
data <- data %>% filter(!Week %in% c(12:15,23:27))
data_by_weeks <- data %>% group_by(Week) %>% summarise(TTw = sum(TTw))

plot_scatter_lines(data_by_weeks, "Week", "TTw")



# Add fatalities data 
out <- load("Data/fatalities.RData")

data <- left_join(data, map_w_fatalities %>% select(FID_1, fatalities, any_fatalities))



save(data, map, file="Data/data.RData")

# load("Data/data.RData")








# Aggregate data to NAME_1 region level ########################################################################

data$NAME_1 %>% table()

# Some NAME_1 are NA, but only for some of the weeks. 
# Pool the weeks and choose those combinations where NAME_1 was not NA.
data_names <- data %>% select(NAME_1,NAME_2) %>% distinct()
data_names <- data_names %>% filter(!is.na(NAME_1))
data <- data %>% select(-NAME_1)
data <- data %>% left_join(data_names)
# Test that it worked.
data_names <- data %>% select(NAME_1,NAME_2) %>% distinct()
data_names %>% filter(is.na(NAME_1))

data %>% group_by(NAME_1) %>% summarise(n=n())

# Join regions with a relatively small amount of data with its neighboring region
data0 <- data %>% mutate(NAME_1=ifelse(NAME_1=="Hamburg", "Schleswig-Holstein", NAME_1),
                         NAME_1=ifelse(NAME_1=="Bremen", "Niedersachsen", NAME_1),
                         NAME_1=ifelse(NAME_1=="Berlin", "Brandenburg", NAME_1))

map0 <- map %>% select(-NAME_1) %>% 
  left_join(data0 %>% select(FID_1, NAME_1) %>% distinct())

data0 %>% group_by(NAME_1) %>% summarise(n=n())

# Plot each region on NAME_2 level, colored by the region on NAME_1 level that it belongs to
plot_sf(left_join(data0 %>% filter(Week==60), map0 %>% select(FID_1, geometry)), "NAME_1")



# Aggregate data to NAME_1 level
data_aggregated <- data0 %>% group_by(NAME_1, Week) %>% 
  summarise(NTw = sum(NTw),
            TTw = sum(TTw),
            emoTw = sum(emoTw),
            Temp = mean(Temp),
            Precip = mean(Precip),
            n_only_positive = sum(n_only_positive),
            n_only_negative = sum(n_only_negative),
            n_neutral = sum(n_neutral),
            n_more_positive = sum(n_more_positive),
            n_more_negative = sum(n_more_negative),
            n_both = sum(n_both),
            n_total = sum(n_total),
            Week = first(Week),
            date = first(date),
            calendar_week = first(calendar_week),
            calendar_month = first(calendar_month),
            unique_month = first(unique_month),
            year = first(year),
            fatalities = sum(fatalities),
            any_fatalities = max(any_fatalities)) %>% 
  ungroup()

unique_names <- data_aggregated %>% pull(NAME_1) %>% unique()



# Aggregate map to NAME_1 level
map_aggregated <- tibble(NAME_1 = unique_names)
geometry <- NULL

for (j in 1:nrow(map_aggregated)) {
  
  map_curr <- map0 %>% filter(NAME_1==map_aggregated$NAME_1[j])
  
  if (length(geometry)==0) {
    geometry <- st_combine(st_union(map_curr$geometry))
  } else {
    geometry <- c(geometry, st_combine(st_union(map_curr$geometry)))
  }
}

map_aggregated$geometry <- geometry
map_aggregated$FID_1 <- 1:nrow(map_aggregated)

# Plot aggregated map
plot_sf(map_aggregated, "NAME_1")

# Plot aggregated data (fatalities)

plot_sf(left_join(data_aggregated %>% filter(Week==1), map_aggregated), "fatalities")

plot_sf(left_join(data_aggregated %>% filter(Week==1), map_aggregated) %>% 
          mutate(fatalities_pow = fatalities^0.25), "fatalities_pow")

data_aggregated <- data_aggregated %>% left_join(map_aggregated %>% select(NAME_1, FID_1)) %>% 
  relocate(FID_1, NAME_1)
data_aggregated <- data_aggregated %>% mutate(ratio=NTw/TTw)




save(data_aggregated, map_aggregated, file="Data/data_aggregated.RData")

















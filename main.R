

# Run scripts in order:
# 1. import_data.R
# 2. main.R
# 3. run.R



options <- list()
options$use_aggregated_version <- TRUE



for (setup_type in 1:2) {
  
  rm(list=setdiff(ls(), c("setup_type", "options")))
  
  source("functions.R")
  source("functions_general.R")
  
  
  # Define year and season
  if (setup_type==1) {
    
    season_type <- "full"
    year_selection <- 2020
    
    data_subset_name <- "full_2020"
  } else if (setup_type==2) {
    
    season_type <- "warm"
    year_selection <- c(2019, 2021, 2022)
    
    data_subset_name <- "warm_19_21_22"
  }
  
  cat(sprintf("Data: %s\n", data_subset_name))
  
  
  
  # Load data created in script import_data.R
  if (options$use_aggregated_version) {
    output <- load("Data/data_aggregated.RData") #map_aggregated, data_aggregated
    data <- data_aggregated
    map <- map_aggregated
    rm(data_aggregated, map_aggregated)
    
    data_subset_name <- paste0("aggr_", data_subset_name)
    
  } else {
    output <- load("Data/data.RData") #map, data
    
    data_subset_name <- paste0("orig_", data_subset_name)
    
  }
  
  data_filename <- paste0("Data/subsets/data_", data_subset_name, ".RData")
  
  
  
  
  
  # Select warm weeks, cold weeks or all weeks during the year(s)
  if (season_type=="warm") {
    calendar_week_selection <- 12:47
  } else if (season_type=="cold") {
    calendar_week_selection <- unique(data$calendar_week) %>% setdiff(12:47) %>% 
      sort() 
  } else if (season_type=="full") {
    calendar_week_selection <- unique(data$calendar_week) %>% 
      sort() 
  }
  
  # Filter data on selected weeks
  data1 <- filter(data, calendar_week %in% calendar_week_selection)
  
  data1 <- data1 %>% filter(year %in% year_selection)
  
  
  
  
  # Create indices for weeks, months and time (useful when running INLA)
  data1 <- data1 %>% mutate(week_idx = match(Week, unique(Week)),
                            shared_month_idx = match(calendar_month, unique(calendar_month)),
                            month_idx = match(unique_month, unique(unique_month)),
                            year_idx = match(year, unique(year)),
                            time_idx = year_idx)  
  
  
  
  
  # Create bins for precipitation (low, mid and high precipitation)
  data1 <- data1 %>% mutate(PrecipBins = as.numeric(cut(Precip,8)))
  data1$PrecipBins %>% table()
  data1 <- data1 %>% mutate(PrecipBins = if_else(PrecipBins >= 5, 5, PrecipBins),
                            PrecipBins = if_else(PrecipBins==1, 2, PrecipBins),
                            PrecipBins = PrecipBins-1)
  data1$PrecipBins %>% table()
  data1 <- data1 %>% mutate(PrecipBins = if_else(PrecipBins >= 3, 3, PrecipBins))
  data1$PrecipBins %>% table()
  
  
  
  
  # Create bins for temperature depending on season
  if (season_type=="warm") {
    data1 <- data1 %>% mutate(TempBins = as.numeric(cut(Temp,c(min(Temp)-1,18,20,23,max(Temp)+1))))
    
  } else if (season_type=="cold") {
    data1 <- data1 %>% mutate(TempBins = as.numeric(cut(Temp,c(min(Temp)-1,3,6,max(Temp)+1))))
    
  } else if (season_type=="full") {
    data1 <- data1 %>% mutate(TempBins = as.numeric(cut(Temp,c(min(Temp)-1,3,6,18,20,23,max(Temp)+1))))
    
  } else {
    stop("season error")
  }
  
  data1$TempBins %>% table()
  data1$PrecipBins %>% table()
  
  
  # Summarise temperature bins
  temp_summary <- data1 %>% group_by(TempBins) %>% 
    summarise(min_temp=min(Temp), max_temp=max(Temp), median_temp=median(Temp), 
              n=n(), n_tweets=sum(TTw))
  # Summarise precipitation bins
  precip_summary <- data1 %>% group_by(PrecipBins) %>% 
    summarise(min_precip=min(Precip), max_precip=max(Precip), median_precip=median(Precip), 
              n=n(), n_tweets=sum(TTw))
  
  # Number of tweets within each temperature bin
  n_tweets <- temp_summary$n_tweets
  sprintf("%.3g, %.3g, %.3g, %.3g", n_tweets[1], n_tweets[2], n_tweets[3], n_tweets[4])
  
  # Number of tweets within each precipitation bin
  n_tweets <- precip_summary$n_tweets
  sprintf("%.3g, %.3g, %.3g", n_tweets[1], n_tweets[2], n_tweets[3])
  
  
  
  # Arrange data1 by year, month and week
  data1 <- data1 %>% arrange(year_idx, month_idx, week_idx) %>% 
    relocate(FID_1, week_idx, year_idx, month_idx, date)
  
  
  
  # Save
  save(data1, data_subset_name, file=data_filename)
  
}




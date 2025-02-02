

# Run scripts in order:
# 1. import_data.R
# 2. main.R
# 3. run.R



options <- list()


# Load names of files with data created in script main.R
data_file_lst <- list.files(path = "./Data/subsets", pattern = "^data_aggr_.*.RData")


# Run on data in loaded files
for (data_file in data_file_lst) {
  
  data_file <- paste0("Data/subsets/", data_file)
  print(data_file)
  
  # Check if file name contains the string "data_aggr", if so then it is aggregated, i.e. it is on NAME_1 scale 
  options$is_aggr_data <- grepl("data_aggr", data_file)
  
  # Run for negative tweets and for emotional tweets
  for (data_type in c(1,3)) { # data_type controls the response variable (1: negative tweets, 2: positive tweets, 3: emotional tweets)
    
    # Reset environment, i.e., remove everything from previous iterations except for the essential variables "data_file_lst", ...
    rm(list=setdiff(ls(), c("data_file_lst", "data_file", "data_type", "options")))
    
    if (data_type==1) {
      #if use_positive and use_all_emo are FALSE, then negative tweets are used as response variable
      use_positive <- FALSE
      use_all_emo <- FALSE
    } else if (data_type==2) {
      # use positive tweets as response variable
      use_positive <- TRUE
      use_all_emo <- FALSE
    } else if (data_type==3) {
      # use both positive and negative tweets as response variable
      use_positive <- FALSE
      use_all_emo <- TRUE
    } else {
      stop("data_type error.")
    }
    
    
    # Setup ##############################################################################
    source("functions_general.R")
    source("functions.R")
    
    # Load data
    output <- load(file=data_file) #data1
    
    if (options$is_aggr_data) {
      output <- load("Data/data_aggregated.RData") #map_aggregated, data_aggregated
      map <- map_aggregated
      
      rm(map_aggregated, data_aggregated)
      
    } else { 
      output <- load("Data/data.RData") #map, data
      
      rm(data)
      
    }
    
    
    
    if (use_positive && use_all_emo) {
      stop("only one of these options should be true.")
    }
    
    # Define response variable (it is called NTw even when positive or emotional tweets are used because it
    # is convenient, then I don't have to change the code below to something other than NTw)
    if (use_positive) {
      data1$NTw <- data1$n_only_positive + data1$n_both
    } else if (use_all_emo) {
      data1$NTw <- data1$n_total - data1$n_neutral
    } else { 
      if (!identical(data1$NTw, data1$n_only_negative + data1$n_both))
        stop("NTw error")
    }
    
    
    # Select only those regions in map that are present in data1
    map <- map %>% filter(FID_1 %in% unique(data1$FID_1))
    
    # load(file="Data/fatalities.RData")
    
    # data1 <- left_join(data1, map_w_fatalities)
    

    
    
    
    # Create week, month, year and time idx
    n_years <- length(unique(data1$year_idx))
    data1$week_idx <- match(data1$Week, unique(data1$Week))
    data1$month_idx <- match(data1$month_idx, unique(data1$month_idx))
    data1$year_idx <- match(data1$year_idx, unique(data1$year_idx))
    data1$time_idx <- data1$year_idx
    n_unique_weeks <- length(unique(data1$calendar_week))
    n_weeks <- length(unique(data1$week_idx))
    cat(sprintf("n calendar weeks: %d, n weeks total: %d\n", n_weeks, n_unique_weeks))
    
    
    
    map <- arrange(map, FID_1)
    
    # shapes <- as_Spatial(map$geometry)
    # nb_map <- poly2nb(shapes)
    # nb2INLA("map.adj", nb_map)
    
 
    
    
    
    n_regions <- length(unique(data1$FID_1))
    
    T3 <- sum(data1$NTw) / sum(data1$TTw) # ratio NTw/TTw over the whole dataset
    data1$E <- data1$TTw*T3 # expected value of the response variable NTw according to the ratio for the whole dataset
    
    
    # Append what type of response variable is used to the model name
    if (use_positive) {
      use_positive_str <- "T"
    } else {
      use_positive_str <- "F"
    }
    if (use_all_emo) {
      use_all_emo_str <- "T"
    } else {
      use_all_emo_str <- "F"
    }
    model_name_start <- paste0(data_subset_name, 
                               "_pos", use_positive_str, "_emo", use_all_emo_str)    
    
    
    # If we run the model over more than one year, then we need two region indices (because we have then two random effects for regions, one by year and one by month)
    if (n_years>1)
      data1$FID_2 <- data1$FID_1
    
    
    
    # Sort data1 by year, month and week
    data1 <- data1 %>% arrange(year_idx, month_idx, week_idx)
    
    
    
    # Define INLA models ###############################################################################
    # Time iid model
    if (n_years>1) {
      formula1 <- NTw ~ -1 + f(FID_1, model = "iid", # FID_1, random effect in time weeks
                               group = week_idx, 
                               control.group=list(model='iid'),
                               constr = FALSE) +
        f(FID_2, model = "iid", # FID_2, random effect in time years
          group = year_idx,
          control.group=list(model='iid'),
          constr = FALSE)
      
      time_variable1 <- c("week_idx", "year_idx")
    } else {
      formula1 <- NTw ~ -1 + f(FID_1, model = "iid", # Need only FID_1 random effect when running for only one year
                               group = week_idx,
                               control.group=list(model='iid'),
                               constr = FALSE)
      
      time_variable1 <- "week_idx"
    }
    
    
    # Time AR model
    if (n_years>1) {
      formula2 <- NTw ~ -1 + f(FID_1, model = "iid", # FID_1, random effect in time weeks
                               group = week_idx,
                               control.group=list(model='ar1'),
                               constr = FALSE) +
        f(FID_2, model = "iid",  # FID_2, random effect in time years
          group = year_idx,
          control.group=list(model='iid'),
          constr = FALSE)
      
      time_variable2 <- c("week_idx", "year_idx")
    } else {
      formula2 <- NTw ~ -1 + f(FID_1, model = "iid", # Need only FID_1 random effect when running for only one year
                               group = week_idx,
                               control.group = list(model='ar1'),
                               constr = FALSE) 
      
      time_variable2 <- "week_idx"
    }
    
    
    # Time AR1 model with PrecipBins as random effect
    if (n_years>1) {
      formula3 <- NTw ~ -1 + f(FID_1, model = "iid", # FID_1, random effect in time months
                               group = month_idx,
                               control.group=list(model='ar1'),
                               constr = FALSE) +
        f(FID_2, model = "iid", # FID_2, random effect in time years
          group = year_idx,
          control.group=list(model='iid'),
          constr = FALSE) +
        f(PrecipBins, model="rw1", constr = TRUE) # PrecipBins random effect
      
      time_variable3 <- c("month_idx", "year_idx")
    } else {
      formula3 <- NTw ~ -1 + f(FID_1, model = "iid", # Need only FID_1 random effect when running for only one year
                               group = month_idx,
                               control.group=list(model='ar1'),
                               constr = FALSE) +
        f(PrecipBins, model="rw1", constr = TRUE) # PrecipBins random effect
      
      time_variable3 <- "month_idx"
    }
    
    
    # Time AR1 model with PrecipBins and TempBins as random effects 
    if (n_years>1) {
      formula4 <- NTw ~ -1 + f(FID_1, model = "iid", # FID_1, random effect in time months
                               group = month_idx,
                               control.group=list(model='ar1'),
                               constr = FALSE) +
        f(FID_2, model = "iid", # FID_2, random effect in time years
          group = year_idx,
          control.group=list(model='iid'),
          constr = FALSE) +
        f(PrecipBins, model="rw1", constr = TRUE) + # PrecipBins random effect
        f(TempBins, model="rw1", constr = TRUE) # TempBins random effect
      
      time_variable4 <- c("month_idx", "year_idx")
    } else {
      formula4 <- NTw ~ -1 + f(FID_1, model = "iid", # Need only FID_1 random effect when running for only one year
                               group = month_idx,
                               control.group=list(model='ar1'),
                               constr = FALSE) +
        f(PrecipBins, model="rw1", constr = TRUE) + # PrecipBins random effect
        f(TempBins, model="rw1", constr = TRUE) # TempBins random effect
      
      time_variable4 <- "month_idx"
    }
    
    
    
    
    # Run INLA models ###################################################################
    data1$E_input <- data1$E
    # browser()
    
    # Run first model
    res <- inla(formula1, family = "poisson", data = data1, E = data1$E_input,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE))
    
    # Summary of results
    summary(res) %>% print()

    # Process the results and save plots
    process_results(res, data1,  
                    model_name=paste0(model_name_start, "_model1"),
                    time_variable=time_variable1,
                    compute_difference_by_simulation=TRUE)
    
    
    
    # Run second model
    res <- inla(formula2, family = "poisson", data = data1, E = data1$E_input,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE))
    
    # Summary of results
    summary(res) %>% print()
    
    # Process the results and save plots
    process_results(res, data1,  
                    model_name=paste0(model_name_start, "_model2"),
                    time_variable=time_variable2,
                    compute_difference_by_simulation=TRUE)
    
    
    
    # Run third model
    res <- inla(formula3, family = "poisson", data = data1, E = data1$E_input,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE))
    
    # Summary of results
    summary(res) %>% print()
    
    # Process the results and save plots
    process_results(res, data1, 
                    model_name=paste0(model_name_start, "_model3"),
                    time_variable=time_variable3,
                    compute_difference_by_simulation=FALSE)
    
    
    
    # Run fourth model
    res <- inla(formula4, family = "poisson", data = data1, E = data1$E_input,
                control.predictor = list(compute = TRUE),
                control.compute = list(config = TRUE))
    
    # Summary of results
    summary(res) %>% print()
    
    # Process the results and save plots
    process_results(res, data1,
                    model_name=paste0(model_name_start, "_model4"),
                    time_variable=time_variable4,
                    compute_difference_by_simulation=FALSE)
    
    
    
  }
  
}





# set the library path to your user-specific library directory
library_path <- "/export/data/talahdal"
if (!file.exists(library_path)) {
	  dir.create(library_path, recursive = TRUE)
}
.libPaths(c(library_path, .libPaths()))

#List of packages to install and load
packages <- c("readxl", "tidyr", "dplyr", "readr", "ggplot2", "scales")

# Install and load packages
for (package in packages) {
	  if (!requireNamespace(package, quietly = TRUE)) {
		      install.packages(package)
  }
  library(package, character.only = TRUE)
}

# Your script's main code goes here




###############################################################################
# Extract week ################################################################
###############################################################################

library(lubridate)
library(glue)
lang = 'de'
year = '2015'
# raw_data <- read.csv("LIWCForGermany.csv")
raw_data <- read.csv(glue("/export/data/talahdal/LIWCMergedYears/LIWC_results_{lang}AllYears_resultsnuts3.csv"))
raw_data=unique(raw_data$cleaned_text)
raw_data2 <- raw_data %>%
	  mutate(
		     Year = year(ymd_hms(date)),
		         Month = month(ymd_hms(date)),
		         DateWeek = week(ymd_hms(date)),
			     YMD = ymd(format(ymd_hms(date), "%Y%m%d"))
			   )


start_date <- as.Date(glue("{year}-01-01"))
raw_data2$date <- as.Date(raw_data2$date)
num_days_since_start <- as.double(difftime(raw_data2$date, start_date, units="days"))
num_weeks_since_start <- floor(num_days_since_start / 7)
# set week nr to number of weeks since start date + 1 so that the first week has week_nr=1
raw_data2$week_nr <- num_weeks_since_start + 1
raw_data2 <- relocate(raw_data2, week_nr)

week_with_date <- raw_data2 %>% select(date, week_nr) %>% group_by(week_nr) %>% summarise(week_date = min(date))
raw_data2 <- raw_data2 %>% left_join(week_with_date)


###############################################################################
# Classify and aggregate sentiment, version 2 #################################
###############################################################################

# Classify
# These first four categories are mutually exclusive
raw_data2$is_only_positive <- raw_data2$posEmo > 0 & raw_data2$negEmo == 0
raw_data2$is_only_negative <- raw_data2$negEmo > 0 & raw_data2$posEmo == 0
raw_data2$is_neutral <- raw_data2$posEmo == 0 & raw_data2$negEmo == 0
raw_data2$is_both_positive_and_negative <- raw_data2$posEmo > 0 & raw_data2$negEmo > 0

# These two categories are subsets of the 'is_both_positive_and_negative'-category
raw_data2$is_more_positive <- raw_data2$posEmo > raw_data2$negEmo & 
	  raw_data2$is_both_positive_and_negative
  raw_data2$is_more_negative <- raw_data2$negEmo > raw_data2$posEmo & 
	    raw_data2$is_both_positive_and_negative

    # Aggregate
    pivot_df_v2 <- raw_data2 %>%
	      group_by(week_nr, week_date, country, District) %>%
	        summarize(n_only_positive = sum(is_only_positive),
			              n_only_negative = sum(is_only_negative),
				                  n_neutral = sum(is_neutral),
				                  n_both = sum(is_both_positive_and_negative),
						              n_more_positive = sum(is_more_positive),
						              n_more_negative = sum(is_more_negative)) %>% 
      ungroup() %>% 
        mutate(n_total = n_only_positive + n_only_negative + n_neutral + n_both)

# write_csv(pivot_df_v2, glue('/export/data/talahdal/LIWCencoding/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_{lang}.csv'), fileEncoding='UTF-8')
save(pivot_df_v2, glue('export/data/talahdal/LIWCencoding/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_{lang}.RData'))

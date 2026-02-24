# Set the library path to your user-specific library directory
library_path <- "/export/data/talahdal/myRlibrary"
if (!file.exists(library_path)) {
	  dir.create(library_path, recursive = TRUE)
}
.libPaths(c(library_path, .libPaths()))

# List of packages to install and load
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
# raw_data <- read.csv("LIWCForGermany.csv")
raw_data <- read.csv("/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_all_last.csv")

###############################################################################
# Classify and aggregate sentiment, version 2 #################################
###############################################################################

# Classify
    # Aggregate
    pivot_df_v2 <- raw_data %>%
	      group_by(week_nr, week_date, country, District) %>%
	        summarize(n_only_positive = sum(n_only_positive),
			             n_only_negative = sum(n_only_negative),
				                n_neutral = sum(n_neutral),
				                n_both = sum(n_both),
					                   n_more_positive = sum(n_more_positive),
					            n_more_negative = sum(n_more_negative)) %>% 	
      ungroup() %>% 
        mutate(n_total = n_only_positive + n_only_negative + n_neutral + n_both)

save(pivot_df_v2, file = "aggregate_after_R_filtered_last.RData")
# write_csv(pivot_df_v2, '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_all_after_R_filtered.csv')

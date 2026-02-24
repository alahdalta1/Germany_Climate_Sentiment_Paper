

source("functions_plotting.R")
source("functions.R")



packages <- c("readxl", "tidyr", "dplyr", "rgdal", "sf", "ggplot2", "scales")

lapply(packages, library, character.only = TRUE)


#load('climate_sentiment_cleaned_text_withFID_filtered.RData')
#ls()
df <- read.csv('all_countries_lasso_bins_NA_an_cdc_final.csv')
library(dplyr)
#df <- df %>% filter(year == 2022)
#df$NTw[is.na(df$NTw)] <- 0
#df <- df[!is.na(df$NTw), ]
#df <- df %>% filter(country == "United Kingdom")

#df <- df %>%
#    filter(!(country %in% c("Belgium")))
#df <- df %>%
 # filter(country == "United Kingdom")

#df <- df %>%
 # filter(!shared_month_idx %in% c(8, 9, 10, 11, 12))
#df <- df %>%
 # mutate(
  #  NTw = replace(NTw, is.na(NTw), 0),
   # TTw = replace(TTw, is.na(TTw), 0)
  #)
#merged_df$group1 <- as.numeric(merged_df$group1)
#df <- df %>%
#filter(country %in% c("Spain"))
#df <- df %>%
#  filter(NTw != 0 | TTw != 0)
df 

#indices_to_update <- which(df$month_idx >= 10)
#df$month_idx[indices_to_update] <- df$month_idx[indices_to_update] - 1
#df
#---Join week 53 with week 52 (more stable for the INLA model since week 53 have fewer data-points)
#df <- df %>% mutate(calendar_week=ifelse(calendar_week==53, 52, calendar_week))

#---Filter on weeks above 185

#df <- subset(df, !(week_idx >= 356 & week_idx <= 366))#df <- df %>% filter(week_idx > 205)


#---Renumber weeks and months
#df <- df %>% filter(week_idx > 205)
df$week_date <- as.Date(df$week_date)
#df <- df %>%
#  filter(format(week_date, "%Y") != "2020")
#df$pollen_alder_Bins <- as.factor(df$pollen_alder_Bins)
df <- df %>% mutate(week_idx=match(week_idx, sort(unique(week_idx))))
df <- df %>% mutate(month_idx=match(month_idx, sort(unique(month_idx))))
df <- df %>% mutate(country_ID=match(country_ID, sort(unique(country_ID))))
 

df <- df %>% select(FID, week_idx,country,NUTS2, month_idx,time_idx, country_ID, calendar_week,
                    TTw, NTw,airqualityBins,SolarradiationBins,Cloud_fractionBins,SPIBins,Pollen_birch_bins,Pollen_alder_bins,Pollen_oliver_bins,PrecipBins,TempBins,NAOBins,Wind_speedBins,Wind_directionBins,Relative_humidityBins,TempmeanBins,TempminBins,an_bin,TotalCases_bin,beta,beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9,beta_10)
df <- df %>% arrange(week_idx, FID,country_ID)
df
#T3 <- sum(df$NTw) / sum(df$TTw)
#T3 <- sum(df$NTw, na.rm = TRUE) / sum(df$TTw)
T3 <- sum(df$NTw, na.rm = TRUE) / sum(df$TTw[!is.na(df$NTw)])
df$E <- df$TTw*T3
T3 <- sum(df$NTw) / sum(df$TTw)

#When we run the model for more than two random effects with FID
df$FID_2 <- df$FID
library(dplyr)

# collapse 4 bins -> 3 bins by merging the middle two (2 & 3 -> 2)
collapse_4to3_mid <- function(x){
  xi <- as.integer(as.character(x))
  out <- dplyr::case_when(
    is.na(xi)      ~ NA_integer_,
    xi == 1L       ~ 1L,        # low
    xi %in% c(2,3) ~ 2L,        # middle (merged 2 & 3)
    xi == 4L       ~ 3L,        # high
    TRUE           ~ NA_integer_
  )
  as.integer(out)  # INLA rw1 likes consecutive integers
}

# Overwrite the existing columns used by your formula
df <- df %>%
  mutate(
    TempBins          = collapse_4to3_mid(TempBins),
    Pollen_alder_bins = collapse_4to3_mid(Pollen_alder_bins),
    Pollen_birch_bins = collapse_4to3_mid(Pollen_birch_bins)
  )

# sanity check
cat("TempBins:\n");            print(table(df$TempBins, useNA="ifany"))
cat("Pollen_alder_bins:\n");   print(table(df$Pollen_alder_bins, useNA="ifany"))
cat("Pollen_birch_bins:\n");   print(table(df$Pollen_birch_bins, useNA="ifany"))



formula1 <- NTw ~ 1 + 
  f(FID, model = "iid", # FID, random effect in time months
    group = week_idx,
    control.group=list(model='ar1'),
    constr = FALSE) +
  f(country_ID, model="iid", constr=TRUE)+
  f(calendar_week, model="rw1", constr=TRUE)+
  f(SPIBins, model="rw1", constr=TRUE)+
  f(TempBins, model="rw1",constr=TRUE)+
  f(airqualityBins, model="rw1",constr=TRUE)+
  f(an_bin, model="rw1", constr=TRUE)+
  f(TotalCases_bin, model="rw1", constr=TRUE)+
  f(Wind_speedBins, model="rw1",constr=TRUE)+
  f(Pollen_birch_bins, model="rw1",constr=TRUE)+
  f(Pollen_alder_bins, model="rw1",constr=TRUE)+
  f(Pollen_oliver_bins, model="rw1",constr=TRUE)+
  f(beta, model="rw1", constr=TRUE)+
  f(beta_1, model="rw1", constr=TRUE)+
  f(beta_2, model="rw1",constr=TRUE)+
  f(beta_3, model="rw1",constr=TRUE)+
  f(beta_4, model="rw1",constr=TRUE)+
  f(beta_5, model="rw1",constr=TRUE)+
  f(beta_6, model="rw1",constr=TRUE)+
  f(beta_7, model="rw1", constr=TRUE)+
  f(beta_8, model="rw1",constr=TRUE)+
  f(beta_9, model="rw1",constr=TRUE)+
  f(beta_10, model="rw1",constr=TRUE)
  #f(Wild_fire_PM2_5_binned,model="rw1",constr = TRUE)+
  #f(Wild_fire_RP_bin, model="rw1", constr=TRUE)
  #f(pollen_birch_Bins, model="rw1", constr=TRUE)+
  #f(pollen_oliver_Bins, model="rw1", constr=TRUE)+
  #f(pollen_alder_Bins, model="rw1", constr=TRUE)
  #f(group1,model="rw1",constr = TRUE)
  #f(group3,model="rw1",constr = TRUE)+
  #f(group4,model="rw1",constr = TRUE)  

library(INLA)



#---Re-arrange FID and week_idx so the index starts from zero
df <- df %>% mutate(FID = match(FID, sort(unique(FID))),
                              week_idx = match(week_idx, sort(unique(week_idx))))
#---Arrange df in the order week_idx and FID
df <- df %>% arrange(week_idx, FID)
df$E_input <- df$E
res <- inla(formula1, family = "poisson", data = df, E = df$E_input,
            control.predictor = list(compute = TRUE),
            control.compute   = list(dic = TRUE, waic = TRUE, cpo = TRUE, config = TRUE),
            control.inla = list(int.strategy = "eb", control.vb = list(f.enable.limit = 2)),
            verbose = TRUE)
# --- Fit criteria
dic   <- if (!is.null(res$dic))  res$dic$dic  else NA_real_
pdic  <- if (!is.null(res$dic))  res$dic$p.eff else NA_real_
waic  <- if (!is.null(res$waic)) res$waic$waic else NA_real_
pwaic <- if (!is.null(res$waic)) res$waic$p.eff else NA_real_
mlik  <- suppressWarnings(as.numeric(res$mlik[1]))
lsc   <- if (!is.null(res$cpo$cpo)) mean(-log(res$cpo$cpo), na.rm = TRUE) else NA_real_

cat("Fit criteria:\n",
    "  DIC  =", format(dic,  digits = 6), " (p.eff =", format(pdic,  digits = 6), ")\n",
    "  WAIC =", format(waic, digits = 6), " (p.eff =", format(pwaic, digits = 6), ")\n",
    "  Marginal logLik =", format(mlik, digits = 6), "\n",
    "  LogScore (mean -log CPO) =", format(lsc, digits = 6), "\n")

# Optional: write to CSV alongside your other outputs
fit_tbl <- data.frame(
  model = "IID",
  DIC = dic, DIC_p_eff = pdic,
  WAIC = waic, WAIC_p_eff = pwaic,
  MarginalLogLik = mlik,
  LogScore = lsc
)
readr::write_csv(fit_tbl,
  "/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/IID_fit_metrics.csv"
)

#summary(res) %>% print()
save(res, file = "/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/SPI_model.RData")

#load("TMP/SPI_model.RData")

df_data <- res$.args$data

res$call

res$.args$formula


df_data$FID %>% table() %>% table()




# df_data <- df_data %>% mutate(FID = match(FID, sort(unique(FID))),
#                               week_idx = match(week_idx, sort(unique(week_idx))))
# df_data <- df_data %>% arrange(week_idx, FID)
# # df_data <- df_data %>% arrange(FID, week_idx)
# 
# 
# formula1 <- NTw ~ 1 + 
#   f(FID, model = "iid", # FID, random effect in time months
#     group = week_idx,
#     control.group=list(model='ar1'),
#     constr = FALSE) +
#   f(country_ID, model="iid", constr=TRUE)+
#   f(calendar_week, model="rw1", constr=TRUE)+
#   f(SPIBins, model="rw1",constr=TRUE)+
#   f(airqualityBins, model="rw1",constr=TRUE)+
#   f(SolarradiationBins, model="rw1", constr=TRUE)+
#   f(Cloud_fractionBins, model="rw1", constr=TRUE)+
#   f(beta, model="rw1", constr=TRUE)
# 
# res0 <- inla(formula1, family = "poisson", data = df_data, E = df_data$E,
#             control.predictor = list(compute = TRUE),
#             control.compute = list(config = TRUE),
#             control.inla = list(int.strategy = "eb", control.vb = list(f.enable.limit = 2)),
#             verbose = FALSE)
# res <- res0





model_elements <- c(names(res$summary.random), rownames(res$summary.fixed))
model_elements

data_results <- df_data





#---Summarise relative risk
data_results <- bind_cols(data_results,
                          tibble(rel_risk.mean = res$summary.fitted.values$mean,
                                 rel_risk.lower_bound = res$summary.fitted.values$`0.025quant`,
                                 rel_risk.upper_bound = res$summary.fitted.values$`0.975quant`,
                                 rel_risk.st_dev = res$summary.fitted.values$sd,
                                 rel_risk.is_significant =
                                   sign(rel_risk.lower_bound-1) == sign(rel_risk.upper_bound-1))) # is_significant checks if the lower_bound and upper_bound are either both above 1 or both below 1

#data_results <- data_results %>% fil

# Number of data points with significant relative risk
data_results$rel_risk.is_significant %>% table()

#---Relative risk, average over weeks and countries
data_summ <- data_results %>% group_by(week_idx, country) %>%
  summarise(rel_risk.mean.mean_over_regions_per_country=mean(rel_risk.mean))

library(ggplot2)

plot_scatter_lines(data_summ,
                   "week_idx", "rel_risk.mean.mean_over_regions_per_country", "country")

p <- plot_scatter_lines(data_summ, "week_idx", "rel_risk.mean.mean_over_regions_per_country", "country")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_over_regions1.png", plot = p, width = 10, height = 6)

#---Relative risk, average over weeks
data_summ <- data_results %>% group_by(week_idx) %>% summarise(rel_risk.mean=mean(rel_risk.mean))

plot_scatter_lines(data_results %>% group_by(week_idx) %>% summarise(rel_risk.mean=mean(rel_risk.mean)),
                   "week_idx", "rel_risk.mean")

p <- plot_scatter_lines(data_results %>% group_by(week_idx) %>% summarise(rel_risk.mean=mean(rel_risk.mean)),
                    "week_idx", "rel_risk.mean")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_over_weeks1.png", plot = p, width = 10, height = 6)





#---Summarise residuals
data_results <- data_results %>% mutate(data_ratio = ifelse(TTw > 0, NTw / TTw, 1),
                                        data_rel_ratio = ifelse(TTw > 0, NTw / E_input, 1),
                                        data_raw_residual = NTw - E_input,
                                        data_pearson_residual = ifelse(TTw > 0, data_raw_residual / sqrt(E_input), 0),
                                        predicted_mean = E_input * rel_risk.mean,
                                        # predicted_median = TTw*rel_risk.median,
                                        raw_residual = NTw - predicted_mean,
                                        pearson_residual = ifelse(TTw > 0, raw_residual / sqrt(predicted_mean), 0))
library(dplyr)

#data_resulst <- data_results %>%
#  filter(pearson_residual <= 4.5 & pearson_residual >= -4.5)

write.csv(data_results, "filtered_residuals.csv", row.names = FALSE)
#---Re-arrange FID and week_idx so the index starts from zero
data_results <- data_results %>% mutate(FID = match(FID, sort(unique(FID))),
                              week_idx = match(week_idx, sort(unique(week_idx))))
#---Arrange df in the order week_idx and FID
data_results <- data_results %>% arrange(week_idx, FID)



# write.csv(data_results, file = "data_results.csv", row.names = FALSE)
#---Residuals and TTw, average over weeks and countries
data_summ <- data_results %>% group_by(week_idx, country) %>%
  summarise(data_pearson_residual.mean_per_week_per_country=mean(data_pearson_residual),
            pearson_residual.mean_per_week_per_country=mean(pearson_residual),
            TTw.sum_per_week_per_country=sum(TTw))

#---Plot data residuals per week and per country (all in one plot)
plot_scatter_lines(data_summ,
                   "week_idx", "data_pearson_residual.mean_per_week_per_country", "country")

plot_scatter_lines(data_summ,
                    "week_idx", "data_pearson_residual.mean_per_week_per_country", "country")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/residuals_per_week_per_country1.png", plot = p, width = 10, height = 6)





#---Plot Pearson residuals per week and per country (all in one plot)
plot_scatter_lines(data_summ,
                   "week_idx", "pearson_residual.mean_per_week_per_country", "country")

p <- plot_scatter_lines(data_summ,
                    "week_idx", "pearson_residual.mean_per_week_per_country", "country")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/pearson_residuals_per_week_per_country.png", plot = p, width = 10, height = 6)





#---Plot all fitted residuals (each country in its own plot)
plot_scatter_lines(data_results, "week_idx", "pearson_residual", "country", "country")


ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/residuals_per_week_per_country_individual_countries.png", plot = p, width = 10, height = 6)


#---Plot all fitted residuals (colored by country)
p <- plot_scatter_lines(data_results, "week_idx", "pearson_residual", "country")

ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/fitted_residuals_per_week_per_country.png", plot = p, width = 10, height = 6)


#---Plot all fitted residuals (colored by country, each country in its own plot)
p <- plot_scatter_lines(data_results, "week_idx", "pearson_residual", "country", "country")
p

ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/fited_residuals_per_week_per_country.png", plot = p, width = 10, height = 6)




#---Plot TTw per week and per country (each country in its own plot)
plot_scatter_lines(data_summ,
                   "week_idx", "TTw.sum_per_week_per_country", "country", "country")

p <- plot_scatter_lines(data_summ,
                    "week_idx", "TTw.sum_per_week_per_country", "country", "country")
# 
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/total_tweets_per_week_per_country1.png", plot = p, width = 10, height = 6)

#---Residuals, average over weeks
data_summ <- data_results %>% group_by(week_idx) %>%
  summarise(rel_risk.mean_per_week=mean(rel_risk.mean),
            TTw.sum_per_week=sum(TTw))


#########################################################################
# Plot random effects #
#########################################################################

#---Function for extracting the relevant columns from the INLA result
random_effect_tb <- function(res, df, col, add_time=FALSE, time_var="time_idx") {
  #---Check that col is a valid column
  if (!col %in% names(res$summary.random)) {
    cat("The column <", col, "> is not part of the model\n")
    cat("Possible choices are:", names(res$summary.random), "\n")
  }

  #---Extract random effect for column col
  re_tb <- tibble(!!sym(col) := res$summary.random[[col]]$ID,
                  !!paste0(col, ".mean") := res$summary.random[[col]]$mean,
                  !!paste0(col, ".lower_bound") := res$summary.random[[col]]$`0.025quant`,
                  !!paste0(col, ".upper_bound") := res$summary.random[[col]]$`0.975quant`,
                  !!paste0(col, ".is_significant") :=
                    sign(!!sym(paste0(col, ".lower_bound")))==sign(!!sym(paste0(col, ".upper_bound"))) &
                    (!!sym(paste0(col, ".lower_bound")) != 0 | !!sym(paste0(col, ".upper_bound")) != 0))
  
  if (add_time) {
    if (nrow(re_tb) == nrow(df)) {
      re_tb <- bind_cols(re_tb, df %>% select(time_idx, week_idx))
    } else {
      # re_tb <- bind_cols(re_tb, df %>% select(sym(col), sym(time_var)) %>% distinct() %>%
      #                       arrange(!!sym(time_var), !!sym(col)) %>% select(sym(time_var)))

      tb_with_time <- crossing(df %>% select(sym(time_var)) %>% distinct(),
                               df %>% select(sym(col)) %>% distinct())
      if (!identical(as.numeric(re_tb[[col]]), as.numeric(tb_with_time[[col]]))) {
        stop("col error")
      }
      re_tb <- bind_cols(re_tb, tb_with_time %>% select(sym(time_var)))
    }
  }

  return(re_tb)
}



res$.args$formula

#---Extract INLA fit for random effect FID
#---Adding time variable month_idx, as in the formula f(FID, model="iid", group=month_idx, ...)
re_tb_FID <- random_effect_tb(res, df_data, "FID", add_time=TRUE, time_var="week_idx")

#---Average random effect FID over each month
re_tb_summ <- re_tb_FID %>% group_by(week_idx) %>%
  summarise(FID.mean.by_week=mean(FID.mean),
           FID.lower_bound.by_week=mean(FID.lower_bound),
            FID.upper_bound.by_week=mean(FID.upper_bound))

plot_scatter_lines(re_tb_FID %>% filter(FID==1), "week_idx", "FID.mean", "FID")
plot_scatter_lines(re_tb_FID %>% filter(FID==max(FID)), "week_idx", "FID.mean", "FID")

plot_scatter_lines(re_tb_summ, "week_idx", "FID.mean.by_week")

p <-  plot_scatter_lines(re_tb_summ, "week_idx", "FID.mean.by_week")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect_month1.png", plot = p, width = 10, height = 6)



#---Extract INLA fit for random effect calendar_week
#---no time variable for calendaar week as in the, as in the formula f(calendar_week, model="rw1",, ...)
re_tb_calendar <- random_effect_tb(res, df_data, "calendar_week", add_time=FALSE)

re_tb_calendar$calendar_week <- as.numeric(re_tb_calendar$calendar_week)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_calendar %>% select(x=calendar_week, y=calendar_week.mean), 
                               type="mean"),
                        tibble(re_tb_calendar %>% select(x=calendar_week, y=calendar_week.lower_bound), 
                               type="lower bound"),
                        tibble(re_tb_calendar %>% select(x=calendar_week, y=calendar_week.upper_bound), 
                               type="upper_bound")) %>% 
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="calendar_week",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
 
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="calendar_week",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal) 
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__calendar_week.png", plot = p, width = 10, height = 6)

#---Extract INLA fit for random effect calendar_week
#---no time variable for calendaar week as in the, as in the formula f(calendar_week, model="rw1",, ...)
 #re_tb_an <- random_effect_tb(res, df_data, "anbins", add_time=FALSE)
 #
 #re_tb_an$anbins <- as.numeric(re_tb_an$anbins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 #re_tb_long <- bind_rows(tibble(re_tb_an %>% select(x=anbins, y=anbins.mean),
             #                   type="mean"),
           #              tibble(re_tb_an %>% select(x=anbins, y=anbins.lower_bound),
         #                       type="lower bound"),
       #                  tibble(re_tb_an %>% select(x=anbins, y=anbins.upper_bound),
     #                           type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
   #                                     reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="anbins",
     #               plot_constant_lines_y = 0,
   #                 manual_palette=col_pal)

 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="anbins",
     #               plot_constant_lines_y = 0,
   #                 manual_palette=col_pal)

 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__an.png", plot = p, width = 10, height = 6)

#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
#re_tb_avg_relR0_Bins <- random_effect_tb(res, df_data, "avg_relR0_Bins", add_time=FALSE)

#re_tb_avg_relR0_Bins$avg_relR0_Bins <- as.numeric(re_tb_avg_relR0_Bins$avg_relR0_Bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_avg_relR0_Bins %>% select(x=avg_relR0_Bins, y=avg_relR0_Bins.mean),
 #                              type="mean"),
  #                      tibble(re_tb_avg_relR0_Bins %>% select(x=avg_relR0_Bins, y=avg_relR0_Bins.lower_bound),
   #                            type="lower bound"),
    #                    tibble(re_tb_avg_relR0_Bins %>% select(x=avg_relR0_Bins, y=avg_relR0_Bins.upper_bound),
     #                          type="upper_bound")) %>%
  #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
#col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #                                      reorder=c(1,3,2))

#plot_scatter_lines(re_tb_long, "x", "y", "type", title="avg_relR0_Bins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)

#p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="avg_relR0_Bins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)


#ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__avg_relR0_Bins.png", plot = p, width = 10, height = 6)

#re_tb_Wild_fire_RP_bin_Bins <- random_effect_tb(res, df_data, "Wild_fire_RP_bins", add_time=FALSE)

 #re_tb_Wild_fire_RP_bin_Bins$Wild_fire_RP_bins <- as.numeric(re_tb_Wild_fire_RP_bin_Bins$Wild_fire_RP_bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 #re_tb_long <- bind_rows(tibble(re_tb_Wild_fire_RP_bin_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.mean),
   #                             type="mean"),
     #                    tibble(re_tb_Wild_fire_RP_bin_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.lower_bound),
       #                         type="lower bound"),
         #                tibble(re_tb_Wild_fire_RP_bin_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.upper_bound),
           #                     type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #				       reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_RP_bins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)
 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_RP_bins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)


 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect_wild_fire.png", plot = p, width = 10, height = 6)

#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
# re_tb_PrecipBins <- random_effect_tb(res, df_data, "PrecipBins", add_time=FALSE)

 #re_tb_PrecipBins$PrecipBins <- as.numeric(re_tb_PrecipBins$PrecipBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
# re_tb_long <- bind_rows(tibble(re_tb_PrecipBins %>% select(x=PrecipBins, y=PrecipBins.mean),
 #                               type="mean"),
  #                       tibble(re_tb_PrecipBins %>% select(x=PrecipBins, y=PrecipBins.lower_bound),
   #                             type="lower bound"),
    #                     tibble(re_tb_PrecipBins %>% select(x=PrecipBins, y=PrecipBins.upper_bound),
     #                           type="upper_bound")) %>%
    #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
  #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
   #                                     reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="PrecipBins",
  #                   plot_constant_lines_y = 0,
   #                 manual_palette=col_pal)

  #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="PrecipBins",
   #                  plot_constant_lines_y = 0,
    #                manual_palette=col_pal)


  #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__Precipbins.png", plot = p, width = 10, height = 6)

re_tb_country <- random_effect_tb(res, df_data, "country_ID", add_time=FALSE)

re_tb_country$country_ID <- as.numeric(re_tb_country$country_ID)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_country %>% select(x=country_ID, y=country_ID.mean),
                               type="mean"),
                        tibble(re_tb_country %>% select(x=country_ID, y=country_ID.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_country %>% select(x=country_ID, y=country_ID.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="country_ID",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)

p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="country_ID",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__country1.png", plot = p, width = 10, height = 6)


#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
 re_tb_SPI_Bins <- random_effect_tb(res, df_data, "SPIBins", add_time=FALSE)

 re_tb_SPI_Bins$SPIBins <- as.numeric(re_tb_SPI_Bins$SPIBins)

 #---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 re_tb_long <- bind_rows(tibble(re_tb_SPI_Bins %>% select(x=SPIBins, y=SPIBins.mean),
                                type="mean"),
                         tibble(re_tb_SPI_Bins %>% select(x=SPIBins, y=SPIBins.lower_bound),
                                type="lower bound"),
                         tibble(re_tb_SPI_Bins %>% select(x=SPIBins, y=SPIBins.upper_bound),
                                type="upper_bound")) %>%
   mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                        reorder=c(1,3,2))

 plot_scatter_lines(re_tb_long, "x", "y", "type", title="SPIBins",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
 p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="SPIBins",
                    plot_constant_lines_y = 0,
                     manual_palette=col_pal)
 ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__SPI.png", plot = p, width = 10, height = 6)

#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
#re_tb_SPI_Bins <- random_effect_tb(res, df_data, "group1", add_time=FALSE)

#re_tb_SPI_Bins$group1 <- as.numeric(re_tb_SPI_Bins$group1)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_SPI_Bins %>% select(x=group1, y=group1.mean),
      #                         type="mean"),
     #                   tibble(re_tb_SPI_Bins %>% select(x=group1, y=group1.lower_bound),
    #                           type="lower bound"),
   #                     tibble(re_tb_SPI_Bins %>% select(x=group1, y=group1.upper_bound),
  #                             type="upper_bound")) %>%
 # mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
#col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #                                      reorder=c(1,3,2))

#plot_scatter_lines(re_tb_long, "x", "y", "type", title="group1",
  #                 plot_constant_lines_y = 0,
   #                manual_palette=col_pal)

#p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="group1",
    #               plot_constant_lines_y = 0,
     #              manual_palette=col_pal)


#ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__group1.png", plot = p, width = 10, height = 6)
#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
#re_tb_TempmeanBins <- random_effect_tb(res, df_data, "TempmeanBins", add_time=FALSE)

#re_tb_TempmeanBins$TempmeanBins <- as.numeric(re_tb_TempmeanBins$TempmeanBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_TempmeanBins %>% select(x=TempmeanBins, y=TempmeanBins.mean),
 #                               type="mean"),
  #                        tibble(re_tb_TempmeanBins %>% select(x=TempmeanBins, y=TempmeanBins.lower_bound),
   #                              type="lower bound"),
    #                      tibble(re_tb_TempmeanBins %>% select(x=TempmeanBins, y=TempmeanBins.upper_bound),
     #                            type="upper_bound")) %>%
    #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
  #                                       reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempmeanBins",
  #                   plot_constant_lines_y = 0,
   #                  manual_palette=col_pal)

 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempmeanBins",
  #                   plot_constant_lines_y = 0,
   #                  manual_palette=col_pal)


 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__TempmeanBins.png", plot = p, width = 10, height = 6)
#re_tb_TempminBins <- random_effect_tb(res, df_data, "TempminBins", add_time=FALSE)

#re_tb_TempminBins$TempminBins <- as.numeric(re_tb_TempminBins$TempminBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_TempminBins %>% select(x=TempminBins, y=TempminBins.mean),
 #                                type="mean"),
  #                        tibble(re_tb_TempminBins %>% select(x=TempminBins, y=TempminBins.lower_bound),
   #                              type="lower bound"),
    #                      tibble(re_tb_TempminBins %>% select(x=TempminBins, y=TempminBins.upper_bound),
     #                            type="upper_bound")) %>%
    #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
  #                                       reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempminBins",
  #                   plot_constant_lines_y = 0,
   #                  manual_palette=col_pal)

 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempminBins",
  #                   plot_constant_lines_y = 0,
   #                  manual_palette=col_pal)


 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__TempminBins.png", plot = p, width = 10, height = 6)


re_tb_TempBins <- random_effect_tb(res, df_data, "TempBins", add_time=FALSE)

re_tb_TempBins$TempBins <- as.numeric(re_tb_TempBins$TempBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_TempBins %>% select(x=TempBins, y=TempBins.mean),
                                 type="mean"),
                          tibble(re_tb_TempBins %>% select(x=TempBins, y=TempBins.lower_bound),
                                 type="lower bound"),
                          tibble(re_tb_TempBins %>% select(x=TempBins, y=TempBins.upper_bound),
                                 type="upper_bound")) %>%
    mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
                                         reorder=c(1,3,2))

 plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempBins",
                     plot_constant_lines_y = 0,
                     manual_palette=col_pal)

 p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="TempBins",
                     plot_constant_lines_y = 0,
                     manual_palette=col_pal)


 ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__TempBins.png", plot = p, width = 10, height = 6)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below


#---Extract INLA fit for random effect SPI_Bins
#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
 re_tb_airquality_Bins <- random_effect_tb(res, df_data, "airqualityBins", add_time=FALSE)

 re_tb_airquality_Bins$airqualityBins <- as.numeric(re_tb_airquality_Bins$airqualityBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 re_tb_long <- bind_rows(tibble(re_tb_airquality_Bins %>% select(x=airqualityBins, y=airqualityBins.mean),
                                type="mean"),
                         tibble(re_tb_airquality_Bins %>% select(x=airqualityBins, y=airqualityBins.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_airquality_Bins %>% select(x=airqualityBins, y=airqualityBins.upper_bound),
                                type="upper_bound")) %>%
   mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                        reorder=c(1,3,2))

 plot_scatter_lines(re_tb_long, "x", "y", "type", title="airqualityBins",
       		    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
  p<- plot_scatter_lines(re_tb_long, "x", "y", "type", title="airqualityBins",
                     plot_constant_lines_y = 0,
                     manual_palette=col_pal)
 ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__airqualitybins.png", plot = p, width = 10, height = 6)

 #re_tb_NAOBins <- random_effect_tb(res, df_data, "NAOBins", add_time=FALSE)

 #re_tb_NAOBins$NAOBins <- as.numeric(re_tb_NAOBins$NAOBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 #re_tb_long <- bind_rows(tibble(re_tb_NAOBins %>% select(x=NAOBins, y=NAOBins.mean),
   #                             type="mean"),
     #                    tibble(re_tb_NAOBins %>% select(x=NAOBins, y=NAOBins.lower_bound),
       #                         type="lower bound"),
         #                tibble(re_tb_NAOBins %>% select(x=NAOBins, y=NAOBins.upper_bound),
           #                     type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
   #                                     reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="NAOBins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)

 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="NAOBins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)


 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__NAOBins.png", plot = p, width = 10, height = 6)


#re_tb_solarradiation_Bins <- random_effect_tb(res, df_data, "SolarradiationBins", add_time=FALSE)

#re_tb_solarradiation_Bins$SolarradiationBins <- as.numeric(re_tb_solarradiation_Bins$SolarradiationBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_solarradiation_Bins %>% select(x=SolarradiationBins, y=SolarradiationBins.mean),
 #                               type="mean"),
  #                       tibble(re_tb_solarradiation_Bins %>% select(x=SolarradiationBins, y=SolarradiationBins.lower_bound),
   #                             type="lower bound"),
    #                     tibble(re_tb_solarradiation_Bins %>% select(x=SolarradiationBins, y=SolarradiationBins.upper_bound),
     #                           type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
  #                                      reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="SolarradiationBins",
  #                  plot_constant_lines_y = 0,
   #                 manual_palette=col_pal)

  #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="SolarradiationBins",
   #                  plot_constant_lines_y = 0,
    #                 manual_palette=col_pal)
  #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__SolarradiationBins.png", plot = p, width = 10, height = 6)




 re_tb_Wind_speedBins_Bins <- random_effect_tb(res, df_data, "Wind_speedBins", add_time=FALSE)

 re_tb_Wind_speedBins_Bins$Wind_speedBins <- as.numeric(re_tb_Wind_speedBins_Bins$Wind_speedBins)

 #---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 re_tb_long <- bind_rows(tibble(re_tb_Wind_speedBins_Bins %>% select(x=Wind_speedBins, y=Wind_speedBins.mean),
                                type="mean"),
                         tibble(re_tb_Wind_speedBins_Bins %>% select(x=Wind_speedBins, y=Wind_speedBins.lower_bound),
                               type="lower bound"),
                         tibble(re_tb_Wind_speedBins_Bins %>% select(x=Wind_speedBins, y=Wind_speedBins.upper_bound),
                                type="upper_bound")) %>%
   mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
                                        reorder=c(1,3,2))

 plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wind_speedBins",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
 p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wind_speedBins",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)


 ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__Wind_speedBins.png", plot = p, width = 10, height = 6)



 #re_tb_Wind_directionBins_Bins <- random_effect_tb(res, df_data, "Wind_directionBins", add_time=FALSE)

 #re_tb_Wind_directionBins_Bins$Wind_directionBins <- as.numeric(re_tb_Wind_directionBins_Bins$Wind_directionBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 #re_tb_long <- bind_rows(tibble(re_tb_Wind_directionBins_Bins %>% select(x=Wind_directionBins, y=Wind_directionBins.mean),
   #                             type="mean"),
     #                    tibble(re_tb_Wind_directionBins_Bins %>% select(x=Wind_directionBins, y=Wind_directionBins.lower_bound),
       #                         type="lower bound"),
         #                tibble(re_tb_Wind_directionBins_Bins %>% select(x=Wind_directionBins, y=Wind_directionBins.upper_bound),
           #                     type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
   #                                     reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wind_directionBins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)
 #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wind_directionBins",
   #                 plot_constant_lines_y = 0,
     #               manual_palette=col_pal)


 #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__Wind_directionBins.png", plot = p, width = 10, height = 6)



#re_tb_Relative_humidityBins_Bins <- random_effect_tb(res, df_data, "Relative_humidityBins", add_time=FALSE)

#re_tb_Relative_humidityBins_Bins$Relative_humidityBins <- as.numeric(re_tb_Relative_humidityBins_Bins$Relative_humidityBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_Relative_humidityBins_Bins %>% select(x=Relative_humidityBins, y=Relative_humidityBins.mean),
 #                              type="mean"),
  #                      tibble(re_tb_Relative_humidityBins_Bins %>% select(x=Relative_humidityBins, y=Relative_humidityBins.lower_bound),
   #                            type="lower bound"),
    #                    tibble(re_tb_Relative_humidityBins_Bins %>% select(x=Relative_humidityBins, y=Relative_humidityBins.upper_bound),
     #                          type="upper_bound")) %>%
  #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
#col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #                                      reorder=c(1,3,2))

#plot_scatter_lines(re_tb_long, "x", "y", "type", title="Relative_humidityBins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)
#p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Relative_humidityBins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)


#ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__relativehumidityBins.png", plot = p, width = 10, height = 6)

 #re_tb_Cloud_fractionBins_Bins <- random_effect_tb(res, df_data, "Cloud_fractionBins", add_time=FALSE)

 #re_tb_Cloud_fractionBins_Bins$Cloud_fractionBins <- as.numeric(re_tb_Cloud_fractionBins_Bins$Cloud_fractionBins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
 #re_tb_long <- bind_rows(tibble(re_tb_Cloud_fractionBins_Bins %>% select(x=Cloud_fractionBins, y=Cloud_fractionBins.mean),
  #                              type="mean"),
   #                      tibble(re_tb_Cloud_fractionBins_Bins %>% select(x=Cloud_fractionBins, y=Cloud_fractionBins.lower_bound),
    #                            type="lower bound"),
     #                    tibble(re_tb_Cloud_fractionBins_Bins %>% select(x=Cloud_fractionBins, y=Cloud_fractionBins.upper_bound),
      #                          type="upper_bound")) %>%
   #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
 #col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
  #                                      reorder=c(1,3,2))

 #plot_scatter_lines(re_tb_long, "x", "y", "type", title="Cloud_fractionBins",
  #                  plot_constant_lines_y = 0,
   #                 manual_palette=col_pal)
  #p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Cloud_fractionBins",
   #                  plot_constant_lines_y = 0,
    #                 manual_palette=col_pal)
  #ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__cloudfractionBins.png", plot = p, width = 10, height = 6)



#re_tb_Wild_fire_PM2_5_binned_Bins <- random_effect_tb(res, df_data, "Wild_fire_PM2_5_binned", add_time=FALSE)

#re_tb_Wild_fire_PM2_5_binned_Bins$Wild_fire_PM2_5_binned <- as.numeric(re_tb_Wild_fire_PM2_5_binned_Bins$Wild_fire_PM2_5_binned)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_Wild_fire_PM2_5_binned_Bins %>% select(x=Wild_fire_PM2_5_binned, y=Wild_fire_PM2_5_binned.mean),
 #                              type="mean"),
  #                      tibble(re_tb_Wild_fire_PM2_5_binned_Bins %>% select(x=Wild_fire_PM2_5_binned, y=Wild_fire_PM2_5_binned.lower_bound),
   #                            type="lower bound"),
    #                    tibble(re_tb_Wild_fire_PM2_5_binned_Bins %>% select(x=Wild_fire_PM2_5_binned, y=Wild_fire_PM2_5_binned.upper_bound),
     #                          type="upper_bound")) %>%
  #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
#col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #                                      reorder=c(1,3,2))

#plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_PM2_5_binned",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)
#p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_PM2_5_binned",
 #                  plot_constant_lines_y = 0,
 #                  manual_palette=col_pal)


#ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__wildfire.png", plot = p, width = 10, height = 6)


re_tb_pollen_alder_Bins_Bins <- random_effect_tb(res, df_data, "Pollen_alder_bins", add_time=FALSE)

re_tb_pollen_alder_Bins_Bins$Pollen_alder_bins <- as.numeric(re_tb_pollen_alder_Bins_Bins$Pollen_alder_bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_pollen_alder_Bins_Bins %>% select(x=Pollen_alder_bins, y=Pollen_alder_bins.mean),
                               type="mean"),
                        tibble(re_tb_pollen_alder_Bins_Bins %>% select(x=Pollen_alder_bins, y=Pollen_alder_bins.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_pollen_alder_Bins_Bins %>% select(x=Pollen_alder_bins, y=Pollen_alder_bins.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines
#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="Pollen_alder_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Pollen_alder_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)


ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect_Pollen.alder_bins.png", plot = p, width = 10, height = 6)
re_tb_pollen_oliver_Bins_Bins <- random_effect_tb(res, df_data, "Pollen_oliver_bins", add_time=FALSE)

re_tb_pollen_oliver_Bins_Bins$Pollen_oliver_bins <- as.numeric(re_tb_pollen_oliver_Bins_Bins$Pollen_oliver_bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_pollen_oliver_Bins_Bins %>% select(x=Pollen_oliver_bins, y=Pollen_oliver_bins.mean),
                               type="mean"),
                        tibble(re_tb_pollen_oliver_Bins_Bins %>% select(x=Pollen_oliver_bins, y=Pollen_oliver_bins.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_pollen_oliver_Bins_Bins %>% select(x=Pollen_oliver_bins, y=Pollen_oliver_bins.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="Pollen_oliver_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Pollen_oliver_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)


ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect_Pollen.oliver_bins.png", plot = p, width = 10, height = 6)

re_tb_pollen_birch_Bins_Bins <- random_effect_tb(res, df_data, "Pollen_birch_bins", add_time=FALSE)

re_tb_pollen_birch_Bins_Bins$Pollen_birch_bins <- as.numeric(re_tb_pollen_birch_Bins_Bins$Pollen_birch_bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_pollen_birch_Bins_Bins %>% select(x=Pollen_birch_bins, y=Pollen_birch_bins.mean),
                               type="mean"),
                        tibble(re_tb_pollen_birch_Bins_Bins %>% select(x=Pollen_birch_bins, y=Pollen_birch_bins.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_pollen_birch_Bins_Bins %>% select(x=Pollen_birch_bins, y=Pollen_birch_bins.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))
plot_scatter_lines(re_tb_long, "x", "y", "type", title="pollen_birch_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="pollen_birch_bins",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)


ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect_Pollen.birch_bins.png", plot = p, width = 10, height = 6)


#re_tb_Wild_fire_RP_bins_Bins <- random_effect_tb(res, df_data, "Wild_fire_RP_bins", add_time=FALSE)

#re_tb_Wild_fire_RP_bins_Bins$Wild_fire_RP_bins <- as.numeric(re_tb_Wild_fire_RP_bins_Bins$Wild_fire_RP_bins)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
#re_tb_long <- bind_rows(tibble(re_tb_Wild_fire_RP_bins_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.mean),
 #                              type="mean"),
  #                      tibble(re_tb_Wild_fire_RP_bins_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.lower_bound),
   #                            type="lower bound"),
    #                    tibble(re_tb_Wild_fire_RP_bins_Bins %>% select(x=Wild_fire_RP_bins, y=Wild_fire_RP_bins.upper_bound),
     #                          type="upper_bound")) %>%
  #mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
#col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
 #                                      reorder=c(1,3,2))

#plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_RP_bins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)
#p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="Wild_fire_RP_bins",
 #                  plot_constant_lines_y = 0,
  #                 manual_palette=col_pal)


#ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect_wildfire_bins.png", plot = p, width = 10, height = 6)



re_tb_an_Bins <- random_effect_tb(res, df_data, "an_bin", add_time=FALSE)

re_tb_an_Bins$an_bin <- as.numeric(re_tb_an_Bins$an_bin)

 #---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_an_Bins %>% select(x=an_bin, y=an_bin.mean),
                               type="mean"),
                        tibble(re_tb_an_Bins %>% select(x=an_bin, y=an_bin.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_an_Bins %>% select(x=an_bin, y=an_bin.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14,n_discrete=3,
				       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="an_bin",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="an_bin",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__an_bin.png", plot = p, width = 10, height = 6)


re_tb_wnv_Bins <- random_effect_tb(res, df_data, "TotalCases_bin", add_time=FALSE)

re_tb_wnv_Bins$TotalCases_bin <- as.numeric(re_tb_wnv_Bins$TotalCases_bin)

 #---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_wnv_Bins %>% select(x=TotalCases_bin, y=TotalCases_bin.mean),
                               type="mean"),
                        tibble(re_tb_wnv_Bins %>% select(x=TotalCases_bin, y=TotalCases_bin.lower_bound),
                               type="lower bound"),
                        tibble(re_tb_wnv_Bins %>% select(x=TotalCases_bin, y=TotalCases_bin.upper_bound),
                               type="upper_bound")) %>%
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines

#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3,
				       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="TotalCases_bin",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="TotalCases_bin",
                    plot_constant_lines_y = 0,
                    manual_palette=col_pal)
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__TotalCases_bin.png", plot = p, width = 10, height = 6)


#---No time variable for SPI_Bins, as in the formula f(SPI_Bins, model="rw1", ...)
re_tb_beta <- random_effect_tb(res, data_results, "beta", add_time=FALSE)

#---Make a long tibble, with joint "y"-column for the "mean", "lower bound" and "upper bound"-columns
#---Necessary to make the plot below
re_tb_long <- bind_rows(tibble(re_tb_beta %>% select(x=beta, y=beta.mean), 
                               type="mean"),
                        tibble(re_tb_beta %>% select(x=beta, y=beta.lower_bound), 
                               type="lower bound"),
                        tibble(re_tb_beta %>% select(x=beta, y=beta.upper_bound), 
                               type="upper_bound")) %>% 
  mutate(type = as.factor(type)) #need to make type a factor to get the dots in the plot to be connected by lines



#---Plot of the random effect
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, 
                                       reorder=c(1,3,2))

plot_scatter_lines(re_tb_long, "x", "y", "type", title="beta",
                   plot_constant_lines_y = 0,
                   manual_palette=col_pal)
# p <- plot_scatter_lines(re_tb_long, "x", "y", "type", title="beta",
#                    plot_constant_lines_y = 0,
#                    manual_palette=col_pal)
# ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__beta.png", plot = p, width = 10, height = 6)

data_results$beta %>% table()






# Plot beta by week ##########################################################

#---Process results related to beta
re_tb_beta <- random_effect_tb(res, data_results, "beta", add_time=FALSE)

data_summ <- data_results %>% 
  left_join(re_tb_beta %>% select(beta, beta.fitted=beta.mean)) %>% 
  group_by(week_idx, country) %>% 
  summarise(data_pearson_residual=mean(data_pearson_residual),
            beta=first(beta),
            beta.fitted=first(beta.fitted),
            rel_risk.fitted=mean(rel_risk.mean))


#---Standardize by dividing with max value (helpful for plotting in the same plot)
data_summ <- data_summ %>% ungroup() %>% 
  mutate(beta = beta / max(beta),
         data_pearson_residual = data_pearson_residual / max(data_pearson_residual),
         rel_risk.fitted = rel_risk.fitted / max(rel_risk.fitted),
         beta.fitted = beta.fitted / max(beta.fitted),
         beta.original = beta / max(beta))

#---Collect results in format for plotting
re_tb_beta_with_time <- bind_rows(tibble(data_summ %>% select(x=week_idx, 
                                                              y=data_pearson_residual),
                                         type="data_pearson_residual"),
                                  tibble(data_summ %>% select(x=week_idx, 
                                                              y=beta),
                                         type="beta.original"),
                                  tibble(data_summ %>% select(x=week_idx,
                                                              y=beta.fitted),
                                         type="beta.fitted"),
                                  tibble(data_summ %>% select(x=week_idx,
                                                              y=rel_risk.fitted),
                                         type="rel_risk.fitted"))



#---Plot beta covariate and the data_pearson_residual (was used to create beta)
plot_scatter_lines(re_tb_beta_with_time %>% 
                         filter(type %in% c("beta.original", "data_pearson_residual")), 
                       "x", "y", "type")
# p <-plot_scatter_lines(re_tb_beta_with_time %>% 
#                      filter(type %in% c("beta.original", "data_pearson_residual")), 
#                    "x", "y", "betaoriginal")
# ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__betaoriginal.png", plot = p, width = 10, height = 6)

#---Plot fitted random effect for beta and the fitted rel_risk
p <-plot_scatter_lines(re_tb_beta_with_time %>% 
                     filter(type %in% c("beta.fitted", "rel_risk.fitted")), 
                   "x", "y", "type")
p
# ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/Spain-model/scatter_lines_random_effect__betafitted.png", plot = p, width = 10, height = 6)

#---Plot beta against fitted random effect for beta
p <-plot_scatter_lines(re_tb_beta_with_time %>% 
                         filter(type %in% c("beta.original", "beta.fitted")), 
                       "x", "y", "type")
p







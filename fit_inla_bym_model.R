# ===========================
# Setup
# ===========================
source("functions_plotting.R")
source("functions.R")

packages <- c("readxl","tidyr","dplyr","rgdal","sf","ggplot2","scales","spdep")  ## NEW: spdep
lapply(packages, library, character.only = TRUE)

# ===========================
# Load data
# ===========================
df <- read.csv('all_countries_lasso_bins_NA_an_cdc_final.csv')
library(dplyr)

df$week_date <- as.Date(df$week_date)

# Renumber indices to be consecutive starting at 1 (as you had)
df <- df %>% mutate(
  week_idx  = match(week_idx,  sort(unique(week_idx))),
  month_idx = match(month_idx, sort(unique(month_idx))),
  country_ID = match(country_ID, sort(unique(country_ID)))
)

# Keep columns and order
df <- df %>% select(
  FID, week_idx, country, NUTS2, month_idx, time_idx, country_ID, calendar_week,
  TTw, NTw,
  airqualityBins, SolarradiationBins, Cloud_fractionBins, SPIBins,
  Pollen_birch_bins, Pollen_alder_bins, Pollen_oliver_bins,
  PrecipBins, TempBins, NAOBins, Wind_speedBins, Wind_directionBins,
  Relative_humidityBins, TempmeanBins, TempminBins,
  an_bin, TotalCases_bin,
  beta,beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9,beta_10
) %>% arrange(week_idx, FID, country_ID)

# ===========================
# Expected counts (offset)
# ===========================
T3 <- sum(df$NTw, na.rm = TRUE) / sum(df$TTw[!is.na(df$NTw)])
df$E <- df$TTw * T3
df$FID_2 <- df$FID

# ===========================
# --- NEW: Build NUTS2 adjacency & spatial index for BYM2
# ===========================
df$NUTS2 <- as.character(df$NUTS2)   ## ensure character

# Your path and file (from your message)
nuts_path <- "/export/data/talahdal/NUTS_RG_20M_2006_4326.shp/NUTS_RG_20M_2006_4326.shp"

nuts_sf <- sf::st_read(nuts_path, quiet = TRUE)

# auto-detect level and id columns across vintages
lvl_col <- intersect(c("LEVL_CODE","STAT_LEVL","STAT_LEVL_","NIVEL"), names(nuts_sf)); stopifnot(length(lvl_col)>=1); lvl_col <- lvl_col[1]
id_col  <- intersect(c("NUTS_ID","CODE","id","ID"), names(nuts_sf));            stopifnot(length(id_col)>=1);  id_col  <- id_col[1]

# Keep NUTS-2 polygons and only those in df
nuts2_sf <- nuts_sf %>% dplyr::filter(.data[[lvl_col]] == 2)
nuts2_keep <- nuts2_sf %>%
  dplyr::filter(.data[[id_col]] %in% unique(df$NUTS2)) %>%
  dplyr::arrange(match(.data[[id_col]], sort(unique(df$NUTS2))))

# Warn if codes in df don’t exist in shapefile
unmatched <- setdiff(unique(df$NUTS2), nuts2_keep[[id_col]])
if (length(unmatched)) {
  cat("WARNING: NUTS2 in df not found in shapefile (check vintage/crosswalk):\n")
  print(head(unmatched, 20))
}

# Build neighbors and write INLA graph file in working dir
nb <- spdep::poly2nb(nuts2_keep, queen = TRUE)
spdep::nb2INLA(file = "NUTS2.adj", nb = nb)

# Spatial index that aligns with graph ordering
nuts2_levels <- nuts2_keep[[id_col]]
df$NUTS2_id <- match(df$NUTS2, nuts2_levels)
stopifnot(all(!is.na(df$NUTS2_id)))

# ===========================
# INLA model
# ===========================
library(INLA)

# Re-arrange indexing (as you did before)
df <- df %>% mutate(
  FID = match(FID, sort(unique(FID))),
  week_idx = match(week_idx, sort(unique(week_idx)))
) %>% arrange(week_idx, FID)

df$E_input <- df$E

# --- CHANGED: Replace country_ID IID with BYM2 on NUTS2_id using the graph we just built
formula1 <- NTw ~ 1 +
  f(FID, model = "iid",
    group = week_idx,
    control.group = list(model = "ar1"),
    constr = FALSE) +
  f(NUTS2_id, model = "bym2",
    graph = "NUTS2.adj",
    scale.model = TRUE,
    hyper = list(
      prec = list(prior = "pc.prec", param = c(1, 0.01)),  # P(1/sd > 1)=0.01
      phi  = list(prior = "pc", param = c(0.5, 2/3))       # P(phi < 0.5)=2/3
    )) +
  f(calendar_week,     model = "rw1", constr = TRUE) +
  f(SPIBins,           model = "rw1", constr = TRUE) +
  f(TempBins,          model = "rw1", constr = TRUE) +
  f(airqualityBins,    model = "rw1", constr = TRUE) +
  f(an_bin,            model = "rw1", constr = TRUE) +
  f(TotalCases_bin,    model = "rw1", constr = TRUE) +
  f(Wind_speedBins,    model = "rw1", constr = TRUE) +
  f(Pollen_birch_bins, model = "rw1", constr = TRUE) +
  f(Pollen_alder_bins, model = "rw1", constr = TRUE) +
  f(Pollen_oliver_bins,model = "rw1", constr = TRUE) +
  f(beta,              model = "rw1", constr = TRUE) +
  f(beta_1,            model = "rw1", constr = TRUE) +
  f(beta_2,            model = "rw1", constr = TRUE) +
  f(beta_3,            model = "rw1", constr = TRUE) +
  f(beta_4,            model = "rw1", constr = TRUE) +
  f(beta_5,            model = "rw1", constr = TRUE) +
  f(beta_6,            model = "rw1", constr = TRUE) +
  f(beta_7,            model = "rw1", constr = TRUE) +
  f(beta_8,            model = "rw1", constr = TRUE) +
  f(beta_9,            model = "rw1", constr = TRUE) +
  f(beta_10,           model = "rw1", constr = TRUE)

# Compute DIC/WAIC/CPO for model comparison
res <- inla(
  formula1, family = "poisson", data = df, E = df$E_input,
  control.predictor = list(compute = TRUE),
  control.compute   = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),  ## NEW
  control.inla      = list(int.strategy = "eb", control.vb = list(f.enable.limit = 2)),
  verbose = TRUE
)

save(res, file = "/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/SPI_model_BYM2.RData")

df_data <- res$.args$data

# Quick GOF readout (optional)
cat("\nDIC  :", res$dic$dic, "\n")
cat("WAIC :", res$waic$waic, "\n")
cat("Mean log score (CPO):", mean(-log(res$cpo$cpo), na.rm = TRUE), "\n\n")

# ===========================
# Collect fitted & residuals
# ===========================
data_results <- df_data
data_results <- dplyr::bind_cols(
  data_results,
  tibble::tibble(
    rel_risk.mean         = res$summary.fitted.values$mean,
    rel_risk.lower_bound  = res$summary.fitted.values$`0.025quant`,
    rel_risk.upper_bound  = res$summary.fitted.values$`0.975quant`,
    rel_risk.st_dev       = res$summary.fitted.values$sd,
    rel_risk.is_significant =
      sign(rel_risk.lower_bound-1) == sign(rel_risk.upper_bound-1)
  )
)

data_results <- data_results %>% mutate(
  data_ratio           = ifelse(TTw > 0, NTw / TTw, 1),
  data_rel_ratio       = ifelse(TTw > 0, NTw / E_input, 1),
  data_raw_residual    = NTw - E_input,
  data_pearson_residual= ifelse(TTw > 0, data_raw_residual / sqrt(E_input), 0),
  predicted_mean       = E_input * rel_risk.mean,
  raw_residual         = NTw - predicted_mean,
  pearson_residual     = ifelse(TTw > 0, raw_residual / sqrt(predicted_mean), 0)
)

write.csv(data_results, "filtered_residuals.csv", row.names = FALSE)

# Keep your index normalization for downstream plots
data_results <- data_results %>%
  mutate(
    FID = match(FID, sort(unique(FID))),
    week_idx = match(week_idx, sort(unique(week_idx)))
  ) %>% arrange(week_idx, FID)

# ===========================
# Helper to extract random effects (your function unchanged)
# ===========================
random_effect_tb <- function(res, df, col, add_time=FALSE, time_var="time_idx") {
  if (!col %in% names(res$summary.random)) {
    cat("The column <", col, "> is not part of the model\n")
    cat("Possible choices are:", names(res$summary.random), "\n")
  }
  re_tb <- tibble::tibble(
    !!rlang::sym(col) := res$summary.random[[col]]$ID,
    !!paste0(col, ".mean") := res$summary.random[[col]]$mean,
    !!paste0(col, ".lower_bound") := res$summary.random[[col]]$`0.025quant`,
    !!paste0(col, ".upper_bound") := res$summary.random[[col]]$`0.975quant`,
    !!paste0(col, ".is_significant") :=
      sign(!!rlang::sym(paste0(col, ".lower_bound"))) ==
      sign(!!rlang::sym(paste0(col, ".upper_bound"))) &
      (!!rlang::sym(paste0(col, ".lower_bound")) != 0 | !!rlang::sym(paste0(col, ".upper_bound")) != 0)
  )
  if (add_time) {
    if (nrow(re_tb) == nrow(df)) {
      re_tb <- dplyr::bind_cols(re_tb, df %>% dplyr::select(time_idx, week_idx))
    } else {
      tb_with_time <- tidyr::crossing(
        df %>% dplyr::select(!!rlang::sym(time_var)) %>% dplyr::distinct(),
        df %>% dplyr::select(!!rlang::sym(col)) %>% dplyr::distinct()
      )
      if (!identical(as.numeric(re_tb[[col]]), as.numeric(tb_with_time[[col]]))) {
        stop("col error")
      }
      re_tb <- dplyr::bind_cols(re_tb, tb_with_time %>% dplyr::select(!!rlang::sym(time_var)))
    }
  }
  re_tb
}

# ===========================
# Example summaries/plots (unchanged logic)
# ===========================
library(ggplot2)

# Relative risk, average over weeks & countries
data_summ <- data_results %>% group_by(week_idx, country) %>%
  summarise(rel_risk.mean.mean_over_regions_per_country = mean(rel_risk.mean), .groups="drop")
p <- plot_scatter_lines(data_summ, "week_idx", "rel_risk.mean.mean_over_regions_per_country", "country")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_over_regions1.png", plot = p, width = 10, height = 6)

# Relative risk, average over weeks
p <- plot_scatter_lines(data_results %>% group_by(week_idx) %>% summarise(rel_risk.mean=mean(rel_risk.mean), .groups="drop"),
                        "week_idx", "rel_risk.mean")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_over_weeks1.png", plot = p, width = 10, height = 6)

# ===========================
# Residual spatial autocorrelation check (NEW, optional)
# ===========================
res_by_region <- data_results %>%
  dplyr::group_by(NUTS2_id) %>%
  dplyr::summarise(resid_mean = mean(pearson_residual, na.rm = TRUE), .groups="drop") %>%
  dplyr::arrange(NUTS2_id)

lw <- spdep::nb2listw(nb, style = "W")
print(spdep::moran.test(res_by_region$resid_mean, lw))

# ===========================
# Random effects extraction (examples)
# ===========================
# FID over time
re_tb_FID <- random_effect_tb(res, df_data, "FID", add_time=TRUE, time_var="week_idx")
re_tb_summ <- re_tb_FID %>% group_by(week_idx) %>%
  summarise(FID.mean.by_week=mean(FID.mean),
            FID.lower_bound.by_week=mean(FID.lower_bound),
            FID.upper_bound.by_week=mean(FID.upper_bound), .groups="drop")
p <- plot_scatter_lines(re_tb_summ, "week_idx", "FID.mean.by_week")
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect_month1.png", plot = p, width = 10, height = 6)

# --- CHANGED: Spatial random effect (BYM2) over NUTS2_id instead of country_ID
re_tb_nuts2 <- random_effect_tb(res, df_data, "NUTS2_id", add_time=FALSE)
re_tb_nuts2$NUTS2_id <- as.numeric(re_tb_nuts2$NUTS2_id)
re_tb_long <- dplyr::bind_rows(
  tibble::tibble(re_tb_nuts2 %>% dplyr::select(x=NUTS2_id, y=`NUTS2_id.mean`),          type="mean"),
  tibble::tibble(re_tb_nuts2 %>% dplyr::select(x=NUTS2_id, y=`NUTS2_id.lower_bound`),   type="lower bound"),
  tibble::tibble(re_tb_nuts2 %>% dplyr::select(x=NUTS2_id, y=`NUTS2_id.upper_bound`),   type="upper_bound")
) %>% dplyr::mutate(type = as.factor(type))
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, reorder=c(1,3,2))
p <- plot_scatter_lines(re_tb_long, "x","y","type", title="NUTS2 BYM2 effect",
                        plot_constant_lines_y = 0, manual_palette = col_pal)
ggsave("/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model/scatter_lines_random_effect__NUTS2_BYM2.png", plot = p, width = 10, height = 6)

# ===========================
# The rest of your plotting blocks (TempBins, SPIBins, pollen bins, beta, etc.)
# are unchanged and will run as-is.
# Only replace any reference to "country_ID" random effect with "NUTS2_id".
# ===========================


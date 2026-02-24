

# --- Where helper functions live 
source("functions_plotting.R")
source("functions.R")

# --- Packages
packages <- c(
  "readr","readxl","tidyr","dplyr","sf","rgdal","ggplot2","scales","spdep","tibble","purrr"
)
invisible(lapply(packages, require, character.only = TRUE))

# --- INPUTS / PATHS 
nuts_path <- "/export/data/talahdal/NUTS_RG_20M_2006_4326.shp/NUTS_RG_20M_2006_4326.shp"
out_dir   <- "/export/data/talahdal/Indicator_Lancet_Sentiments_Climate_2015-2022/2015-model"

# --- Load data
df <- read.csv("all_countries_lasso_bins_NA_an_cdc_final.csv")
df$week_date <- as.Date(df$week_date)

# --- Normalize indices to consecutive integers starting at 1
df <- df %>%
  mutate(
    week_idx   = match(week_idx,  sort(unique(week_idx))),
    month_idx  = match(month_idx, sort(unique(month_idx))),
    country_ID = match(country_ID, sort(unique(country_ID))),
    FID        = match(FID,       sort(unique(FID)))   
  ) %>%
  select(
    FID, week_idx, country, NUTS2, month_idx, time_idx, country_ID, calendar_week,
    TTw, NTw,
    airqualityBins, SolarradiationBins, Cloud_fractionBins, SPIBins,
    Pollen_birch_bins, Pollen_alder_bins, Pollen_oliver_bins,
    PrecipBins, TempBins, NAOBins, Wind_speedBins, Wind_directionBins,
    Relative_humidityBins, TempmeanBins, TempminBins,
    an_bin, TotalCases_bin,
    beta,beta_1,beta_2,beta_3,beta_4,beta_5,beta_6,beta_7,beta_8,beta_9,beta_10
  ) %>%
  arrange(week_idx, FID, country_ID)

# --- Offset (expected counts)
T3 <- sum(df$NTw, na.rm = TRUE) / sum(df$TTw[!is.na(df$NTw)])
df$E <- df$TTw * T3
df$E_input <- df$E

# --- Build adjacency aligned to FID order (via NUTS2 polygons)
df$NUTS2 <- as.character(df$NUTS2)

nuts_sf <- sf::st_read(nuts_path, quiet = TRUE)

lvl_col <- intersect(c("LEVL_CODE","STAT_LEVL","STAT_LEVL_","NIVEL"), names(nuts_sf)); stopifnot(length(lvl_col)>=1); lvl_col <- lvl_col[1]
id_col  <- intersect(c("NUTS_ID","CODE","id","ID"), names(nuts_sf));            stopifnot(length(id_col)>=1);  id_col  <- id_col[1]

nuts2_sf   <- nuts_sf %>% dplyr::filter(.data[[lvl_col]] == 2)
nuts2_keep <- nuts2_sf %>% dplyr::filter(.data[[id_col]] %in% unique(df$NUTS2))

# FID <-> NUTS2 must be 1:1
xwalk <- df %>% dplyr::distinct(FID, NUTS2) %>% dplyr::arrange(FID)
stopifnot(nrow(xwalk) == length(unique(df$FID)))

# Reorder polygons to the FID order
nuts2_levels <- nuts2_keep[[id_col]]
ord <- match(xwalk$NUTS2, nuts2_levels)
if (any(is.na(ord))) {
  stop("Some FID→NUTS2 codes not found in shapefile. Missing: ",
       paste(unique(xwalk$NUTS2[is.na(ord)]), collapse = ", "))
}
nuts2_fid_order <- nuts2_keep %>% dplyr::slice(ord)

# Build neighbors and write INLA graph 
nb_fid <- spdep::poly2nb(nuts2_fid_order, queen = TRUE)
graph_file <- "FID.adj"
spdep::nb2INLA(file = graph_file, nb = nb_fid)
stopifnot(file.exists(graph_file))
cat("Wrote INLA graph:", graph_file, "with", length(nb_fid), "regions\n")

# --- Helper: tidy fit metrics
fit_metrics <- function(res, model_name) {
  tibble::tibble(
    model = model_name,
    DIC   = if (!is.null(res$dic))  res$dic$dic  else NA_real_,
    DIC_p_eff = if (!is.null(res$dic))  res$dic$p.eff else NA_real_,
    WAIC  = if (!is.null(res$waic)) res$waic$waic else NA_real_,
    WAIC_p_eff = if (!is.null(res$waic)) res$waic$p.eff else NA_real_,
    MarginalLogLik = suppressWarnings(as.numeric(res$mlik[1])),
    LogScore = if (!is.null(res$cpo$cpo)) mean(-log(res$cpo$cpo), na.rm = TRUE) else NA_real_
  )
}

metrics_path <- file.path(out_dir, "fit_metrics.csv")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ============================================
# (1) IID baseline  (FID iid + AR1 over week_idx)
# ============================================
form_iid <- NTw ~ 1 +
  f(FID, model="iid", group=week_idx, control.group=list(model="ar1"), constr=FALSE) +
  f(country_ID,     model="iid", constr=TRUE) +
  f(calendar_week,  model="rw1", constr=TRUE) +
  f(SPIBins,        model="rw1", constr=TRUE) +
  f(TempBins,       model="rw1", constr=TRUE) +
  f(airqualityBins, model="rw1", constr=TRUE) +
  f(an_bin,         model="rw1", constr=TRUE) +
  f(TotalCases_bin, model="rw1", constr=TRUE) +
  f(Wind_speedBins, model="rw1", constr=TRUE) +
  f(Pollen_birch_bins, model="rw1", constr=TRUE) +
  f(Pollen_alder_bins, model="rw1", constr=TRUE) +
  f(Pollen_oliver_bins,model="rw1", constr=TRUE) +
  f(beta,   model="rw1", constr=TRUE) +
  f(beta_1, model="rw1", constr=TRUE) +
  f(beta_2, model="rw1", constr=TRUE) +
  f(beta_3, model="rw1", constr=TRUE) +
  f(beta_4, model="rw1", constr=TRUE) +
  f(beta_5, model="rw1", constr=TRUE) +
  f(beta_6, model="rw1", constr=TRUE) +
  f(beta_7, model="rw1", constr=TRUE) +
  f(beta_8, model="rw1", constr=TRUE) +
  f(beta_9, model="rw1", constr=TRUE) +
  f(beta_10,model="rw1", constr=TRUE)

library(INLA)
res_iid <- inla(
  form_iid, family="poisson", data=df, E=df$E_input,
  control.predictor = list(compute=TRUE),
  control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE),
  control.inla      = list(int.strategy="eb", control.vb=list(f.enable.limit=2)),
  verbose = TRUE
)
save(res_iid, file=file.path(out_dir,"SPI_model_IID.RData"))
readr::write_csv(fit_metrics(res_iid, "IID"), metrics_path, append = file.exists(metrics_path))

# ============================================
# (2) BYM2 on FID (spatial), keep country_ID iid
#     (no time grouping on BYM2 to keep model stable)
# ============================================
form_bym2_FID <- NTw ~ 1 +
  f(FID, model="bym2", graph=graph_file, scale.model=TRUE,
    hyper=list(
      prec = list(prior="pc.prec", param=c(1, 0.01)),  # P(1/sd > 1)=0.01
      phi  = list(prior="pc",      param=c(0.5, 2/3))  # P(phi < 0.5)=2/3
    )) +
  f(country_ID,     model="iid", constr=TRUE) +
  f(calendar_week,  model="rw1", constr=TRUE) +
  f(SPIBins,        model="rw1", constr=TRUE) +
  f(TempBins,       model="rw1", constr=TRUE) +
  f(airqualityBins, model="rw1", constr=TRUE) +
  f(an_bin,         model="rw1", constr=TRUE) +
  f(TotalCases_bin, model="rw1", constr=TRUE) +
  f(Wind_speedBins, model="rw1", constr=TRUE) +
  f(Pollen_birch_bins, model="rw1", constr=TRUE) +
  f(Pollen_alder_bins, model="rw1", constr=TRUE) +
  f(Pollen_oliver_bins,model="rw1", constr=TRUE) +
  f(beta,   model="rw1", constr=TRUE) +
  f(beta_1, model="rw1", constr=TRUE) +
  f(beta_2, model="rw1", constr=TRUE) +
  f(beta_3, model="rw1", constr=TRUE) +
  f(beta_4, model="rw1", constr=TRUE) +
  f(beta_5, model="rw1", constr=TRUE) +
  f(beta_6, model="rw1", constr=TRUE) +
  f(beta_7, model="rw1", constr=TRUE) +
  f(beta_8, model="rw1", constr=TRUE) +
  f(beta_9, model="rw1", constr=TRUE) +
  f(beta_10,model="rw1", constr=TRUE)

res_bym2 <- inla(
  form_bym2_FID, family="poisson", data=df, E=df$E_input,
  control.predictor = list(compute=TRUE),
  control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE),
  control.inla      = list(int.strategy="eb", control.vb=list(f.enable.limit=2)),
  verbose = TRUE
)
save(res_bym2, file=file.path(out_dir,"SPI_model_BYM2_on_FID.RData"))
readr::write_csv(fit_metrics(res_bym2, "BYM2_on_FID"), metrics_path, append = TRUE)

# ============================================
# (3) Covariates-only 
# ============================================
form_covs <- NTw ~ 1 +
  f(country_ID,     model="iid", constr=TRUE) +
  f(calendar_week,  model="rw1", constr=TRUE) +
  f(SPIBins,        model="rw1", constr=TRUE) +
  f(TempBins,       model="rw1", constr=TRUE) +
  f(airqualityBins, model="rw1", constr=TRUE) +
  f(an_bin,         model="rw1", constr=TRUE) +
  f(TotalCases_bin, model="rw1", constr=TRUE) +
  f(Wind_speedBins, model="rw1", constr=TRUE) +
  f(Pollen_birch_bins, model="rw1", constr=TRUE) +
  f(Pollen_alder_bins, model="rw1", constr=TRUE) +
  f(Pollen_oliver_bins,model="rw1", constr=TRUE) +
  f(beta,   model="rw1", constr=TRUE) +
  f(beta_1, model="rw1", constr=TRUE) +
  f(beta_2, model="rw1", constr=TRUE) +
  f(beta_3, model="rw1", constr=TRUE) +
  f(beta_4, model="rw1", constr=TRUE) +
  f(beta_5, model="rw1", constr=TRUE) +
  f(beta_6, model="rw1", constr=TRUE) +
  f(beta_7, model="rw1", constr=TRUE) +
  f(beta_8, model="rw1", constr=TRUE) +
  f(beta_9, model="rw1", constr=TRUE) +
  f(beta_10,model="rw1", constr=TRUE)

res_covs <- inla(
  form_covs, family="poisson", data=df, E=df$E_input,
  control.predictor = list(compute=TRUE),
  control.compute   = list(dic=TRUE, waic=TRUE, cpo=TRUE, config=TRUE),
  control.inla      = list(int.strategy="eb", control.vb=list(f.enable.limit=2)),
  verbose = TRUE
)
save(res_covs, file=file.path(out_dir,"SPI_model_covariates_only.RData"))
readr::write_csv(fit_metrics(res_covs, "Covariates_only"), metrics_path, append = TRUE)

cat("\n=== Fit metrics written to:", metrics_path, "===\n")

# ============================================
# BYM2-only:
# ============================================
df_bym <- res_bym2$.args$data
data_results <- dplyr::bind_cols(
  df_bym,
  tibble::tibble(
    rel_risk.mean        = res_bym2$summary.fitted.values$mean,
    rel_risk.lower_bound = res_bym2$summary.fitted.values$`0.025quant`,
    rel_risk.upper_bound = res_bym2$summary.fitted.values$`0.975quant`,
    rel_risk.st_dev      = res_bym2$summary.fitted.values$sd,
    rel_risk.is_significant = sign(rel_risk.lower_bound-1) == sign(rel_risk.upper_bound-1)
  )
) %>%
  mutate(
    predicted_mean        = E * rel_risk.mean,
    raw_residual          = NTw - predicted_mean,
    pearson_residual      = ifelse(TTw > 0, raw_residual / sqrt(predicted_mean), NA_real_)
  )

# Save BYM2 residuals/fits table (optional)
readr::write_csv(data_results, file.path(out_dir,"BYM2_filtered_residuals.csv"))

# Plot: relative risk averaged over weeks (BYM2 only)
p <- plot_scatter_lines(
  data_results %>% group_by(week_idx) %>% summarise(rel_risk.mean=mean(rel_risk.mean), .groups="drop"),
  "week_idx", "rel_risk.mean"
)
ggsave(file.path(out_dir,"scatter_lines_over_weeks_BYM2.png"), plot=p, width=10, height=6)

# Plot: BYM2 spatial random effect on FID 
random_effect_tb <- function(res, df, col) {
  stopifnot(col %in% names(res$summary.random))
  s <- res$summary.random[[col]]
  tibble::tibble(
    !!sym(col) := s$ID,
    mean = s$mean,
    lwr  = s$`0.025quant`,
    upr  = s$`0.975quant`
  )
}
re_fid <- random_effect_tb(res_bym2, df_bym, "FID") %>% mutate(FID = as.numeric(FID))
re_long <- bind_rows(
  tibble(x=re_fid$FID, y=re_fid$mean, type="mean"),
  tibble(x=re_fid$FID, y=re_fid$lwr,  type="lower bound"),
  tibble(x=re_fid$FID, y=re_fid$upr,  type="upper_bound")
) %>% mutate(type=factor(type))
col_pal <- color_palette_manual_by_idx(manual_palette_idx=14, n_discrete=3, reorder=c(1,3,2))
p <- plot_scatter_lines(re_long, "x", "y", "type", title="FID BYM2 effect",
                        plot_constant_lines_y=0, manual_palette=col_pal)
ggsave(file.path(out_dir,"scatter_lines_random_effect__FID_BYM2.png"), plot=p, width=10, height=6)

# ============================================
# OPTIONAL: compare covariate RW1 effects 
# ============================================
grab_rw1 <- function(res, term) {
  s <- res$summary.random[[term]]
  if (is.null(s)) return(NULL)
  tibble::tibble(term=term, x=s$ID, mean=s$mean,
                 lwr=s$`0.025quant`, upr=s$`0.975quant`)
}
terms <- c("SPIBins","TempBins","airqualityBins","an_bin","TotalCases_bin",
           "Wind_speedBins","Pollen_birch_bins","Pollen_alder_bins","Pollen_oliver_bins",
           "beta","beta_1","beta_2","beta_3","beta_4","beta_5","beta_6","beta_7","beta_8","beta_9","beta_10")
cov_iid  <- map_dfr(terms, ~grab_rw1(res_iid,  .x))  %>% mutate(model="IID")
cov_bym2 <- map_dfr(terms, ~grab_rw1(res_bym2, .x))  %>% mutate(model="BYM2_on_FID")
cov_df <- bind_rows(cov_iid, cov_bym2)
readr::write_csv(cov_df, file.path(out_dir,"covariate_effects_long.csv"))
cat("Wrote covariate effects to:", file.path(out_dir,"covariate_effects_long.csv"), "\n")

# Climate-Induced Stressors and Negative Sentiment from Geotagged Tweets

Code accompanying the STAR Protocols paper:

> Al-Ahdal T, Barman S, Alahmad B, Rocklöv J. *Protocol for analyzing
> spatiotemporal associations between climate-induced stressors and
> negative sentiment from geotagged social media data.* STAR Protocols (2026).

This protocol is based on the following published research articles:

1. Al-Ahdal T, Barman S, Dafka S, et al. *The impact of climatic factors on
   negative sentiments: An analysis of human expressions from X platform in
   Germany.* iScience 28(3), 2025.
2. Al-Ahdal T, Barman S, Alahmad B, et al. *The association of climate-induced
   stressors on risk of negative sentiment: An analysis from 462 million
   geotagged tweets in Europe.* iScience 28(12), 2025.

## Repository contents

### Python scripts
- `get_NLP15_7.py` — Tweet preprocessing pipeline (URL/mention removal,
  language detection, geographic assignment, temporal indexing).
- `the code for LIWC_CLI.py` — Batch LIWC22 sentiment analysis driver.
- `mergeAllLang.py`, `mergeAllLangRData.py`, `mergeAllYears.py` — Aggregation
  utilities across languages and years.
- `process.ipynb` — Jupyter notebook for climate variable extraction to
  NUTS regions.

### R scripts
- `INLAinstall.R` — INLA installation script.
- `europe.R` — Climate data extraction for European NUTS regions using
  `terra` and `exactextractr`.
- `import_data.R` — Data ingestion utilities.
- `main.R`, `run.R` — Top-level analysis drivers.
- `testrBase.R`, `testrLastAggregate.R` — Sentiment aggregation pipelines.
- `fit_inla_bym_model.R`, `fit_inla_iid_model.R`, `check_bym2.R` — INLA model fitting
  (Poisson GAM, BYM/BYM2 spatial structures, fused lasso temporal effects).
- `functions_general.R`, `functions_plotting.R` — Helper functions.

## Requirements

- R ≥ 4.2.2 with INLA, terra, exactextractr, spdep, genlasso
- Python ≥ 3.11.2 with pandas, numpy, geopandas, nltk, langdetect
- LIWC22 license (https://www.liwc.app/) for sentiment analysis
- High-performance computing recommended for full-scale tweet processing
  (64+ CPU cores, 400 GB+ RAM)

See the STAR Protocols paper for full installation and usage instructions.

## License

MIT License — see `LICENSE`.

## Contact


- Technical contact: Tareq Al-Ahdal (tareq.al-ahdal@uni-heidelberg.de)

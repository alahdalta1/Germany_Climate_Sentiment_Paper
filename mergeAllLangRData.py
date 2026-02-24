import pandas as pd
import pyreadr

# Specify the paths to your .RData files
rdata_file_paths = ['/export/data/talahdal/LIWCMergedYears/aggregate_de.RData','/export/data/talahdal/LIWCMergedYears/aggregate_en.RData','/export/data/talahdal/LIWCMergedYears/aggregate_fr_LAST.RData','/export/data/talahdal/LIWCMergedYears/aggregate_es.RData','/export/data/talahdal/LIWCMergedYears/aggregate_it.RData','/export/data/talahdal/LIWCMergedYears/aggregate_nl.RData','/export/data/talahdal/LIWCMergedYears/aggregate_pt.RData','/export/data/talahdal/LIWCMergedYears/aggregate_ro.RData']

results_list = []

# Use pyreadr to load RData files
for i, rdata_file_path in enumerate(rdata_file_paths):
    print(i)
    result = pyreadr.read_r(rdata_file_path)
    df = result['pivot_df_v2']  # Replace 'your_dataframe_name' with the actual name of the R object
    results_list.append(df)

final_result = pd.concat(results_list, axis=0, ignore_index=True)
final_result.to_csv('/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_all_last.csv', index=False)



























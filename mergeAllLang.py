import pandas as pd

lang_list = ['/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_de.csv', r'/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_en.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_fr_2.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_es.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_it.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_nl.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_pt.csv', '/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_ro.csv']

results_list = []

for i in range(len(lang_list)):
    csv_file_path = lang_list[i]
    print(i)
    if i==3:
        df = pd.read_csv(csv_file_path, on_bad_lines = 'skip', encoding='latin1')
    else:
        df = pd.read_csv(csv_file_path, on_bad_lines = 'skip', encoding='unicode_escape')
    results_list.append(df)

final_result = pd.concat(results_list, axis=0)
final_result.to_csv('/export/data/talahdal/LIWCMergedYears/LIWC_AllYears_resultsnuts3_aggregate_tweet_lang_all.csv', index=False)

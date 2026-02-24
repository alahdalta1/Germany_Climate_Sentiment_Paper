import pandas as pd
import csv

lang = 'de'
df1 = pd.read_csv(f'/export/data/talahdal/LIWC2015/LIWC-22 Results - 2015resultsnuts3_tweet_lang_{lang} - LIWC Analysis.csv', on_bad_lines='skip')
df2 = pd.read_csv(f'/export/data/talahdal/LIWC-2016nuts3/LIWC-22 Results - 2016resultsnuts3_tweet_lang_{lang} - LIWC Analysis.csv', on_bad_lines='skip')
df3 = pd.read_csv(f'/export/data/talahdal/2017LIWC/LIWC-22 Results - {lang}2017resultsnuts3 - LIWC Analysis.csv', on_bad_lines='skip')
df4 = pd.read_csv(f'/export/data/talahdal/LIWC2018/LIWC2018/LIWC-22 Results - 2018resultsnuts3_tweet_lang_{lang} - LIWC Analysis.csv', on_bad_lines='skip') #, encoding='latin1')
df5 = pd.read_csv(f'/export/data/talahdal/LIWC2019/LIWC-22 Results - 2019resultsnuts3_tweet_lang_{lang} - LIWC Analysis.csv', on_bad_lines='skip')
df6 = pd.read_csv(f'/export/data/talahdal/LIWC2020/LIWC-22 Results - 2020resultsnuts3_tweet_lang_{lang} - LIWC Analysis.csv', on_bad_lines='skip') #, encoding='latin1')
df7 = pd.read_csv(f'/export/data/talahdal/LIWC2021/LIWC-22 Results - 2021resultsnuts3_tweet_lang_{lang}___ - LIWC Analysis first.csv', on_bad_lines='skip') #, encoding='latin1')
#df8 = pd.read_csv(f'/export/data/talahdal/LIWC2022/LIWC-22 Results - 2022resultsnuts3_tweet_lang_{lang}___ - LIWC Analysis first.csv', on_bad_lines='skip')
#df9 = pd.read_csv(f'/export/data/talahdal/LIWC2021/LIWC-22 Results - 2021resultsnuts3_tweet_lang_{lang}___ - LIWC Analysis_second.csv', on_bad_lines='skip') #, quoting=csv.QUOTE_NONE)
df10 = pd.read_csv(f'/export/data/talahdal/LIWC2022/LIWC-22 Results - en2022resultsnuts3 - LIWC Analysis.csv', on_bad_lines='skip')

all_lang = pd.concat([df1, df2, df3, df4, df5, df6, df7, df10], axis=0)
print(len(all_lang))

# it = Emo_Pos, es = EmoPos, fr = émopos, en = emo_neg

all_lang2 = all_lang.rename({'posemo':'posEmo', 'negemo':'negEmo'}, axis='columns')
all_lang3 = all_lang2[['cleaned_text', 'processed_text', 'date', 'text', 'tweet_lang', 'place', 'geom', 'latitude', 'longitude', 'country', 'District', 'posEmo', 'negEmo']]

# df3 = df2.drop(['Segment', 'emo_pos_1', 'emo_neg_1', 'emo_anx', 'emo_anger', 'emo_sad', 'Affect', 'Segment_1', 'Affect_1', 'language', 'tone_pos', 'tone_neg', 'emotion_1', 'emotion', 'Emoji_1', 'Emoji', 'swear'], axis=1)

all_lang3.to_csv('/export/data/talahdal/LIWCMergedYears/LIWC_results_detaAllYears_resultsnuts3.csv', index=False)

#all_lang.to_csv(f'/export/data/talahdal/LIWCMergedYears/LIWC_results_{lang}Allresultsnuts3.csv', index=False)

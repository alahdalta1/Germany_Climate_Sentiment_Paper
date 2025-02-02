:wq#!/usr/bin/env python
import pandas as pd
data = pd.read_csv('/Users/alahdalta/Documents/filtered_data1.csv',low_memory=False)
import pandas as pd
import numpy as np
import seaborn as sns
import math
import matplotlib.pyplot as plt
import ast
import re
import nlp
import nltk
from textblob import TextBlob
from nltk import sent_tokenize, word_tokenize
from nltk.stem.snowball import SnowballStemmer
from nltk.stem.wordnet import WordNetLemmatizer
from nltk.corpus import stopwords
import spacy
nltk.download('punkt')
nltk.download('wordnet')
stop_words = stopwords.words('english')
def get_lang_model(lang):
    return spacy.load(f'{lang}_core_news_lg')
def preprocess_text(text, lang):
    nlp = get_lang_model(lang)
    doc = nlp(text)
    tokens = []
    for token in doc:
        if not token.is_punct and not token.is_stop:
            tokens.append(token.lemma_.lower())
    return tokens
def preprocess_tweet(tweet_text):
    cleaned_tweet = []
    tweet = TextBlob(tweet_text)
    lang = tweet.detect_language()
    words = word_tokenize(tweet_text)
    words = [word for word in words if word.isalpha()]
    words = [word for word in words if not word.lower() in stop_words]
    cleaned_tweet += preprocess_text(tweet_text, lang)
    return cleaned_tweet
data['date'] = pd.to_datetime(data['date'], errors ='coerce')
data['day_of_week'] = data['date'].dt.day_name()
data['month'] = data['date'].dt.month_name()
data['Hour'] = data['date'].dt.hour
data['year'] = data['date'].dt.year

data_copy = data.copy()
data_text = data_copy.text
all_sentence = []
for word in data_text:
    all_sentence.append(word)
len(all_sentence)
import re
def preprocess_tweet(tweet):
    # Check that the input tweet is a string or bytes-like object
    if not isinstance(tweet, (str, bytes)):
        return tweet
    
    # Remove URLs and mentions from the tweet
    tweet = re.sub(r"http\S+|@\S+", "", tweet)
    # Remove punctuation and convert to lowercase
    tweet = re.sub(r'[^\w\s]', '', tweet).lower()
    # Remove digits
    tweet = re.sub(r'\d+', '', tweet)
    # Remove stop words
    stop_words = set(stopwords.words('english'))
    tweet_tokens = nltk.word_tokenize(tweet)
    tweet = " ".join([word for word in tweet_tokens if not word in stop_words])
    # Stemming
    stemmer = SnowballStemmer("english")
    tweet_tokens = nltk.word_tokenize(tweet)
    tweet = " ".join([stemmer.stem(word) for word in tweet_tokens])
    # Lemmatization
    lemmatizer = WordNetLemmatizer()
    tweet_tokens = nltk.word_tokenize(tweet)
    tweet = " ".join([lemmatizer.lemmatize(word) for word in tweet_tokens])
    # Spell correction
    # You can use a spellchecker library like pyspellchecker to implement spell correction
    # Handling emojis and emoticons
    # You can use a library like emoji or emot to handle emojis and emoticons
    # Handling special characters
    # Depending on the specific characters that need to be handled, you may need to write custom code to handle them.
    # For example, you could use regular expressions to remove or replace specific characters.
    # Return the cleaned tweet
    return tweet

data_copy = data.copy()
data_text = data_copy.text
all_sentence = []

# Iterate through each sentence in data_text
for sentence in data_text:
    # Append the sentence to all_sentence if it's a string
    if isinstance(sentence, str):
        all_sentence.append(sentence)

lines = []
# Iterate through each sentence in all_sentence
for sentence in all_sentence:
    line = []
    # Check if the sentence is a string before splitting it
    if isinstance(sentence, str):
        words = sentence.split()
        for word in words:
            line.append(word)
    lines.append(line)

cleaned_tweets = []
for tweet in data_text:
    cleaned_tweets.append(preprocess_tweet(tweet))
cleaned_df = pd.DataFrame(cleaned_tweets, columns=['cleaned_text'])
import nltk
from nltk.corpus import udhr 
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('udhr')
from langdetect import detect
from langdetect.lang_detect_exception import LangDetectException
from nltk.corpus import udhr

# Load the English UDHR text
english_text = udhr.raw('English-Latin1')

# Use the detect() method on the English text
language = detect(english_text)

print(language)
# Replace missing values with empty strings

cleaned_df["cleaned_text"].fillna("", inplace=True)

# Create an empty list to store the detected languages
languages = []

# Loop over each row in the cleaned_df dataframe
for index, row in cleaned_df.iterrows():
    # Get the cleaned text for this row
    cleaned_text = row["cleaned_text"].strip()  # Strip whitespace from the text
    
    # Skip empty or whitespace-only strings
    if not cleaned_text:
        languages.append("")
        continue
    
    # Detect the language of the cleaned text
    try:
        language = detect(cleaned_text)
    except LangDetectException:
        language = "unknown"
    
    # Append the detected language to the languages list
    languages.append(language)

# Add the languages list as a new column to the cleaned_df dataframe
cleaned_df["language"] = languages

# Loop over each row in the cleaned_df dataframe
def process_text(text):
    # Check if the text has letters
    has_letters = any(char.isalpha() for char in text)
    if not has_letters:
        # Return an empty list if the text has no letters
        return []

    # Detect the language of the text
    try:
        lang = detect(text)
    except LangDetectException:
        # Handle the case where the language cannot be detected
        lang = "en" # Use English as default language
        
    if lang == "en":
        stopwords = nltk.corpus.stopwords.words("english")
    else:
        stopwords = nltk.corpus.stopwords.words(lang)
    
    # Tokenize the text
    tokens = nltk.word_tokenize(text)
    
    # Remove stopwords from the tokenized text
    tokens = [token.lower() for token in tokens if token.lower() not in stopwords]
    
    # Perform any additional processing steps as needed
    # ...
    
    # Return the processed text as a list of tokens
    return tokens
    
    # Tokenize the text using the appropriate tokenizer for the detected language
def process_text(text):
    try:
        lang = detect(text)
    except LangDetectException:
        lang = "unknown"
        # Log the error or print a message to indicate that the language could not be detected.
    
    if lang == "en":
        stopwords = nltk.corpus.stopwords.words("english")
    elif lang == "unknown":
        stopwords = []
        # You could choose to skip the text or handle it in some other way if the language is unknown.
    else:
        stopwords = nltk.corpus.stopwords.words(lang)
    
    tokens = nltk.word_tokenize(text)
    
    tokens = [token.lower() for token in tokens if token.lower() not in stopwords]
    
    return tokens


import nltk

# Download the necessary NLTK resources
nltk.download("punkt")
nltk.download("stopwords")

def process_text(text, lang="english"):
    # Use the stopwords corpus for the specified language
    stopwords = nltk.corpus.stopwords.words(lang)
    # Tokenize the text
    tokens = nltk.word_tokenize(text)
    # Remove stopwords from the tokenized text
    tokens = [token.lower() for token in tokens if token.lower() not in stopwords]
    # Perform any additional processing steps as needed
    # ...
    # Return the processed text as a list of tokens
    processed_text = tokens
    return processed_text

# Apply the process_text function to each row of the cleaned_df dataframe
cleaned_df["processed_text"] = cleaned_df["cleaned_text"].apply(process_text)

# Print the first few rows of the dataframe to verify the new column has been added
print(cleaned_df.head())
columns_to_add = data[['date', 'text', 'tweet_lang', 'place', 'geom', 'latitude', 'longitude']]
cleaned_df[['date', 'text', 'tweet_lang', 'place', 'geom', 'latitude', 'longitude']] = columns_to_add
desired_codes = ['en', 'de', 'es', 'pt', 'nl', 'ro', 'uk', 'fr', 'it', 'no', 'ru', 'sr']
data_filtered = cleaned_df[cleaned_df['language'].isin(desired_codes)]

data_filtered['latitude'] = pd.to_numeric(data_filtered['latitude'], errors='coerce')
data_filtered['longitude'] = pd.to_numeric(data_filtered['longitude'], errors='coerce')
import geopandas as gpd
from shapely.geometry import Point
world = gpd.read_file('/n/home02/balahmad/countries/ne_110m_admin_0_countries.shp', encoding='utf-8')
print(world.columns)
for index, row in data_filtered.iterrows():
    # Create a Point object using the longitude and latitude columns of the row
    point = Point(row['longitude'], row['latitude'])
    
    # Loop through each country in the shapefile
    for index2, row2 in world.iterrows():
        # Check if the point is inside the country
        if point.within(row2['geometry']):
            # If the point is inside the country, set the "country" column of the row to the country's name
            data_filtered.at[index, 'country'] = row2['SOVEREIGNT']
            # Print the country name assigned to the row
            print(f"Row {index} belongs to {row2['SOVEREIGNT']}")
            # Break out of the inner loop, since we've found the country the point belongs to
            break

data_filtered.to_csv('/n/holyscratch01/koutrakis_lab/Desired/NLP15_7desired.csv', index=False)



from flair.data import Sentence
from flair.models import TextClassifier
import pandas as pd


def get_flair_sentiments(df, text_col):
   sentiment_classifier = TextClassifier.load('sentiment-fast')
   df['sentence'] = df[text_col].apply(lambda k: Sentence(k))
   
   sentiment_classifier.predict(df['sentence'].tolist())
   df['score'] = df['sentence'].apply(lambda k: k.get_labels()[0].score if k.get_labels()[0].value == 'POSITIVE' else (-1)*k.get_labels()[0].score)

   return df

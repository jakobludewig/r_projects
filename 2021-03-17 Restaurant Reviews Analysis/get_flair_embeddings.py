from flair.data import Sentence
from flair.embeddings import TransformerDocumentEmbeddings
import pandas as pd
import numpy as np
import sys

text_col = "review_text"
id_col = "row_id"

file_name = sys.argv[1]
batch_size = int(sys.argv[2])

df = pd.read_csv(file_name)
embedding = TransformerDocumentEmbeddings('bert-base-uncased')
outs = list()

df['batch'] = np.arange(len(df))//batch_size
for b in df['batch'].unique():
   print(b)
   current_batch = df[df['batch'] == b]
   out = current_batch[text_col].apply(lambda k: pd.Series(embedding.embed(Sentence(k))[0].embedding.tolist()))
   out = pd.concat([current_batch[id_col],out],axis = 1)
   outs.append(out)

outs = pd.concat(outs)
outs.columns = [id_col] + ['emb_' + str(c) for c in outs.columns[1:]]
outs.to_csv("embeddings.csv")


# -*- coding: utf-8 -*-
"""emotioncode.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1Ty1bFnC62l65jCxHbmHHqckfNtA8RJf8
"""

import pandas as pd
import re
from sklearn.model_selection import train_test_split
import tensorflow as tf
import numpy as np
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Flatten, LSTM, Dropout, Activation, Embedding, Bidirectional

import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
STOPWORDS = set(stopwords.words('english'))

#vocab_size = 5000 # make the top list of words (common words)
embedding_dim = 64
max_length = 50
trunc_type = 'post'
padding_type = 'post'
oov_tok = '<OOV>' # OOV = Out of Vocabulary
training_portion = .8

emotions=pd.read_csv("/content/emo1.csv",encoding='latin-1')
sentence=emotions['text'].values
labels=emotions['emotion'].values

x2=[]
for each in x:
   x2.append(''.join([i for i in each if not i.isdigit()])) 


import nltk
nltk.download('stopwords')
nltk.download('punkt')
import nltk
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize 
set(stopwords.words('english'))

stop_words = set(stopwords.words('english'))     
# tokens of words  
#word_tokens = word_tokenize(x_train) 
def remove_stopwords(string):    
    tokenized = word_tokenize(string)
    filtered_sentence = [word for word in tokenized if not word in stop_words]
    return ' '.join(c for c in filtered_sentence)    
x3=[]
for i in x2: 
  x3.append(remove_stopwords(i))


from keras.preprocessing.text import Tokenizer

tokenizer = Tokenizer(num_words=500)
tokenizer.fit_on_texts(x2)
x4 = tokenizer.texts_to_sequences(x3)


vocab_size = len(tokenizer.word_index) + 1

from keras.preprocessing.sequence import pad_sequences

maxlen = 100

x5 = pad_sequences(x4, padding='post', maxlen=maxlen)

label_tokenizer=Tokenizer() 
label_tokenizer.fit_on_texts(y)
y_label=np.array(label_tokenizer.texts_to_sequences(y))

train_sen, validation_sen, train_labels, validation_labels = train_test_split(x5, labels, test_size=0.2)

print(len(train_sen),len(validation_sen))

#ps = PorterStemmer() 
#for w in train_sequences: 
    #print(w, " : ", ps.stem(w))

from sklearn.preprocessing import OneHotEncoder 
from sklearn.compose import ColumnTransformer
columnTransformer = ColumnTransformer([('encoder', 
                                        OneHotEncoder(), 
                                        [0])], 
                                      remainder='passthrough') 
  
train_labels= np.array(columnTransformer.fit_transform(training_label_seq), dtype = np.float)
validation_labels=np.array(columnTransformer.fit_transform(validation_label_seq), dtype = np.float)

label_tokenizer.word_index

from keras import layers

model = Sequential()
model.add(Embedding(10,50))
model.add(Dropout(0.5))
#model.add(layers.GlobalMaxPool1D())
model.add(Bidirectional(LSTM(50)))
model.add(layers.Dense (32  , activation='relu'))
model.add(layers.Dense(64, activation='relu'))
model.add(Dense(2,activation='softmax'))
model.summary()

opt = tf.keras.optimizers.Adam(learning_rate=0.002, beta_1=0.9, beta_2=0.999)
model.compile(
    loss='categorical_crossentropy',
    optimizer=opt,
    metrics=['accuracy'],
)

model.save('emotions.h5')
emote = model.fit(train_padded, train_labels, epochs=20, validation_data=(validation_padded, validation_labels), verbose=2)

max_features =50000
embedding_dim =64
sequence_length = 100

model1 = tf.keras.Sequential()
model1.add(tf.keras.layers.Embedding(max_features +1, embedding_dim, input_length=sequence_length,\
                                    embeddings_regularizer = regularizers.l2(0.005))) 
model1.add(tf.keras.layers.Dropout(0.3))

model1.add(tf.keras.layers.LSTM(embedding_dim,dropout=0.2, recurrent_dropout=0.1,return_sequences=True,\
                                                             kernel_regularizer=regularizers.l2(0.005),\
                                                             bias_regularizer=regularizers.l2(0.005)))

model1.add(tf.keras.layers.Flatten())

model1.add(tf.keras.layers.Dense(512, activation='relu',\
                                kernel_regularizer=regularizers.l2(0.001),\
                                bias_regularizer=regularizers.l2(0.001),))

model1.add(tf.keras.layers.Dense(8, activation='relu',\
                                kernel_regularizer=regularizers.l2(0.001),\
                                bias_regularizer=regularizers.l2(0.001),))
#model1.add(tf.keras.layers.Dropout(0.4))


model1.add(tf.keras.layers.Dense(3,activation='softmax'))
                               



model1.summary()


model1.compile(loss=tf.keras.losses.CategoricalCrossentropy(),optimizer=tf.keras.optimizers.Adam(1e-3),metrics=['accuracy'])

history = model1.fit(x_train, y_train,
                    epochs= 10,
                    batch_size=32,
                    validation_data=(x_test, y_test))

from sklearn import svm
from sklearn.ensemble import IsolationForest

oc_svm_clf = svm.OneClassSVM(gamma=0.001, kernel='rbf', nu=0.08)  # Obtained using grid search
if_clf = IsolationForest(contamination=0.08, max_features=1.0, max_samples=1.0, n_estimators=40)  # Obtained using grid search

oc_svm_clf.fit(x_train)
if_clf.fit(x_train)

oc_svm_preds = oc_svm_clf.predict(x_test)
if_preds = if_clf.predict(x_test)
a=0
b=0



txt=["great"]
seq = tokenizer.texts_to_sequences(txt)
padded = pad_sequences(seq, maxlen=max_length)
pred = model.predict(padded)
labels1 =['joy', 'sad', 'anger', 'fear', 'love', 'suprise']
lab=['0','1','2']
print(pred)
print(np.argmax(pred))
#print(lab[np.argmax(pred)])

emo=pd.DataFrame(emote.history)

import matplotlib.pyplot as plt

accuracy = emote.history['accuracy']
val_accuracy = emote.history['val_accuracy']
loss = emote.history['loss']
val_loss = emote.history['val_loss']

epochs = range(1, len(accuracy)+1)

plt.plot(epochs, accuracy, 'g', label='Training accuracy')
plt.plot(epochs, val_accuracy, 'r', label='Validation accuracy')
plt.title('Training and validation accuracy')
plt.legend()

plt.figure()

plt.plot(epochs, loss, 'g', label='Training loss')
plt.plot(epochs, val_loss, 'r', label='Validation loss')
plt.title('Training and validation loss')
plt.legend()

plt.show()

txt=["we can go in detail"]
seq = tokenizer.texts_to_sequences(txt)
padded = pad_sequences(seq, maxlen=100)


ou=oc_svm_clf.predict(padded)
if_pred = if_clf.predict(padded)
print(ou)
print(if_pred)
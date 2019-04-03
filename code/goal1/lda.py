#!/usr/bin/env python2
# -*- coding: utf-8 -*-
"""
Created on Thu Mar 14 16:23:55 2019

@author: tyt
"""
import pandas as pd
import gensim
from gensim import corpora, models
import matplotlib.pyplot as plt
import pyLDAvis

dat1 = pd.read_csv('brunch_process.csv')
dat1=dat1[["stars","text"]]
dat1.shape#(505696, 2)
reviews=dat1["text"].tolist()
stars=dat1["stars"].tolist()
rev=[]
no_importance_words=["food","make","come","take","give","dish","didnt","love","best","ask"]
#find, more, little, definitely, menu
for text in reviews:
    if type(text)==float:#in case there's nan
        continue
    a=text.split(" ")
    if (set(a) & set(no_importance_words)):
        retain=[]
        for i in a:
            if i not in no_importance_words:
                retain.append(i)
        rev.append(retain)
    else:
        rev.append(a)
       
del_index=[] 
start=-1     
for text in reviews:
    if type(text)==float:
        del_index.append(reviews.index(text,start+1))
        start=reviews.index(text,start+1)
 #[37594, 65812, 93694, 98491, 299675,407617, 501816]       
del_index.reverse()
for i in del_index:
    del reviews[i]
    del stars[i]
    
del_index=[]
for i in range(len(rev)):
    if len(rev[i])==0:
        del_index.append(i)
del_index.reverse()
for i in del_index:
    del reviews[i]
    del stars[i]
    del rev[i]
    
    
dic=gensim.corpora.Dictionary(rev)
len(dic.keys())
#218646

count=0
for k, v in dic.iteritems():
    print(k, v)   #k is assigned by the sequence of alphabet
    count+=1
    if count > 10:
        break
         
dic.filter_extremes(no_below=15, no_above=0.5, keep_n=100000) # no_below: absolute number, no_above: fraction
len(dic.keys())#53966

#using frequency
word_times = [dic.doc2bow(text) for text in rev] #sorted k of word and how many times they appear
len(word_times)# 505689
#using tf-idf
tfidf = models.TfidfModel(word_times)
tfidf_scores = tfidf[word_times] #length=505685

#composites sample
sample_10000 = word_times[10000]
"""for sample 10000:
     'definitely unique_twist enchilada start spicey potato corn_chowder doc guajillo short_rib 
     little jack_cheddar nice ceramic dish surprise put little oven finish soup bam deliciouso 
     wife split enchilada dish corn_flour tortilla straight corn roast_tomatillo chicken 
     other sweet spicey cornbread again doc otg delicious sauce red_green few more goody back oven 
     awesomeness flavor little back_again' """
stars[10000]#4
for i in range(len(sample_10000)):
    print("Word {} (\"{}\") appears {} time.".format(sample_10000[i][0], dic[sample_10000[i][0]], sample_10000[i][1]))

sample_399991 = word_times[399991]
"""for sample 399991:
     'long update_review az bread_company fantastic come move_arizona 
     freindliest people_work come hi remember cheerful 
     food general french_toast egg salad fondness green_chili 
     quiche particular come fast unique actually extremely quick bring_out food appear busy wind 
     bring niece_nephew child friendly_staff happy see take time see true_hidden gem glad house 
     green_chile quiche strawberry french_toast egg salad sandwich'"""
stars[399991]#5
sample_99999=word_times[99999]
for i in range(len(sample_399991)):
    print("Word {} (\"{}\") appears {} time.".format(sample_99999[i][0], dic[sample_99999[i][0]], sample_99999[i][1]))

""" LDA """
lda_model = gensim.models.LdaMulticore(word_times, num_topics=15, id2word=dic, passes=2, workers=2)
#lda_model.print_topic(0,15)
for idx, topic in lda_model.print_topics(-1):
    print('Topic: {} \nWords: {}'.format(idx, topic))

for index, score in sorted(lda_model[sample_10000], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model.print_topic(index, 10)))

#tfidf
lda_model_tfidf_15 = gensim.models.LdaMulticore(tfidf_scores, num_topics=15, id2word=dic, passes=2, workers=3)
lda_model_tfidf_18 = gensim.models.LdaMulticore(tfidf_scores, num_topics=18, id2word=dic, passes=2, workers=3)
lda_model_tfidf_30 = gensim.models.LdaMulticore(tfidf_scores, num_topics=30, id2word=dic, passes=2, workers=3)
lda_model_tfidf_50 = gensim.models.LdaMulticore(tfidf_scores, num_topics=50, id2word=dic, passes=2, workers=3)
lda_model_tfidf_75 = gensim.models.LdaMulticore(tfidf_scores, num_topics=75, id2word=dic, passes=2, workers=3)

coherence_model_lda = gensim.models.CoherenceModel(model=lda_model_tfidf_30, texts=rev, dictionary=dic, coherence='c_v')
coherence_lda = coherence_model_lda.get_coherence()
print('\nCoherence Score: {}'.format(coherence_lda))
#we choose 15 topics
for idx, topic in lda_model_tfidf_15.print_topics(-1):
    print('Topic: {} \nWord: {}'.format(idx, topic))
lda_model_tfidf_15.print_topic(1,20)

for index, score in sorted(lda_model_tfidf_15[sample_10000], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model_tfidf_15.print_topic(index, 15)))
for index, score in sorted(lda_model_tfidf_18[sample_399991], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model_tfidf_18.print_topic(index, 15)))
for index, score in sorted(lda_model_tfidf_15[sample_99999], key=lambda tup: -1*tup[1]):
    print("\nScore: {}\t \nTopic: {}".format(score, lda_model_tfidf_15.print_topic(index, 15)))
#word2vec word similarity
model = gensim.models.Word2Vec(
        rev,
        window=10,
        min_count=5,
        workers=3)
#model.train(documents, total_examples=len(documents), epochs=10)

model.wv.most_similar('postino')
model.wv.most_similar(positive=["snooze","http_skeptical"])
model.wv.similarity(w1='snooze',w2='http_skeptical')
model.wv.doesnt_match(output_15[0])
def above_70(w2vmodel,wordvector):
    result=w2vmodel.wv.most_similar(positive=wordvector,topn=50)
    word_select=[]
    i=0
    while (result[i][1] >=0.70) and (i<=49):
        word_select.append(result[i][0])
        i+=1
    return word_select

    
def gather_topic(topicnum,ldamodel,w2vmodel):
    main=[]
    for i in range(0,topicnum):
        terms=[]
        j=0
        while (ldamodel.get_topic_terms(i,100)[j][1] >= 0.002) and (j<=98):
            wordid=ldamodel.get_topic_terms(i,100)[j][0]
            terms.append(str(dic[wordid]))
            j+=1
        #if len(terms) != 0:
            #terms=terms + above_70(w2vmodel,terms)
        main.append(terms)
    return main

output_18=gather_topic(18,lda_model_tfidf_18,model)  
output_15=gather_topic(15,lda_model_tfidf_15,model)    
#use output_15 match topic and review  

def count_topic(topicnum,ldamodel,stars):
    scores=[[]]*topicnum
    for i in range(len(scores)):
        scores[i]=scores[i]+[0,0,0,0,0]
    n1=0;n2=0;n3=0;n4=0;n5=0;
    for i in range(len(reviews)):   
        if stars[i]==1:
            n1+=1
            for topicid, score in ldamodel[word_times[i]]:
                scores[topicid][0]+=score
        if stars[i]==2:
            n2+=1
            for topicid, score in ldamodel[word_times[i]]:
                scores[topicid][1]+=score
        if stars[i]==3:
            n3+=1
            for topicid, score in ldamodel[word_times[i]]:
                scores[topicid][2]+=score
        if stars[i]==4:
            n4+=1
            for topicid, score in ldamodel[word_times[i]]:
                scores[topicid][3]+=score
        if stars[i]==5:
            n5+=1
            for topicid, score in ldamodel[word_times[i]]:
                scores[topicid][4]+=score
    return scores, [n1,n2,n3,n4,n5]
scores,times=count_topic(15,lda_model_tfidf_15,stars)
    
                
            
        
        
            


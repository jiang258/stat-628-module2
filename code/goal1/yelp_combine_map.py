import json
import pandas as pd
import numpy as np
import plotly as py

"""
read in data, combine two training datasets
"""

with open('review_train.json', 'r') as r_train:
    test1=r_train.readlines()
dat_r = {'business_id':[],'stars':[],'text':[],'date':[]}
for d in test1:
    t_dic = json.loads(d)
    dat_r["business_id"].append(t_dic["business_id"])
    dat_r["stars"].append(t_dic["stars"])
    dat_r["text"].append(t_dic["text"])
    dat_r["date"].append(t_dic["date"])
df1 = pd.DataFrame(dat_r)

with open('business_train.json', 'r') as b_train:
    test2=b_train.readlines()
dat_b = {'business_id':[],'name':[],'city':[],'state':[],'postal_code':[],'latitude':[],'longitude':[],'is_open':[],'attributes':[],'categories':[],'hours':[]}
for d in test2:
    t_dic = json.loads(d)
    for k in dat_b.keys():
        dat_b[k].append(t_dic[k])
df2 = pd.DataFrame(dat_b)

df3=pd.merge(df1, df2, on="business_id",how="left")

"""
using plotly,give businesses distribution on map
color represents stars, size represents number of reviews
"""
ave_star=df1.groupby(['business_id']).agg(['mean'])
no_review=df1.groupby(['business_id']).agg(['count'])
dfnew=pd.merge(ave_star,no_review,on="business_id").iloc[:,[0,3]]
dfnew["business_id"]=dfnew.index.tolist()
df_t=df2[["business_id","name","latitude","longitude","categories","attributes"]]
dfnew=pd.merge(dfnew, df_t, on="business_id")
dfnew["text"]=dfnew["business_id"].astype(str)+" "+dfnew["name"]
#from light to dark, from one star to five stars
scl = [ [0,"rgb(204, 229, 255)"],[0.25,"rgb(102,178,255)"],[0.5,"rgb(0,128,255)"],[0.75,"rgb(0,102,204)"],[1,"rgb(0,51,102)"] ]
py.offline.plot({
        "data": [ dict(
                type = 'scattergeo',
                lon = dfnew['longitude'],
                lat = dfnew['latitude'],
                text = dfnew['text'],
                marker = dict(
                    size = dfnew.iloc[:,2]**(0.5),
                    opacity = 0.7,
                    line = dict(
                        width=0.5,
                        color='rgb(40, 40, 40)'
                    ),
                    colorscale = scl,
                    color = dfnew.iloc[:,1],
                    colorbar=dict(
                        title="Rating Stars"
                    )
                ))],
        "layout": dict(
                title = 'Yelp Reviews Distribution',
                geo = dict(
                    scope = 'north america',
                    lonaxis = dict( range= [ -125.0, -60.0 ] ),
                    lataxis = dict( range= [ 23.0, 60.0 ] ),
                    showland = True,
                    landcolor = 'rgb(243, 243, 243)',
                    countrycolor = 'rgb(204, 204, 204)',
                ),
            )
        },auto_open=True)

#fig = dict(data=data, layout=layout)
#py.iplot(fig,filename='yelp reviews map')


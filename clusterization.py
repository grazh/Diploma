###########
# K-means #
###########

import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score

test_data = pd.read_excel('rfm_data.xlsx')
samp = test_data

res = []
res_2 = []

for i in range(2, 11):

    print(i)
    mod = KMeans(n_clusters=i, random_state=3228).fit(samp[['recency', 'frequency', 'monetary']])
    samp['labels'] = mod.labels_
    samp.groupby('labels').mean()
    
    res.append(silhouette_score(samp[['recency', 'frequency', 'monetary']], samp['labels']))
    res_2.append(mod.inertia_)
    
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    x = np.array(samp['recency'])
    y = np.array(samp['frequency'])
    z = np.array(samp['monetary'])
    
    ax.scatter(x,y,z, marker="s", c=samp["labels"], s=40, cmap="RdBu")
    plt.savefig(Z_ENV_CREDENTIAL_MY_DIR + 'clust_plot' + str(i) + '.png')


###############################
# Иерархическая кластеризация #
###############################

from sklearn.datasets import load_iris
from sklearn.cluster import AgglomerativeClustering
import matplotlib.pyplot as plt
from sklearn.metrics import silhouette_score
import numpy as np
import pandas as pd
import tinkoff as tf

test_data = pd.read_excel('rfm_data.xlsx')

res = []
res_2 = []

for i in range(10, 11):
    print(i)
    clusterer = AgglomerativeClustering(n_clusters = i, affinity = 'euclidean', linkage = 'ward')
    labels = clusterer.fit_predict(test_data[['recency', 'frequency', 'monetary']])
    
    test_data['labels'] = labels
    test_data.groupby('labels').mean()

    res.append(silhouette_score(test_data[['recency', 'frequency', 'monetary']], test_data['labels']))
    # res_2.append(mod.inertia_)
    
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    x = np.array(1-test_data['recency'])
    y = np.array(test_data['frequency'])
    z = np.array(test_data['monetary']) * 10
    
    ax.scatter(x,y,z, c=labels, s=20, cmap="RdBu")
    plt.show()
    plt.scatter(x, y, s=np.exp(z)/10, c=labels)
    plt.savefig(Z_ENV_CREDENTIAL_MY_DIR + 'clust_plot' + str(i) + '.png')
    
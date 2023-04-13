import networkx as nx
import pandas as pd
import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import scipy
import pydot

#region Charger les données en csv
data_authors = pd.read_csv(os.getcwd() + '/data/full_db_with_authors.csv')
#endregion
#region Avoir les données de compte
# J'utilise un dictionnaire ici, car il y a une fonctionalité de matching que je peux utiliser ensuite, notamment pour réassigner à chaque
# nom de famille son compte dans toute la base.
# Ensuite, j'utilise .value_counts qui est l'équivalent de table sur R. Il y a pas mal de combinaisons qui valent le coup d'être vues,
# notamment sommer par colonnes etc.

a = dict(data_authors['last_names_author'].value_counts())

# On a ensuite un dataframe parce que plus simple à voir
compte = pd.DataFrame.from_dict(a, orient='index')
compte = compte.reset_index()
compte = compte.rename(columns = {compte.columns.values.tolist()[0]:'name', compte.columns.values.tolist()[1]:'value'})

# Ici, je mets la limite pour les occurences limites pour garder les auteurs.
# Tu peux changer la valeur pour le compte à garder
to_keep = compte[compte['value']>=3]

# Pour voir les co-auteurs, je construis une matrice d'adjacence:
# En lignes : les auteurs
# En colonne : les auteurs
# Une case vaut 1 si et seulement si les auteurs sont connectés par un papier

# On commence par faire une matrice vide
adjacency = np.zeros((len(compte), len(compte)))
# Pas le plus efficient, mais je fais une liste pour garder les noms
compte_names = list(compte.name)
# Boucle sur les noms
for x in compte_names:
    # On garde les titres tels que le nom de l'auteur considéré y apparaît
    titles = list(data_authors[data_authors['last_names_author']==x].title)
    # Parmi ces titres, je regarde les co-auteurs
    for y in titles:
        authors =list(data_authors[data_authors['title']==y].last_names_author)
        for z in authors:
            adjacency[compte_names.index(x),compte_names.index(z)]+=1 # Je rajoute un compte de 1

# On transforme cette matrice en dataframe
adjacency = pd.DataFrame(adjacency)
# Et on met les lignes et colonnes sous le bon format
adjacency.columns = compte_names
adjacency.index = compte_names

# Parmi cette liste de noms, tu peux ne garder que ceux que tu as choisi à partir du cut-off défini plus haut :
adjacency_k = adjacency.loc[list(to_keep.name), list(to_keep.name)]

# Enfin, tu peux inspecter comme tu veux ceci, parmi les noms de ta base :
    # 1. Tu définis la clé que tu cherches
nom_ = 'costello'
    # 2. Cela définit ici une Serie, en gros une colonne du DataFrame (c'est un format spécifique de pandas)
s = adjacency_k[nom_]
    # Là, tu vois le nombre de papier écrit par l'auteur dans sa case, avec ses co-auteurs.
    # Le seul truc un poil relou c'est que tu n'as pas ici les combinaisons de co-auteurs : il se pourrait que la somme des autres lignes soit
    # plus grande que le nombre d'articles, si jamais y'a genre 5 papiers mais 50 co-auteurs.
s[s>0]
# Ici, le nombre de co-auteurs, sans distinction de leur distribution à travers les papiers.
len(s[s>0])-1

# Enfin, par articles : si tu cherches les articles où sont les co-auteurs, ainsi que le compte.
auteurs = ['costello','polasky']
auteurs_set = set(auteurs)
# Tu peux ici choisir inclusif, c'est à dire les deux auteurs et leurs autres co-auteurs, ou papiers écrits seuls
# ou juste cette combinaison
choice = 'inclusive'
titres = list(set(data_authors.title))
# to_keep est ton output
to_keep = pd.DataFrame()
for t in titres:
    check = data_authors[data_authors['title']==t]
    if choice == 'inclusive':
        if set(check.last_names_author).intersection(auteurs_set):
            to_keep = pd.concat([to_keep,check])
    elif choice == 'strict':
        if set(check.last_names_author)==auteurs_set:
            to_keep = pd.concat([to_keep,check])




#endregion
#region Vision en graphes
# Pas encore fini, mais tu peux essayer si t'as envie :)
mapping = {}
for x in range(len(adjacency_k)):
    mapping[x] = to_keep.iloc[x,0]
adj_mat = np.mat(adjacency_k)
#adj_mat = np.mat(adjacency)
sizes = [int(adj_mat[i,i])*100 for i in range(len(adj_mat))]
adj_mat = np.array(adj_mat)

for i in range(len(adj_mat)):
    adj_mat[i,i]=0

adj_mat = adj_mat/100
G = nx.from_numpy_matrix(adj_mat, create_using=nx.Graph())
G = nx.relabel_nodes(G, mapping)
plt.figure(3,figsize=(20,20))
layout = nx.spring_layout(G)
nx.draw_networkx(G,layout, with_labels=True, node_size=sizes)
labels = nx.get_edge_attributes(G,'weight')
for x in list(labels.keys()):
    ind = list(labels.keys()).index(x)
    labels[x] = list(labels.values())[ind]*100
nx.draw_networkx_edge_labels(G, pos=layout,  edge_labels=labels)
plt.show()
#endregion
#region Rajouter les données de compte dans la base originale 2022
occurences_ = []
for x in list(data_authors['last_names_author']):
    occurences_.append(a.get(x,x))
data_authors['count_author'] = occurences_
#endregion


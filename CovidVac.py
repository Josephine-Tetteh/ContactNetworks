#!/usr/bin/env python
# coding: utf-8

# In[1]:


import igraph as ig
import random as rd
import numpy as np
import matplotlib.pyplot as plt

font = {'family': 'serif',
        'color':  'black',
        'weight': 'normal',
        'size': 18,
        }
params = {'legend.fontsize': 16,
          'legend.handlelength': 2.}
plt.rcParams.update(params)


# Each node can have three possible states: Susceptible (S), Infected (I), Recovered (R).  
# _pop_ is the size of the network (population)  
# _edge_per_node_ is the number of edges generated for each node  
# _p_infect_ is the probability for an infected node to contaminate its susceptible neighbours  
# _len_infect_ is the number of days an infected node remains contagious.

# In[2]:


color_dict = {"S": "blue", "I": "red", "R": "green", "V":"yellow"}
#pop=10000
edge_per_node = 10


# In[3]:


gg=ig.Graph.Erdos_Renyi(10000, m=50000)
pop = gg.vcount()


# In[ ]:


#gg = ig.Graph.Barabasi(pop, edge_per_node,power=0)
#gg=ig.Graph.Read_Ncol('myedgegr.txt',directed=False)
g=gg
g.vs["state"] = "S"
g.vs["duration"] = 0


# In[ ]:


np.mean(g.vs.degree())


# The model is initialised by infecting one random node (i). 

# In[ ]:


i = rd.randint(0, pop-1)
g.vs[i]["state"] = "I"
nb_S = [pop]
nb_I = [1]
nb_R = [0]
nb_V = [0]


# At each time step:  
# - The duration of infection is incremented for each infected node
# - Each infected node can contamine its neighbours with probability _p_infect_
# - When the duration of infection becomes equal to _infect_len_, the node becomes recovered (R)
# - The maximum degree among infected nodes is recorded (_max_deg_I_)

# https://www.newscientist.com/article/2238473-you-could-be-spreading-the-coronavirus-without-realising-youve-got-it/#ixzz6HjYOcZEu
# 
# - most people develop symptomps after about 5.1 (5) days
# - infectiousness starts about 2.5 (3) days before the onset of symptoms and peaks 0.625 (1) days before
# - once symptoms develop, a person's viral load declines steadily, and they become increasingly less infectious
# - people carry the virus (can infect) typically from 2-14 days

# In[ ]:


time_1perc = [] #time when 1% of the population is infected
time_1hub = [] #time when the first hub is infected
#deg_cutoff = 8
vacgrp = []
#unvac = []
Vlim = 0.35
probs_vac = 0.45 #0.45
beta = 0.05 #0.0170
probs_inf = (1-probs_vac)*beta
for time in range(150): #no. of days       
    for n in g.vs.select(state_eq = "I"): #iterates through each node in the network
        for nb in g.neighbors(n): #iterates through neighbours of that node
            if g.vs[nb]["state"] == "S": #if node is infected...
                r = rd.random() #random state
                if r < probs_vac:
                    g.vs[nb]["state"] = "V" #change state to infected
                    vacgrp.append(nb)
                    if len(g.vs.select(state_eq = "V")) == Vlim*pop: 
                        print(time,len(g.vs.select(state_eq = "V")))
                        len(g.vs.select(state_eq = "V"))==Vlim*pop
                        probs_vac = 0
                        if g.vs[nb]["state"] == "S": #if node is infected...
                            t = rd.random() #random state
                            if t < beta:
                                g.vs[nb]["state"] = "I"
                        break
                       
                    else:
                        continue
            unvac =  [i for i in g.neighbors(n) if not i in vacgrp]
            for un in unvac:
                if g.vs[un]["state"] == "S": #if node is infected...
                    s = rd.random() #random state
                    if s < probs_inf:
                        g.vs[un]["state"] = "I" 

        if g.vs[n.index]["duration"] > rd.randrange(1,14):
            g.vs[n.index]["state"] = "R"
    nb_S.append(len(g.vs.select(state_eq = "S"))) #no. of susceptibles in population
    nb_I.append(len(g.vs.select(state_eq = "I"))) #no. of infecteds in population
    nb_R.append(len(g.vs.select(state_eq = "R"))) #no. of recovereds in population
    nb_V.append(len(g.vs.select(state_eq = "V"))) #no. of recovereds in population
    #continue


# In[ ]:


newList = [x / pop for x in nb_I]


# In[ ]:


plt.plot(newList, label='I')


# In[ ]:


#plt.subplot(1, 2, 1)
plt.plot(nb_S, label='S')
plt.plot(nb_I, label='I')
plt.plot(nb_R, label='R')
plt.plot(nb_V, label='V')

plt.legend()


# In[ ]:





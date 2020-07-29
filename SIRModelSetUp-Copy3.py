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

# The model is initialised by infecting one random node (i). 

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

# In[2]:


def simfxn(Vlim,probs_vac,beta):
    color_dict = {"S": "blue", "I": "red", "R": "green", "V":"yellow"}
    g=ig.Graph.Erdos_Renyi(1000000, m=5000000)
    pop = g.vcount()
    g.vs["state"] = "S"
    g.vs["duration"] = 0
    i = rd.randint(0, pop-1)
    g.vs[i]["state"] = "I"
    nb_S = [pop]
    nb_I = [1]
    nb_R = [0]
    nb_V = [0]
    probs_inf = (1-probs_vac)*beta
    vacgrp = []
    for time in range(150): #no. of days
    #     if time == 20: 
    #         print(nb_V[-1])
    #         break
    #     if nb_V[-1] == 450: 
    #         print(nb_V[-1])
    #         break
        #else:        
        for n in g.vs.select(state_eq = "I"): #iterates through each node in the network
            g.vs[n.index]["duration"] += 1 #from day 0 to infect_len this node continues to infect
    #         if g.vs[n.index]["duration"] > rd.randrange(1,20):
    #             g.vs[n.index]["state"] = "R"
        # day_inf = g.vs[n.index]["duration"]
            #if len(g.vs.select(state_eq = "I"))<= Vlim*pop:
            for nb in g.neighbors(n): #iterates through neighbours of that node
                if g.vs[nb]["state"] == "S": #if node is infected...
                    r = rd.random() #random state
                    if r < probs_vac:
                        g.vs[nb]["state"] = "V" #change state to infected
                        #print(nb)
                        vacgrp.append(nb)
                        if len(g.vs.select(state_eq = "V")) == Vlim*pop: 
                            #print(time,len(g.vs.select(state_eq = "V")))
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
    return(nb_S,nb_I,nb_R,nb_V)


# In[3]:


Vlim = 0.4
probs_vac = 0.45 #0.45
beta = 0.02 #0.0170
simout=simfxn(Vlim,probs_vac,beta)


# In[4]:


datavector = []
for i in range(1000):
    simu = simfxn(Vlim,probs_vac,beta)
    datavector.append(simu)


# In[8]:


plt.figure()
plt.plot(simout[0], label='S',color = 'blue')
plt.plot(simout[1], label='I',color = 'red')
plt.plot(simout[2], label='R',color = 'green')
plt.plot(simout[3], label='V',color = 'yellow')

plt.legend()

for n in datavector:
    plt.plot(n[0],color = 'blue')
    plt.plot(n[1],color = 'red')
    plt.plot(n[2],color = 'green')
    plt.plot(n[3],color = 'yellow')
#plt.legend(loc=0, frameon=False)
#plt.show()
plt.savefig('toplot^6.pdf', bbox_inches='tight')


# In[ ]:





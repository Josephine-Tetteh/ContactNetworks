#!/usr/bin/env python
# coding: utf-8

# In[1]:


import igraph as ig
import math
import random as rd
import numpy as np
import matplotlib.pyplot as plt
import datetime
import matplotlib.dates as mdates
from matplotlib.dates import DayLocator, HourLocator, DateFormatter, drange, date2num
import numpy as np
import random
font = {'family': 'serif',
        'color':  'black',
        'weight': 'normal',
        'size': 18,
        }
params = {'legend.fontsize': 16,
          'legend.handlelength': 2.}
plt.rcParams.update(params)


# In[2]:


import pandas as pd

df = pd.read_excel ('Book2.xlsx') #place "r" before the path string to address special character, such as '\'. Don't forget to put the file name at the end of the path + '.xlsx'
# print (df)


# In[3]:


def double_smoothlog(time, bound1 , bound2, rate1 , rate2 , midpoint1 , midpoint2):
    result = []
    mini = 0
    maxi = time
    for x in range(time):
        if (midpoint1 > maxi | midpoint2 > maxi | midpoint1 < mini | midpoint2 < mini | midpoint1 > midpoint2):
            stop('midpoints not in range!')
        t1 = 1 / (1 + math.exp(-rate1*(x - midpoint1)))
        t2 = 1 / (1 + math.exp( rate2*(x - midpoint2)))
        out = bound1 + (bound2-bound1) * ((t1 + t2) - 1)
        result.append(out)
    return(result)


# In[6]:


def simfxn(Time,popul):
    bound1 = 0.028
    bound2 = 0.001
    rate1 = 0.09
    rate2 = 0.04
    midpoint1 = 50
    midpoint2 = 126
    beta = double_smoothlog(Time, bound1 , bound2, rate1 , rate2 , midpoint1 , midpoint2)
    color_dict = {"S": "blue", "I": "red", "R": "green", "V":"orange"}
    g=ig.Graph.Erdos_Renyi(popul, m=5*popul)
    pop = g.vcount()
    g.vs["state"] = "S"
    g.vs["duration"] = 0
    g.vs["somecol"] = 'THB'
    vac_eff = 0.1  # vaccine efficacy
    #randomly select an infected node to start epidemic
    i = rd.randint(0, pop-1)
    g.vs[i]["state"] = "E"  
    nb_S = [pop]
    nb_E = [1]
    nb_I = [0]
    nb_R = [0]
    nb_V = [0]
    exposed_vac = []
    Time = Time 
    count = 0 
    inf_nodes = []
    sec_contacts = []
    g.vs["tcount"] = 0 
    for time in range(Time): #no. of days  
        cutime = time
        if len(g.vs.select(state_eq = "E"))< int(0.01*popul):
            for n in g.vs.select(state_eq = "E"): #iterates through each node in the network
                g.vs[n.index]["duration"] += 1 
                if g.vs[n.index]["duration"] in range(7,21):  #(7,21)
                    g.vs[n.index]["state"] = 'I'
                for nb in g.neighbors(n): #iterates through neighbours of that node
                    if g.vs[nb]["state"] == "S": #if node is infected...
                        s=np.random.binomial(1, beta[time],1)
                        if s == 1:
                            g.vs[nb]["state"] = "E" 
            for m in g.vs.select(state_eq = "I"): #iterates through each node in the network
                g.vs[m.index]["duration"] += 1 #from day 0 to infect_len this node continues to infect                                
                for nbm in g.neighbors(m): #iterates through neighbours of that node
                    if g.vs[nbm]["state"] == "S": #if node is infected...
                        j=np.random.binomial(1, beta[time],1)
                        if j == 1:
                            g.vs[nbm]["state"] = "E"
                if g.vs[m.index]["duration"] in range(21,Time):
                    g.vs[m.index]["state"] = 'R'

        else:
            for inf in g.vs.select(state_eq = "E"):
                for na in g.neighbors(inf):  # select neighbour/contact of exposed nodes to vaccinate
                    g.vs[na]["somecol"] = 'myo'   
                    inf_nodes.append(na)

            for ele in g.vs.select(somecol_eq = 'myo'):
                g.vs[ele.index]["tcount"] += 1
                for elenb in g.neighbors(ele):         # select neighbour/contact of contact to vaccinate  
                    g.vs[elenb]["somecol"] = 'second'
                    sec_contacts.append(elenb)

            for sec in g.vs.select(somecol_eq = 'second'):
                g.vs[sec.index]["tcount"] += 1

            for then in inf_nodes:
                if g.vs[then]["tcount"]>= 3:
                    B = np.random.binomial(1, (1-vac_eff)*beta[time],1)  #vaccinate first contacts after 3 days
                    if B == 1:
                        g.vs[then]["state"] = "V"
            for scon in sec_contacts:
                if g.vs[scon]["tcount"]>= 3:
                    C = np.random.binomial(1, (1-vac_eff)*beta[time],1)  # vaccinate conact of contact after 3 days
                    if C == 1:
                        g.vs[scon]["state"] = "V"

            for n in g.vs.select(state_eq = "E"): #iterates through each node in the network
                g.vs[n.index]["duration"] += 1 
                if g.vs[n.index]["duration"] in range(7,21):  #(7,21)
                    g.vs[n.index]["state"] = "I"
                    count = count + 1
    #             else:
                for nb in g.neighbors(n): #iterates through neighbours of that node
                    if g.vs[nb]["state"] == "S": #if node is infected...
    #                     s = rd.random() #random state
                        s=np.random.binomial(1, beta[time],1)
                        if s == 1:
                            g.vs[nb]["state"] = "E" 
                for m in g.vs.select(state_eq = "I"): #iterates through each node in the network
                    g.vs[m.index]["duration"] += 1 #from day 0 to infect_len this node continues to infect                                
                    for nbm in g.neighbors(m): #iterates through neighbours of that node
                        if g.vs[nbm]["state"] == "S": #if node is infected...
        #                     j = rd.random() #random state
                            j=np.random.binomial(1, beta[time],1)
                            if j == 1:
                                g.vs[nbm]["state"] = "E"
                    if g.vs[m.index]["duration"] in range(21,Time):
                        g.vs[m.index]["state"] = 'R'

        nb_S.append(len(g.vs.select(state_eq = "S"))) #no. of susceptibles in population
        nb_E.append(len(g.vs.select(state_eq = "E"))) #no. of recovereds in population
        nb_I.append(len(g.vs.select(state_eq = "I"))) #no. of infecteds in population
        nb_R.append(len(g.vs.select(state_eq = "R"))) #no. of recovereds in population
        nb_V.append(len(g.vs.select(state_eq = "V"))) #no. of recovereds in population

    return(nb_S,nb_E,nb_I,nb_R,nb_V,cutime,g.vs['state'])


# In[7]:


Time = 400
popul = 100000
simout=simfxn(Time,popul)


# In[8]:


datavector = []
for i in range(10):          #repeat simulation 50 times
    simu = simfxn(Time,popul)
    datavector.append(simu)


# In[9]:


plt.figure(figsize=(9, 5))

plt.plot([x/popul for x in simout[0]], label='S',color = '#1f77b4')
plt.plot([x/popul for x in simout[1]], label='E',color = 'yellow')
plt.plot([x/popul for x in simout[2]], label='I',color = '#d62728')
plt.plot([x/popul for x in simout[3]], label='R',color = '#2ca02c')
plt.plot([x/popul for x in simout[4]], label='V',color = '#ff7f0e')
plt.legend(loc='right')

s_final = []
e_final = []
i_final = []
r_final = []

i_each = []

sList = []
eList = []
iList = []
rList = []

i_max = []

perc_inf = []
for n in datavector:
    sList = [x / popul for x in n[0]]
    eList = [x / popul for x in n[1]]
    iList = [x / popul for x in n[2]]
    rList = [x / popul for x in n[3]]
    vList = [x / popul for x in n[4]]
    num_inf = [n[5] / popul ]

 
    plt.plot(sList,color = '#1f77b4')
    plt.plot(eList,color = 'yellow')
    plt.plot(iList,color = '#d62728')
    plt.plot(rList,color = '#2ca02c')
    plt.plot(vList,color = '#ff7f0e')
    
    s_final.append(n[0][-1])
    e_final.append(n[1][-1])
    i_final.append(n[2][-1])
    r_final.append(n[3][-1])
#     v_final.append(n[4][-1])
    
    i_each.append(iList)
    i_max.append(max(iList))
    
    perc_inf.append(num_inf)
    
plt.axvspan(50, 126, color='gray', alpha=0.5, lw=0)
# plt.legend(loc='right')
plt.tick_params(axis = 'both', which = 'major', labelsize = 20)
plt.ylabel('Population', fontsize=30) 
plt.xlabel('Time(days)', fontsize=30)
x=list(range(Time))
plt.xticks(np.arange(min(x), max(x)+20, 50.0))
plt.show()

# plt.savefig('Ring_vac.pdf', bbox_inches='tight')


# In[ ]:





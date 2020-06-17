## This function simulates spread of infection on a network

gfunct <- function(G){
  # G is the network/graph
  G$AdjList = get.adjlist(G,mode="out")  # get adjoining neighbours of all nodes
  totpop = length(V(G))                  # total population
  vin = sample(totpop-1,1)               # pick anode at random
  V(G)$state = "S"
  V(G)$duration = 0
  V(G)[vin]$state = "NS"                 # set initial random node as infected with no symptoms
  V(G)[vin]$duration = 1
  # Initialize subcompartments
  NS = c()
  SS = c()
  MS = c()
  HP = c()
  ICU = c() 
  RM = c()
  SU = c()
  INF = c()
  tog = c()
  to = c()
  timing = 30                 # simulation duration
  tcount = 0
  m_inf = vin
  Time = 1:length(seq(1,timing,1))
  for (time in Time){
    new_inf = NULL
    for (n in m_inf){
      daily_contacts <- G$AdjList[[n]]  # get neighbours of infected nodes
      for (nb in daily_contacts){
        # there are no new infections if infected nodes are in hospital or ICU
        # else infection is spread with a probability p
        if(V(G)[n]$state == "HP"| V(G)[n]$state=="ICU"){
          new_inf = c(new_inf,NULL)
        }
        else{
          p = 0.15
          new_state = sample(c("S","NS"),1,prob = c(p,1-p))
          if (V(G)[nb]$state == "S" & new_state == "NS"){
            new_inf = c(new_inf,nb)
          }
        }
      }
    }
    m_inf = new_inf       # record new infections 
    to = c(to,new_inf)
    tog = c(vin,to)
    V(G)[tog]$duration = V(G)[tog]$duration + 1   # duratioin of infection in infected nodes
    V(G)[tog]$state=lapply(tog, function(x) intdyn(x,V(G)[x]$duration))
    
    tcount <- tcount + 1
    SU[tcount] = length(which(V(G)$state == "S")) 
    NS[tcount] = length(which(V(G)$state == "NS"))    #no. of nodes with no symptoms    
    SS[tcount] = length(which(V(G)$state == "SS"))    #no. of nodes with severe symptoms
    RM[tcount] = length(which(V(G)$state == "RM"))    #no. of nodes removed (immune or dead)
    ICU[tcount] = length(which(V(G)$state == "ICU"))  #no. of nodes in ICU
    HP[tcount] = length(which(V(G)$state == "HP"))    #no. of nodes hospitalised
    MS[tcount] = length(which(V(G)$state == "MS"))    #no. of nodes with mild symptoms 
    INF[tcount] = length(which(V(G)$state == "NS"))+length(which(V(G)$state == "MS"))+
                  length(which(V(G)$state == "SS"))+length(which(V(G)$state == "HP"))+
                  length(which(V(G)$state == "ICU"))    # total number of infected nodes
    }
  return(list(SU=SU,NS=NS,SS=SS,RM=RM,ICU=ICU,HP=HP,MS=MS,Time=Time))
}
gfunct <- compiler::cmpfun(gfunct)


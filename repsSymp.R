gfunct <- function(G){
  G = gr$g
  GAge <- gr$age
  dfGAg <- data.frame(cut(GAge, breaks = abreaks,labels=la1, right = FALSE))
  colnames(dfGAg) <-"agegrps"
  totpop = length(V(G))
  vin = sample(totpop-1,1)
  vin
  V(G)$state = "S"
  V(G)[vin]$state = "NS"
  V(G)$duration = 0
  nbS = c(totpop)
  nbI = c(1)
  nbR = c(0)
  probs_inf = c(0.01,0.01,0.1,0.2,0.3,0.3,0.3,0.25,0.2,0.15,0.1,0.05,0.01,0.01)
  NS = c()
  SS = c()
  MS = c()
  HP = c()
  ICU = c()
  RM = c()
  SU = c()
  timing = 30
  for (time in 1:length(seq(1,timing,1))){
    n = vin
    #print(n)
    #for (n in ni){
    V(G)[n]$color = intdyn(n,time)
    V(G)[n]$duration = V(G)[n]$duration+1
    day_inf = V(G)[n]$duration
    for (nb in neighbors(G,n)){
      p=0.15
      new_state = sample(c("S","NS"),1,prob = c(p,1-p))
      if (V(G)[nb]$state == "S" & new_state == "NS"){
        # r = runif(1)
        # if(r < probs_inf[day_inf]){
        V(G)[nb]$state = "NS"
        # }
      }
    }
    V(G)[n]$state = intdyn(n,V(G)[n]$duration)
    # if( V(G)[n]$duration >= sample(14,1)){
    #   V(G)[n]$state = "R"
    # #}
    # }
    SU[time] = length(which(V(G)$state == "S")) 
    NS[time] = length(which(V(G)$state == "NS")) #no. of susceptibles in population
    SS[time] = length(which(V(G)$state == "SS")) #no. of infecteds in population
    RM[time] = length(which(V(G)$state == "RM")) #no. of recovereds in population
    ICU[time] = length(which(V(G)$state == "ICU"))
    HP[time] = length(which(V(G)$state == "HP"))
    MS[time] = length(which(V(G)$state == "MS"))
  }
  return(data.frame(NS,SS,RM,ICU,HP,MS))
}

reps = replicate(50,gfunct(G))

write.table(reps,"repsSymp.csv")

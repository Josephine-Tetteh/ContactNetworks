### Function to determine the infection process based on age groups

intdyn = function(v1,ti){ 
  # v1 here is the newly infected node,
  # ti is the time of infection
  # G is the network
  l=ageGrp[v1]
  #G = gr$g
  ct= 1
  if (l==1&ti<=5){
    #print("old no symp")
    V(G)[v1]$state = "NS"
  }
  else if (l==1&(ti>5&ti<=8)){
    #print("old sev symp")
    V(G)[v1]$state = "SS"
  }
  else if (l==1&(ti>8&ti<=21)){
    #print("old ICU")
    V(G)[v1]$state = "ICU"
  }
  else if (l==1&ti>21){
    #print("old immune/dead")
    V(G)[v1]$state = "RM"
  }
  else if (l==2&ti<=5){
    #print("young no symp")
    V(G)[v1]$state = "NS"
  }
  else if (l==2&(ti>5&ti<=9)){
    #print("young sev symp")
    V(G)[v1]$state = "SS"
  }
  else if (l==2&(ti>9&ti<=20)){
    #print("young hosp")
    V(G)[v1]$state = "HP"
  }
  else if (l==2&ti>20){
    #print("young immune")
    V(G)[v1]$state = "RM"
     }
  else if (l==3&ti<=5){
    #print("mid-55% no symp")
    V(G)[v1]$state = "NS"
  }
  else if (l==3&(ti>5&ti<=10)){
    #print("mid-55% mild symp")
    V(G)[v1]$state = "MS"
  }
  else if (l==3&(ti>10)){
    #print("mid-55% immune")
    V(G)[v1]$state = "RM"
  }
  else if (l==4&ti<=5){
    #print("no symp-class no symp")
    V(G)[v1]$state = "NS"
  }
  else if (l==4&ti>5){
    #print("no symp-class immune")
    V(G)[v1]$state = "RM"
  }
  return(V(G)[v1]$state)
}

intdyn <- cmpfun(intdyn)

require(readr)
require(ggplot2)
require(igraph)
require(readxl)
#library(lattice)
require(reshape2)


genNet <- function(N=N, age=age, n.try=100) {
  require(igraph) # TODO: add check points for invalid inputs
  if (N < 100) warning("Considering increase N!")
  # Utils functions -------------------------------------------------------
  `%notin%` <- function(x, y) is.na(match(x, y, nomatch=NA_integer_))
  # pick age to make contacts according the prob. from contact matrix
  pickAge <- function(x) sample(Mnmr, ncont[x], 1, M[, Grp[x]])
  # pick id that is free and in respected age groups
  pickId <- function(x) {
    pool <- freeI[freeI %in% which(Grp==apick[x])]
    if (length(pool)==1) return(pool)
    else return(sample(pool, npick[x]))
  }
  makeLinks <- function(i, j) c(sapply(j, function(x) c(i, x)))
  # templates and storages ------------------------------------------------
  g <- make_empty_graph(n = N, directed = FALSE)
  freeI <- which(ncont > 0) # some might have no contact
  # Main loop -------------------------------------------------------------
  for (i in 1:N) {
    cat('\r', "Node:", i); flush.console()
    if (ncont[i] == 0) next # am I free?
    freeI <- freeI[freeI %notin% i] # exclude myself: no loop back
    # Check if enough peoples to contact ----------------------------------
    freeA <- rle(sort(Grp[freeI])) #freenodes by age-grp
    afree <- freeA$values  # age-grp that have freenodes
    nfree <- freeA$lengths  # of freenodes by ages
    isEnough <- noWay <- FALSE
    i.try    <- 0  # trying to find enough matched age partners
    while(!isEnough) {
      i.try <- i.try + 1
      if ( i.try > n.try ) isEnough <- noWay <- TRUE # force stop
      conti <- rle(sort(pickAge(i)))  # pick age of my contacts
      npick <- conti$lengths  # of contact by ages
      apick <- conti$values  # age-grp to pick
      if (!all(apick %in% afree)) next  # if no free age, try again
      ok <- sapply(seq_along(npick), function(x) npick[x] <= nfree[which(afree==apick[x])]) # if there are, check number enough?
      if (all(ok)) isEnough <- TRUE
    }
    if (noWay) next  # leave this guy alone
    pickedId <- unlist(sapply(seq_along(npick), function(x) pickId(x)))
    if (any(which(ncont==0) %in% pickedId) ) break
    g <- add_edges(g, makeLinks(i, pickedId) ) # add weights here if needed
    ncont[i] <- 0  # update me, no more friend
    ncont[pickedId] <- ncont[pickedId] - 1  # update friends
    freeI <- which(ncont > 0) # update free id
  }
  g <- set_vertex_attr(g, 'age', value=age)
  return(list(g=g, age=age))
}
genNet <- compiler::cmpfun(genNet)
plotNet <- function(g, vs=log(degree(g)+2)*2, vc=vertex_attr(g, "age"), ly=layout_with_fr, ...) plot(g, vertex.size=vs, vertex.label=NA, edge.arrow.mode=0, layout=ly, vertex.color=vc, edge.color="gray90",...) 
plotNet2 <- function(g, vs=log(degree(g)+2)*2, vc=vertex_attr(g, "age"),  ...) plot(g, vertex.size=vs, vertex.label=NA, edge.arrow.mode=0, vertex.color="#00AFBB", edge.color="gray90",...) 
###############################

df <- read_excel("data/Italy_ages_raw.xlsx")

plot(x=df$Number, y=df$Age)

abreaks<-c(0,19,51,69,103)
la1 <- c(paste(c(0,19,51),c(18,50,70),sep = "-"),paste(70,"+",  sep = ""))
la1

df$AgeGroup <- cut(df$Age, breaks = abreaks,labels=la1, right = FALSE)
agegrp_sum <- aggregate(df$Number, by=list(Category=df$AgeGroup), FUN=sum)
colnames(agegrp_sum)<-c("grps","freq")
agegrp_sum$agegrp_perc<- 100*(agegrp_sum$freq/sum(agegrp_sum$freq))
#plot(agegrp_sum$grps,agegrp_sum$freq)

#barchart(freq~grps,agegrp_sum, xlab="Age group", ylab="Count")
######
pdf("age distn plot.pdf")
p<-ggplot(data=agegrp_sum, aes(x=grps, y=freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Age distribution", 
       x="Age group", y = "Count") +
  theme_classic()
print(p)
dev.off()
# Horizontal bar plot
#p + coord_flip()

################################################
############ GROUPING ASPECT ###################

part2 <- read_excel("data/part2.xlsx", col_types = c("numeric","numeric"))
cdata  <- read_excel("data/contacts_data.xlsx", 
                     col_types = c("numeric", "numeric", "numeric", 
                                   "numeric", "numeric","text"))
mydata = c()
for (i in part2$part_id){
  mydata$dist[i]= length(which(cdata$part_id==i))
  mydata$pp[i] = i
}
datdf = data.frame(na.omit(mydata$dist),na.omit(mydata$pp))

part2$part_nocont = datdf$na.omit.mydata.dist.
part2$pt_grp <- cut(part2$part_age, breaks = abreaks,labels=la1, right = FALSE)
part_grp = as.data.frame(table(part2$pt_grp))


lli=lapply(seq_along(part2$part_age), function(i)rep(part2$part_age[i], part2$part_nocont[i]))
cdata$p_ages = unlist(lli)
ages_con = list()
for (i in 1:length(cdata$cnt_age_exact)){
  if (is.na(cdata$cnt_age_exact[i])==FALSE){
    cdata$ages_con[i] <- cdata$cnt_age_exact[i]}
  else {cdata$ages_con[i] <- cdata$cnt_age_est_max[i]}
  # else {cdata$ages_con[i] <- sample(seq(cdata$cnt_age_est_min[i],cdata$cnt_age_est_max[i]),1)}
}

cdata$cnt_grp <- cut(cdata$ages_con, breaks = abreaks,labels=la1, right = FALSE)
c_grp = as.data.frame(table(cdata$ages_con))
c_grp$ct_grp <- cut(as.numeric(c_grp$Var1), breaks = abreaks,labels=la1, right = FALSE)
c_sum <- aggregate(c_grp$Freq, by=list(Category=c_grp$ct_grp), FUN=sum)
cdata$c_grp_pro=ave(cdata$ages_con,cdata$cnt_grp)

#
cdata$pt_grp <- cut(cdata$p_ages, breaks = abreaks,labels=la1, right = FALSE)
p_grp = as.data.frame(table(cdata$p_ages))
p_grp$p_grp <- cut(as.numeric(p_grp$Var1), breaks = abreaks,labels=la1, right = FALSE)
p_sum <- aggregate(p_grp$Freq, by=list(Category=p_grp$p_grp), FUN=sum)
cdata$p_prob <- cdata$p_ages/length(cdata$p_ages)
cdata$p_grp_pro=ave(cdata$p_ages,cdata$pt_grp)
p_sum$perc_ita = c(1.6,27,34.2,37.2)
#
part2$part_pro = part2$part_nocont/length(part2$part_nocont)
#
c_time = as.data.frame(table(cdata$duration_multi))
p_time <- aggregate(p_grp$Freq, by=list(Category=p_grp$p_), FUN=sum)

#
mx <- with(cdata, table(participants=cdata$pt_grp,contacts=cdata$cnt_grp))
t(mx)

mtmx = melt(mx)
mtmx$nval = mtmx$value/sum(c_sum$x)

pdf("partcont.pdf")
pcplot = ggplot(mtmx, aes(participants,contacts, fill=nval)) + 
  geom_raster()+
  scale_fill_gradient(low="#F3F8FB", high="dodgerblue") +
  guides(fill = guide_legend(title = ""))
print(pcplot)
dev.off()
##################################################
cases_Italy <- read_excel("data/cases_Italy.xlsx", 
                          col_types = c("text", "numeric"))

summary(cases_Italy$Number)

######################################

a_sum <- as.data.frame(table(part2$part_nocont))
a_sum$prob <- 100*a_sum$Freq/sum(a_sum$Freq)

pdf("contdist.pdf")
PL<-ggplot(data=a_sum[1:50,], aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Contact distribution", 
       x="Number of contacts", y = "Frequency") +
  scale_x_discrete(breaks=c("10","20","30","40","50"),
                   labels=c("10","20","30","40","50"))+
  theme_classic()
print(PL)
dev.off()

###############
N    <- 500
seed <- 123

orderBy <- function(.data,index,...) as.data.frame(.data[order(.data[, index],...), ])

## Steps

#Assign age: depends on the target population age distribution

SR1     <- dget("./data/Italy_age_distn.txt")  # Italy population by age
if (!is.null(seed)) set.seed(seed)
age           <- sample(SR1$age, N, TRUE, SR1$prob)

#Assign contacts: depends on both target age and POLYMOD data
agecont      <- orderBy(dget("./data/POLYMODtab1"), 1) # POLYMOD Tab.1
agecont$rk   <- rank(agecont$age)  # Rank age-group by contact
# POLYMODbreak <- c(0,4,9,14,19,29,39,49,59,69,100) 
abreaks<-c(0,19,51,69,103)
POLYMODbreak <- abreaks 
ageGrp1 <- cut(age, breaks=POLYMODbreak, include.lowest=1)
ageGrp <- as.numeric(ageGrp1) # mapping age to ageGrp
agerk  <- sapply(ageGrp, function(x) agecont$rk[x==agecont$nmr])
age    <- rev(orderBy(cbind(age, agerk), 2)[, 1]) # large to small

#Sampling from contact distribution

distCont <- dget("data/distCont")

#distCont <- a_sum
ncont    <- sort(sample(distCont$freq, N, 1, distCont$prob), TRUE)

#Load contact matrix (as probability) POLYMOD data: averaging all countries

M <- mx  # image(M)
Mbrk <- abreaks
Mnmr <- 1:4
Grp  <- as.numeric(cut(age, breaks=Mbrk, include.lowest=1))

## Run

source("gennet.R")
gr <- genNet(N, age)

plotNet2(gr$g)
qqplot(degree(gr$g), ncont, main="QQ Plot", ylab="Target contact distribution")
qqplot(age, vertex_attr(gr$g, "age"), main="QQ Plot", ylab="Target age distribution")
########################################
#



intdyn = function(v1,ti){ #v1 here is the newly infected node
  #dinf = df$AgeGroup[400]
  #l = which(la1==dinf)
  l=ageGrp[v1]
  G1list <- list(G)
  ct= 1
  ti = ti
  #tim = seq(1,25,1)
  #while (length(ti)>0) {
  i= ti
  # for (i in ti){
  if (l==1&i<=5){
    #print("old no symp")
    V(G)[v1]$color = "orange"
    V(G)[v1]$state = "NS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==1&(i>5&i<=8)){
    #print("old sev symp")
    V(G)[v1]$color = "magenta"
    V(G)[v1]$state = "SS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==1&(i>8&i<=21)){
    #print("old ICU")
    V(G)[v1]$color ="cyan"
    V(G)[v1]$state = "ICU"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==1&i>21){
    #print("old immune/dead")
    V(G)[v1]$color = "green"
    V(G)[v1]$state = "RM"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==2&i<=5){
    #print("young no symp")
    V(G)[v1]$color = "orange"
    V(G)[v1]$state = "NS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==2&(i>5&i<=9)){
    #print("young sev symp")
    V(G)[v1]$color = "magenta"
    V(G)[v1]$state = "SS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==2&(i>9&i<=20)){
    #print("young hosp")
    V(G)[v1]$color ="pink"
    V(G)[v1]$state = "HP"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==2&i>20){
    #print("young immune")
    V(G)[v1]$color = "green"
    V(G)[v1]$state = "RM"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==3&i<=5){
    #print("mid-55% no symp")
    V(G)[v1]$color = "orange"
    V(G)[v1]$state = "NS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==3&(i>5&i<=10)){
    #print("mid-55% mild symp")
    V(G)[v1]$color = "magenta"
    V(G)[v1]$state = "MS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==3&(i>10)){
    #print("mid-55% immune")
    V(G)[v1]$color ="green"
    V(G)[v1]$state = "RM"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==4&i<=5){
    #print("no symp-class no symp")
    V(G)[v1]$color = "orange"
    V(G)[v1]$state = "NS"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  else if (l==4&i>5){
    #print("no symp-class immune")
    V(G)[v1]$color = "green"
    V(G)[v1]$state = "RM"
    ct <- ct + 1
    G1list[[ct]] <- G
  }
  #}
  return(V(G)[v1]$state)
}
G<-gr$g









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

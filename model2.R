#require(tidyverse)
require(readr)
require(ggplot2)
require(igraph)
require(readxl)
#library(lattice)
require(reshape2)
require(magrittr)
require(purrr)
require(dplyr)
require(compiler)


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
# pdf("m5.pdf")
# p<-ggplot(data=agegrp_sum, aes(x=grps, y=freq)) +
#   geom_bar(stat="identity", fill="steelblue") +
#   labs(title="Age distribution", 
#        x="Age group", y = "Count") +
#   theme_classic()
# print(p)
# dev.off()
# # Horizontal bar plot
# #p + coord_flip()
# 
# ################################################
# ############ GROUPING ASPECT ###################
# 
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
cdata$ages_con = NA
for (i in 1:length(cdata$cnt_age_exact)){
  if (is.na(cdata$cnt_age_exact[i])==FALSE){
    cdata$ages_con[i] <- cdata$cnt_age_exact[i]}
  else {cdata$ages_con[i] <- cdata$cnt_age_est_max[i]}
  # else {cdata$ages_con[i] <- sample(seq(cdata$cnt_age_est_min[i],cdata$cnt_age_est_max[i]),1)}
}
# 
# 
cdata$cnt_grp <- cut(cdata$ages_con, breaks = abreaks,labels=la1, right = FALSE)
c_grp = as.data.frame(table(cdata$ages_con))
c_grp$ct_grp <- cut(as.numeric(c_grp$Var1), breaks = abreaks,labels=la1, right = FALSE)
c_sum <- aggregate(c_grp$Freq, by=list(Category=c_grp$ct_grp), FUN=sum)
cdata$c_grp_pro=ave(cdata$ages_con,cdata$cnt_grp)
# 
# #
cdata$pt_grp <- cut(cdata$p_ages, breaks = abreaks,labels=la1, right = FALSE)
# p_grp = as.data.frame(table(cdata$p_ages))
# p_grp$p_grp <- cut(as.numeric(p_grp$Var1), breaks = abreaks,labels=la1, right = FALSE)
# p_sum <- aggregate(p_grp$Freq, by=list(Category=p_grp$p_grp), FUN=sum)
# cdata$p_prob <- cdata$p_ages/length(cdata$p_ages)
# cdata$p_grp_pro=ave(cdata$p_ages,cdata$pt_grp)
# p_sum$perc_ita = c(1.6,27,34.2,37.2)
# #
# part2$part_pro = part2$part_nocont/length(part2$part_nocont)
# #
# c_time = as.data.frame(table(cdata$duration_multi))
# p_time <- aggregate(p_grp$Freq, by=list(Category=p_grp$p_), FUN=sum)
# 
# #
mx <- with(cdata, table(participants=cdata$pt_grp,contacts=cdata$cnt_grp))
# t(mx)
# 
# mtmx = melt(mx)
# mtmx$nval = mtmx$value/sum(c_sum$x)
# # 
# # pdf("m6.pdf")
# # pcplot = ggplot(mtmx, aes(participants,contacts, fill=nval)) + 
# #   geom_raster()+
# #   scale_fill_gradient(low="#F3F8FB", high="dodgerblue") +
# #   guides(fill = guide_legend(title = ""))
# # print(pcplot)
# # dev.off()
# ##################################################
# # cases_Italy <- read_excel("data/cases_Italy.xlsx", 
# #                           col_types = c("text", "numeric"))
# # summary(cases_Italy$Number)
# 
# ######################################
# 
# a_sum <- as.data.frame(table(part2$part_nocont))
# a_sum$prob <- 100*a_sum$Freq/sum(a_sum$Freq)
# 
# pdf("m7.pdf")
# PL<-ggplot(data=a_sum[1:50,], aes(x=Var1, y=Freq)) +
#   geom_bar(stat="identity", fill="steelblue") +
#   labs(title="Contact distribution", 
#        x="Number of contacts", y = "Frequency") +
#   scale_x_discrete(breaks=c("10","20","30","40","50"),
#                    labels=c("10","20","30","40","50"))+
#   theme_classic()
# print(PL)
# dev.off()

###############
N    <- 100

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

#source("gennet.R")
gr <- genNet(N, age)
# 
# pdf("10m1.pdf")
# netp = plotNet(gr$g)
# print(netp)
# dev.off()

# 
# qqplot(degree(gr$g), ncont, main="QQ Plot", ylab="Target contact distribution")
# qqplot(age, vertex_attr(gr$g, "age"), main="QQ Plot", ylab="Target age distribution")
########################################

intdyn = function(v1,ti){ #v1 here is the newly infected node
  l=ageGrp[v1]
  G1list <- list(G)
  ct= 1
  i= ti
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
  
  return(V(G)[v1]$state)
}

#intdyn <- cmpfun(intdyn1)


gfunct <- function(G){
  G$AdjList = get.adjlist(G,mode="out")
  totpop = length(V(G))
  vin = sample(totpop-1,1) # take a random person to initiate infection
  Nn = 0.1*N
  vacin = sample(totpop-1,Nn,replace = FALSE) # choose 10% of popn at random
  vacgroup = unlist(lapply(vacin, function(x) neighbors(G,x))) #vaccinate neighbours of random person V(G)$state = "S"
  V(G)$duration = 0
  V(G)$num = 1
  V(G)[vin]$state = "NS"
  V(G)[vin]$duration = 1
  V(G)[vin]$num = 3
  V(G)[vacgroup]$state = "RM"
  #V(G)[vacgroup]$num = 0
  V(G)[vacgroup]$duration = 50
  # 
  NS = c()
  SS = c()
  MS = c()
  HP = c()
  ICU = c()
  RM = c()
  SU = c()
  VAC = c()
  timing = 120
  mglist = list(G)
  infcount = list()
  tcount = 0
  m_inf = vin
  all_inf = NULL
  tog = c()
  to = c()
  p = NULL
  #p = 0.15
  Time = 1:length(seq(1,timing,1))
  for (time in Time){
    new_inf = NULL
    for (n in m_inf){
      daily_contacts <- G$AdjList[[n]]
      if(n %in% vacgroup==TRUE){
        # p = 0
        new_state = "S"
        #new_inf = c(new_inf,nb)
        # break
      } else{
        p = 0.15
        new_state =  sample(c("S","NS"),1,prob = c(p,1-p))}
      for (nb in daily_contacts){
        if (V(G)[nb]$state == "S" & new_state == "NS"){
          V(G)[nb]$num = 2
          new_inf = c(new_inf,nb)
        }
      }
    }
    m_inf = new_inf
    to = c(to,new_inf)
    tog = c(vin,to)
    V(G)[tog]$duration = V(G)[tog]$duration + 1
    # for (ji in tog) {
    #V(G)[tog]$state = intdyn(tog,V(G)[tog]$duration)
    #}
    V(G)[tog]$state=lapply(tog, function(x) intdyn(x,V(G)[x]$duration))
    
    tcount <- tcount + 1
    SU[tcount] = length(which(V(G)$state == "S")) 
    NS[tcount] = length(which(V(G)$state == "NS")) #no. of  in population
    SS[tcount] = length(which(V(G)$state == "SS")) #no. of  in population
    RM[tcount] = length(which(V(G)$state == "RM")) #no. of  in population
    ICU[tcount] = length(which(V(G)$state == "ICU"))
    HP[tcount] = length(which(V(G)$state == "HP"))
    MS[tcount] = length(which(V(G)$state == "MS"))
    VAC[tcount] = length(which(V(G)$state == "vac"))
    
    mglist[[tcount]] = G
    
    #infcount[[tcount]] = new_inf
  }
  
  ############ R0
  GAge <- gr$age
  dfGAg <- data.frame(cut(GAge, breaks = abreaks,labels=la1, right = FALSE))
  colnames(dfGAg) <-"agegrps"
  VAge = GAge[vin]
  VAg = which(la1==dfGAg$agegrps[vin])
  Glist = mglist
  
  R0net <- function(net = gr$g, All=FALSE) {
    n  <- igraph::delete.vertices(net, which(V(tail(Glist,1)[[1]])$state=="S"))
    n  <- igraph::as.directed(n, mode = c("arbitrary"))
    dg <- igraph::degree(n,  mode='out')
    if (All==TRUE) r0 <- dg
    else r0 <- mean(dg)
    return(list(r0,n))
  }
  
  fro = R0net(G)
  return(list(SU=SU,NS=NS,SS=SS,RM=RM,ICU=ICU,HP=HP,MS=MS,Time=Time,R0 = fro[[1]],VAge = VAge,vin = vin))
  
  #return(list(SU=SU/N,NS=NS/N,SS=SS/N,RM=RM/N,ICU=ICU/N,HP=HP/N,MS=MS/N,Time=Time,R0 = fro[[1]],VAge = VAge,vin = vin))
}

gfunct <- compiler::cmpfun(gfunct)

G <- gr$g #sample_gnp(1000, 1/1000)

system.time(gfunct(G))

glist = gfunct(G)
glist
# 
df=data.frame(glist$SU,glist$NS,glist$SS,glist$RM,glist$ICU,glist$HP,glist$MS,glist$Time)
colnames(df)<- c("SU","NS","SS","RM","ICU","HP","MS","Time")
mdf2 = melt(df, id.vars = "Time")
colnames(mdf2)<- c("Time","State","Population")
# 
pdf("10m2.pdf")
gpl = ggplot(mdf2, aes(Time,Population, color=State)) +
  geom_line() +
  theme_bw()+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))
print(gpl)
dev.off()
# # 
# # #############################3
rep = replicate(10,gfunct(G))
reps = rep[1:8,]

#
# write.table(rep,"data1.csv")
#
myplot <- function(data){
  ggplot(data, aes(Time,Population, color=variable)) +
    geom_line() +
    theme_bw()+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))
}
# #
lo = c()
lo2 = c()
lo3 = c()
lo4 = list()
tml=list()
mtl4 = list()
mlti = list()
plot_list = list()
qp = list()
for (ik in 1:ncol(reps)) {
  lo = list(reps[,ik])
  #print(lo)
  lo2 <- map(lo,~data.frame(.))
  lo3 <-map_df(lo2,~mutate_all(.,as.numeric))
  lo4 <-as_tibble(lo3)
  tml[ik] = list(lo4)
  mtl4 = melt(tml[ik], id.vars = "Time")
  colnames(mtl4)<- c("Time","variable","Population")
  mlti[ik] = list(mtl4)
  #p <- myplot(mlti,1)
  #plot_list[[ik]] = p
  p <- myplot(mlti[[1]])
  qp[[ik]] = geom_line(mlti[[ik]], mapping=aes(Time,Population, color=variable))
  np = p + qp
  #print(np)
}
pdf("10npil.pdf")
print(np)
dev.off()
# # for (i in 1:length(seq(1,ncol(reps),1))) {
# #   file_name = paste("m9_", i, ".tiff", sep="")
# #   tiff(file_name)
# #   print(plot_list[[i]])
# #   dev.off()
# # }
#
# # # Another option: create pdf where each page is a separate plot.
# # pdf("m3.pdf")
# # for (i in 1:length(seq(1,ncol(reps),1))) {
# #   print(plot_list[[i]])
# # }
# # dev.off()
#
# #
# R0table = rep[9 :11,]
# nr0 = data.frame(c(unlist(R0table[1,])),c(unlist(R0table[2,])))
# colnames(nr0) = c("val","age")
# nr0df = as.data.frame(nr0)
# nr0df$grp <- cut(nr0df$ag, breaks = abreaks,labels=la1, right = FALSE)
# 
# #
# # nr0df2 = nr0df[order(nr0df$ag,nr0df$val),]
# # plot(nr0df2$ag,nr0df2$val)
# 
# dfr = data.frame(c(la1),c(tapply(nr0df$val, nr0df$grp, max)))
# dfr
# colnames(dfr)<- c("grp","val")
# 
# pdf("10mr0.pdf")
# R0plot <- ggplot(data=dfr, aes(x=grp, y=val)) +
#   geom_bar(stat="identity", fill="steelblue") +
#   labs(x="Age group", y = bquote(R[0]))+
#   theme_classic()+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14))
# print(R0plot)
# dev.off()
#  


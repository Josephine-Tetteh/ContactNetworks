# Simulating infection spread using "intdyn" and "gfunct"
require(readxl)
require(ggplot2)
require(igraph)
require(reshape2)
require(parallel)

## Data cleaning

df <- read_excel("data/Italy_ages_raw.xlsx")   # Italy population by age
# Divide population into age groups
abreaks<-c(0,19,51,69,103)  
la1 <- c(paste(c(0,19,51),c(18,50,70),sep = "-"),paste(70,"+",  sep = ""))
la1

df$AgeGroup <- cut(df$Age, breaks = abreaks,labels=la1, right = FALSE)
agegrp_sum <- aggregate(df$Number, by=list(Category=df$AgeGroup), FUN=sum)
colnames(agegrp_sum)<-c("grps","freq")

######
pdf("agedist.pdf")
p<-ggplot(data=agegrp_sum, aes(x=grps, y=freq)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="Age distribution",
       x="Age group", y = "Count") +
  theme_classic()
print(p)
dev.off()

## Data from POLYMOD study

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


lli=lapply(seq_along(part2$part_age), function(i)rep(part2$part_age[i], part2$part_nocont[i]))
cdata$p_ages = unlist(lli)
cdata$ages_con = NA
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

cdata$pt_grp <- cut(cdata$p_ages, breaks = abreaks,labels=la1, right = FALSE)
p_grp = as.data.frame(table(cdata$p_ages))
p_grp$p_grp <- cut(as.numeric(p_grp$Var1), breaks = abreaks,labels=la1, right = FALSE)

mx <- with(cdata, table(participants=cdata$pt_grp,contacts=cdata$cnt_grp))
t(mx)

mtmx = melt(mx)
mtmx$nval = mtmx$value/sum(c_sum$x)
# 
pdf("part_cont.pdf")
pcplot = ggplot(mtmx, aes(participants,contacts, fill=nval)) +
  geom_raster()+
  scale_fill_gradient(low="#F3F8FB", high="dodgerblue") +
  guides(fill = guide_legend(title = ""))
print(pcplot)
dev.off()
######################################
a_sum <- as.data.frame(table(part2$part_nocont))
 
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
# Using simnet procedure (by Kinh)
N    <- 100
seed <- 456789

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
Gbb = gr$g

G = gr$g
source("AGDyn.R")
source("InfSpread.R")

glist = gfunct(Gbb)
glist

df=data.frame(glist$SU,glist$NS,glist$SS,glist$RM,glist$ICU,glist$HP,glist$MS,glist$Time)
colnames(df)<- c("SU","NS","SS","RM","ICU","HP","MS","Time")
mdf2 = melt(df, id.vars = "Time")
colnames(mdf2)<- c("Time","variable","Population")
# 
pdf("simplot.pdf")
gpl = ggplot(mdf2, aes(Time,Population, color=variable)) +
  geom_line() +
  theme_bw()+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20))
print(gpl)
dev.off()

rep = replicate(10,gfunct(G))  # repeat simulations for n=10 times
reps = rep[1:8,]
myplot <- function(data){
  ggplot(data, aes(Time,Population, color=variable)) +
    geom_line() +
    theme_bw()+
    theme(axis.text=element_text(size=20),
          axis.title=element_text(size=20))
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
  lo2 <- map(lo,~data.frame(.))
  lo3 <-map_df(lo2,~mutate_all(.,as.numeric))
  lo4 <-as_tibble(lo3)
  tml[ik] = list(lo4)
  mtl4 = melt(tml[ik], id.vars = "Time")
  colnames(mtl4)<- c("Time","variable","Population")
  mlti[ik] = list(mtl4)
  p <- myplot(mlti[[1]])
  qp[[ik]] = geom_line(mlti[[ik]], mapping=aes(Time,Population, color=variable))
  np = p + qp
}

pdf("simplot2.pdf")
print(np)
dev.off()

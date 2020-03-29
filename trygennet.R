N    <- 100
seed <- 123

## Steps

#Assign age: depends on the target population age distribution

SR             <- dget("./data/SR")  # Sierra Leon population
if (!is.null(seed)) set.seed(seed)
age           <- sample(SR$age, N, TRUE, SR$prob)
# add ages 70/80+
age[age==70] <- sample(70:80, length(age[age==70]), replace=TRUE)


#Assign contacts: depends on both target age and POLYMOD data

#agecont      <- orderBy(dget("./data/POLYMODtab1"), 1) # POLYMOD Tab.1
Ndata <- dget("./data/POLYMODtab1")
dat <- order(Ndata[1],decreasing = TRUE)
Ndata$age=dat
agecont      <- Ndata
agecont$rk   <- rank(agecont$age)  # Rank age-group by contact
POLYMODbreak <- c(0,4,9,14,19,29,39,49,59,69,80) 
ageGrp <- cut(age, breaks=POLYMODbreak, include.lowest=1)
ageGrp <- as.numeric(ageGrp) # mapping age to ageGrp
agerk  <- sapply(ageGrp, function(x) agecont$rk[x==agecont$nmr])
#age    <- rev(cbind(age, agerk)[, 1]) # large to small

age <- rev(cbind(age,order(agerk,decreasing = TRUE))[,1])


#Sampling from contact distribution

distCont <- dget("./data/distCont")
ncont    <- sort(sample(distCont$freq, N, 1, distCont$prob), TRUE)


#Load contact matrix (as probability) POLYMOD data: averaging all countries

M <- dget("./data/M")  # image(M)
Mbrk <- c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,70)
Mnmr <- 1:15
Grp  <- as.numeric(cut(age, breaks=Mbrk, include.lowest=1))
Grp[is.na(Grp)] <- 15 # 70+ age-grp


## Run

source("gennet.R")
g <- genNet(N, age)

## Outputs
#png("nplot")
# plotNet(g)
#dev.off()
# 
# ![](./fig/ng1.png)
# 
# ## Validate
# 
# Compare age- and contact-distribution
# ```R
 ks.test(age, vertex_attr(g, "age"))
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  age and vertex_attr(g, "age")
# D = 0, p-value = 1
# 
 ks.test(degree(g), ncont)
# 
# Two-sample Kolmogorov-Smirnov test
# 
# data:  degree(g) and ncont
# D = 0, p-value = 1
# 
# put(1,2)
qqplot(degree(g), ncont, main="QQ Plot", ylab="Target contact distribution")
qqplot(age, vertex_attr(g, "age"), main="QQ Plot", ylab="Target age distribution")
# ```
# ![](./fig/qqplot.png)
# 
# 
# Compare contact-matrix
# ![](./fig/matrixCompare.png)

# pdf(file="tmp1.pdf",width=5,height=5)
qqplot(degree(g), ncont, main="QQ Plot", ylab="Target contact distribution")
# dev.off()


# pdf(file="plot1.pdf",width=5,height=5)
plotNet(g)
# dev.off()


library(RColorBrewer)
#redgreen <- c("cyan", "magenta","white") 
#pal <- colorRampPalette(redgreen)(100)#pdf(file="matr.pdf",width=15,height=15)
heatmap(M,Rowv = NA, Colv = NA)
#dev.off()

# 
# M = data.frame(degree(g),age)
# colnames(M)<-c('g','age')



library(deSolve)

orderBy <- function(.data,index,...) as.data.frame(.data[order(.data[, index],...), ])

# In host model
# 
pAgAbX <- c(
    deltaI = 1.1187,
    tauG   = 3.1574,
    rA     = 0.0815,
    deltaA = 0.0248
)

AgAbX <- function(t, x, parms) {
    with(as.list(c(parms, x)), {
        dI <- -deltaI*I
        dG <- (deltaI*I - G)/tauG
        dA <- rA*G - deltaA*A
        list(c(dI,dG,dA))
    })
}

outA <- ode(c(I=5*10^7, G=0, A=0), seq(0, 365, .1), AgAbX, 10^pAgAbX)

lastYear <- cbind(seq(-365, 0, 0.1), rep(0, length(seq(-365, 0, 0.1))))
twoYear <- rbind(lastYear, outA[,c("time", "A")])
fAb <- approxfun(twoYear[,1], twoYear[,2], rule=2)

inModel <- function(t, x, parms) {
  with(as.list(c(parms, x)), {
    # checking time of vaccination, before or after the infection?
    A <- ifelse(d == 0,  fAb(t), fAb(t-d) + fAb(t) )
    # and have a corresponding viral dynamics
    dV <- r*V*(V/(N+V)) * (1 - V/K) * (1 - A/KAb)
    list(c(dV))
  })
}

estPars <- dget("data/estPars")

getV <- function(V0=10^.15, dx=0, Kx=10^4, times=seq(0, 30, .1)) {
  output <- ode(c(V=V0), times, inModel, c(d=dx, 10^estPars, KAb=Kx))[, "V"]
  return(approxfun(times, output, rule=2))
}

# Age weights based NATURE review
# -------------------------------------------------------------------------
wAge <- read.csv("data/susceptibilityAge.csv")
wAge[9, ] <- wAge[8, ]+.163
fAge <- approxfun(wAge$age, wAge$p/100)
Kage <- cbind(orderBy(cbind(0:80, fAge(0:80)), 2), seq(2.5,4.5,length.out=81))

# net=M # the network to simulate on generated from gennnet
# ni0=1 # number of initially infected cases
# Vc=10^0.15  # default infective dose
# maxV = 72880400 # default maximum viral load
# tmax = 60 # default simulated within-host infection dynamics in days
# tStep = 1 # default time step in days
# maxi = 30 # default epidemic time in days
# pvac = 0.3 #default vaccination coverage
# tVac = c(0, 30) # first and last time point the vaccines are delivered
# prior=TRUE # vaccination was given before (default to TRUE) or after


source("code/netsim.R")
source("trygennet.R")
out <- netsim(net = g, tmax =365, pvac = 0)
out
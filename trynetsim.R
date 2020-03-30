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

pAge <- dget("data/ageSusceptibility")

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
out <- netsim(net = M, tmax =365, pvac = 0)
out



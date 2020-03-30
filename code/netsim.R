netsim <- function(net=M, ni0=1, Vc=10^0.15, maxV = 72880400, tmax = 60, tStep = 1, maxi = 30, pvac = 0.3, tVac = c(0, 30), prior=TRUE) {
  require(truncnorm); require(Matrix); require(deSolve)
    age   <- net$age
    Mx    <- net$g
    pop   <- length(age)
    t.inf <- seq(0, maxi, .1)
    t.epi <- seq(0, tmax, tStep)
    vlist <- vector("list", pop)
    SIR   <- Matrix(0, length(t.epi), 6)
    colnames(SIR) <- c("S", "I", "R", "D", "B", "In")
    S <- Matrix(0, pop, 8)
    colnames(S) <- c("D", "Tr", "Ti", "Td", "Dd", "Tb", "s", "Tv")
  auc0 <- function(x, y, maxX=length(x)) {
    x <- x[0:maxX]
    y <- y[0:maxX]
    return(sum(diff(x) * (head(y,-1)+tail(y,-1)))/2)
  }
  fFate <- function(x) {
    myV <- vlist[[x]](t.inf)
    tVmax <- t.inf[which.max(myV)]
    auctemp <- sapply(seq_along(t.inf), function(k) auc0(t.inf, myV, maxX=k))
    isDie <- any(auctemp > dieAUC)
    tDie <- ifelse(isDie, which(auctemp > dieAUC)[1], 0)  # 0 to save memory
    tRecover <- t.inf[myV < 50 & t.inf >= tVmax][1]
    tR <- ifelse(isDie, 0, ifelse(is.na(tRecover), 30, tRecover) )  # rebound problem!
    tBr <- ifelse(isDie, round(rtruncnorm(1, 1, 3, 1, .5), 2), 0) # time to burial
    return( c(isDie, tR, tDie, tBr) )
  }
  updateI <- function(Yes, tinf) {
    S[Yes, "Ti"] <<- tinf # record time of infection
    S[Yes, "s"]  <<- 1  # change status # S[Yes, "V"]  <- v  # V0
    S[Yes, c("D", "Tr", "Dd", "Tb")] <<- t(sapply(Yes, fFate))
  }
  updateStatus <- function(r) {
    SIR[r, "S"] <<- length(which(S[,"s"]==0))
    SIR[r, "I"] <<- length(which(S[,"s"]==1))
    SIR[r, "R"] <<- length(which(S[,"s"]==2))
    SIR[r, "D"] <<- length(which(S[,"s"]==3))
    SIR[r, "B"] <<- length(which(S[,"s"]==4))
  }
  neighbors <- function(mat, r) return(which(mat[r, ]==1))
  findNaive <- function(mat, i) {
    nbi  <- neighbors(mat, i)           # Find all neighbor(s)
    nbi  <- nbi[which(S[,"s"][nbi]!=4)] # exclude buried ones
    nbi  <- nbi[which(S[,"s"][nbi]==0)] # who is susceptible
    return(nbi)
  }
  ids <- 1:pop
  i0 <- sample(ids, ni0)  # choose infected and update status
    nn <- length(neighbors(Mx, i0))
    message("\nIndex case ", i0, " infected with ", nn, " contacts!\n")
    vlist[[i0]] <- getV(dx = unname(S[i0, "Tv"]))  # V function in i0
    dieAUC <- auc0(t.inf, vlist[[i0]](t.inf), 71)  # references AUC
    updateI(i0, tinf=0); SIR[1, "In"] <- 1 # incidences as of now
  # Vaccine before ---------------------------------------------------------
  realP <- 0  # actual percentage of vaccinated indiviudal catching the 
              # error of early stopping of the epidemics
  if ( pvac!=0  & prior ==TRUE) {
    realP <- pvac
    vacID <- sample(ids[-i0], round(pop*pvac))
    S[vacID, "Tv"] <- -sample(tVac[1]:tVac[2], length(vacID), replace=1)
  }
  pId <- vector('character', pop)
  for (ti in 2:length(t.epi)) {
    now <- t.epi[ti]
    updateStatus(ti) # tracking numbers
    cat('\r', "time:", now, "S I R D B In", as.vector(SIR[ti-1, ]))
    flush.console()
    # Vaccine after ---------------------------------------------------------
    if (now == tVac[1] & pvac!=0  & prior == FALSE) {
      ssID <- which( S[, "s"] == 0 )  # compare if enough susceptibles
      if ( length(ssID) >= round(pop*pvac) ) {
        vacID <- sample( ssID, round(pop*pvac) ) 
        realP <- pvac
      } else {
        vacID <- unique(sample(ssID, round(pop*pvac), replace=1))
        realP <- (length(vacID)/pop)*100
      } 
      S[vacID, "Tv"] <- sample(tVac[1]:tVac[2], length(vacID), replace=1)
    }
    # Infection processes ---------------------------------------------------  
    infId <- which(S[,"s"] %in% c(1,3) )  # Who infected/death-not-buried
    if ( length(infId) < 1 ) break  # stop if no one infected
    infDuration <- now - S[infId, "Ti"]  # infected duration till now
    currentV <- mapply(do.call, vlist[infId], lapply(infDuration, list) )
    pTrans <- pmin(1, currentV/maxV) # Transmission potential
    nbi <- lapply(infId, findNaive, mat=Mx)  # Find susceptible neighbor
    ageW <- lapply(nbi, function(x) fAge(age[x]))  # Weights age
    pW <- Map("*", ageW, sapply(pTrans, list) ) # Weights age
    p <- Map(rbinom, sapply(nbi, length), size=1, pW) # try to infect
    P <- unlist(p) == 1
    if ( any(P) ) {
      allnb <- unlist(nbi)
      Yes <- allnb[P]  # infected ids
      SIR[ti, "In"] <- length(Yes)  # incidences as of now
      v <- Vc*rep(pTrans, times = sapply(nbi, length))[P]
      vK <- Kage[sapply(age[Yes], function(x) which(Kage[, 1] == x)), 3]
      # vaccination time need to be negative or zero!
      if (prior) {
        dxx <- pmin(replace(S[Yes, "Tv"], which(S[Yes, "Tv"]==0), Inf) - now, 0)
      } else { 
        dxx <- -pmax(now - replace(S[Yes, "Tv"], which(S[Yes, "Tv"]==0), Inf), 0)
      }
      # On the fly computation
      cpId <- paste0(round(v, 2), vK, dxx)  # get unique combination id
      mId <- match(cpId, pId)  # Ids of those matched
      pId[Yes] <- cpId  # record the new infection ids for next time matching
      notRun <- which(!is.na(mId))  # post of infected that NO need to run
      Run <- which(is.na(mId))  # post of infected that need to run
      # check if exist match, copy from existed, run only new profiles
      if ( length(notRun) > 0 ) vlist[ Yes[notRun] ] <- vlist[ mId[notRun] ]  
      if ( length(Run) > 0 ) vlist[ Yes[Run] ] <- Map(getV, V0=v[Run], Kx=10^(vK[Run]), dx = dxx[Run])
      updateI(Yes, now)
    } else {
      SIR[ti, "In"] <- 0  # incidences as of now
    }
    # Update the cured ------------------------------------------------------  
    infId <- which(S[,"s"] == 1 )  # Who infected, exclude death
    infDuration <- now - S[infId, "Ti"]
    recoverConditions <- S[infId, "D"] == 0 & infDuration >= S[infId, "Tr"]
    if (any(recoverConditions)) {
      recoverID <- infId[recoverConditions]
      S[recoverID, "s"] <- 2
    }
    # Update the died -------------------------------------------------------  
    dieConditions <- S[infId, "D"] == 1 & infDuration >= S[infId, "Dd"]
    if (any(dieConditions)) {
      dieId <- infId[dieConditions]
      S[dieId, "s"] <- 3
      S[dieId, "Td"] <- now # record time of death
    }
    # burying the dead ------------------------------------------------------  
    dieId <- which(S[, "s"] == 3)  # all those dead
    if ( length(dieId) > 0 ) {
      dieT <- now - S[dieId, "Td"]  # how long he died
      buried <- dieT > S[dieId, "Tb"]
      if ( any(buried) ) S[dieId[buried], "s"] <- 4  # change status
    }
  }
  # Storing for output ------------------------------------------------------
  SIR <- as.data.frame( cbind(t.epi, as.matrix(SIR)) )
  colnames(SIR)[1]  <- "time"
  class(SIR)        <- "smidSIR"
  return(list(SIR=SIR, aux=S[,"s"], nn=nn, pvac = realP)) 
}
netsim <- compiler::cmpfun(netsim)
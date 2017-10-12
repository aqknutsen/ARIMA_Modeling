library(expm)
library(markovchain)
library(diagram)
library(pracma)

stateNames <- c("Rain","Nice","Snow")
Oz <- matrix(c(.5,.25,.25,.5,0,.5,.25,.25,.5),
             nrow=3, byrow=TRUE)
row.names(Oz) <- stateNames; colnames(Oz) <- stateNames
Oz

#      Rain Nice Snow
# Rain 0.50 0.25 0.25 FREE
# Nice 0.50 0.00 0.50 FREE
# Snow 0.25 0.25 0.50

plotmat(Oz,pos = c(1,2),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light yellow",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13,
        main = "")

Oz3 <- Oz %^% 3
round(Oz3,3)

#      Rain  Nice  Snow
# Rain 0.406 0.203 0.391
# Nice 0.406 0.188 0.406
# Snow 0.391 0.203 0.406

u <- c(1/3, 1/3, 1/3)
round(u %*% Oz3,3)
#0.401 0.198 0.401

p <- c(.5,0,.5)
dw <- c(1,rep(0,4),p,0,0,0,p,0,0,0,p,rep(0,4),1)
DW <- matrix(dw,5,5,byrow=TRUE)

DWmc <-new("markovchain",
           transitionMatrix = DW,
           states = c("0","1","2","3","4"),
           name = "Drunkard's Walk")
DWmc
# Drunkard's Walk
# A 5 - dimensional discrete Markov Chain with following states
# 0 1 2 3 4
# The transition matrix (by rows) is defined as follows
# 0 1 2 3 4
# 0 1.0 0.0 0.0 0.0 0.0
# 1 0.5 0.0 0.5 0.0 0.0
# 2 0.0 0.5 0.0 0.5 0.0
# 3 0.0 0.0 0.5 0.0 0.5
# 4 0.0 0.0 0.0 0.0 1.0

# Determine transient states
transientStates(DWmc)
#[1] "1" "2" "3"

# determine absorbing states
absorbingStates(DWmc)
#[1] "0" "4"


# Find Matrix Q
getRQ <- function(M,type="Q"){
  if(length(absorbingStates(M)) == 0) stop("Not Absorbing Matrix")
  tm <- M@transitionMatrix
  d <- diag(tm)
  m <- max(which(d == 1))
  n <- length(d)
  ifelse(type=="Q",
         A <- tm[(m+1):n,(m+1):n],
         A <- tm[(m+1):n,1:m])
  print(tm);
}

# Put DWmc into Canonical Form
P <- canonicForm(DWmc)
P

Q <- getRQ(P)

# Find Fundamental Matrix
I <- diag(dim(Q)[2])
N <- solve(I - Q)
N
# 1 2 3
# 1 1.5 1 0.5
# 2 1.0 2 1.0
# 3 0.5 1 1.5

# Calculate time to absorption
c <- rep(1,dim(N)[2])
u <- N %*% c
u
# 1 3
# 2 4
# 3 3

R <- getRQ(P,"R")
B <- N %*% R
B
# 0 4
# 1 0.75 0.25
# 2 0.50 0.50
# 3 0.25 0.75


# 11.3 Ergodic Markov Chains

# Four methods to get steady states
# Method 1: compute powers on Matrix

round(Oz %^% 6,2)
# Rain Nice Snow
# Rain 0.4 0.2 0.4
# Nice 0.4 0.2 0.4
# Snow 0.4 0.2 0.4

# Method 2: Compute eigenvector of eigenvalue 1

eigenOz <- eigen(t(Oz))
ev <- eigenOz$vectors[,1] / sum(eigenOz$vectors[,1])
ev

# Method 3: compute null space of (P - I)
I <- diag(3)
ns <- nullspace(t(Oz - I))
ns <- round(ns / sum(ns),2)
ns


# Method 4: use function in markovchain package

OzMC<-new("markovchain",
          states=stateNames,
          transitionMatrix=
            matrix(c(.5,.25,.25,.5,0,.5,.25,.25,.5),
                   nrow=3,
                   byrow=TRUE,
                   dimnames=list(stateNames,stateNames)))

steadyStates(OzMC)


# Create a large random regular matrix
randReg <- function(N){
  M <- matrix(runif(N^2,min=1,max=N),nrow=N,ncol=N)
  rowS <- rowSums(M)
  regM <- M/rowS
  return(regM)
}

N <- 5000
M <-randReg(N)
#rowSums(M)

system.time(regMC <- new("markovchain", states = as.character(1:N),
                         transitionMatrix = M,
                         name = "M"))
# user system elapsed
# 98.33 0.82 99.46

system.time(ss <- steadyStates(regMC))
# user system elapsed
# 618.47 0.61 640.05



#sample from regMC
regMCts <- rmarkovchain(n=1000,object=regMC)
regMCtsDf <- as.data.frame(regMCts,stringsAsFactors = FALSE)
regMCtsDf$index <- 1:1000
regMCtsDf$regMCts <- as.numeric(regMCtsDf$regMCts)

library(ggplot2)
p <- ggplot(regMCtsDf,aes(index,regMCts))
p + geom_line(colour="dark red") +
  xlab("time") +
  ylab("state") +
  ggtitle("Random Markov Chain")

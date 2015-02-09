###
### Based on simulation.R from https://github.com/jtleek/swfdr
##
### Simulate 100 different 'journals' where the p-values
### that are reported are all the p-values less than 0.05
###


### Source our functions
###

source("calculateSwfdr.R")
source("journalAnalysisHelp.R")
library(stats4)


##
## Perform the simulation
##

set.seed(23452134)

n=10 #orifginal value 100 set to 10 throughout file to speed up development
pi0 = pi0hat = rep(NA,n) # pi0 is the "true" false discovery rate; pi0hat is its estimate

#shape-parameters for alt-distribution
a = 1
b = 100

#a function for simulating the behaviour of a dishonest/biased/reckless researcher
fudgePvalues = function(pvalues,u,rdist = (function(n) rbeta(n,a,b)),alpha = 0.05){
  
  acceptNull = pvalues > alpha # returns a boolean vector

  #pLarge = pvalues[acceptNull]  # "Large"/"Small" referrs to wetther or not
  pSmall = pvalues[!acceptNull] # the p-values are larger/smaller than 0.05

  m = length(pvalues)
  m0 = length(pSmall)
  toFudge = as.integer(round((m-m0)*u))

  pLarge = rdist(toFudge)

  #resample all values from rdist
  filter = pLarge[>alpha]
  while(sum(filter) > 0){
    pLarge[filter] = rdist(sum(filter))
    filter = pLarge[>alpha]
#    pLarge = c(pLarge[!filter],rdist(sum(filter)))
  }
  
  #pvalues = c(pSmall,pLarge)
  pvalues[acceptNull][1:toFudge] = pLarge
}

for(i in 1:n){

  palt = rbeta(1e3,a,b)
  
  ### Add code for fudging palt-results.
  
  palt = palt[palt < 0.05]
  x = runif(1,0,5)
  pnull = runif(floor(length(palt)*x),0,0.05)
  
  pp = c(pnull,palt)
  
  
  # randomly truncate and round certain entries?
  tt = rbinom(length(pp),size=1,prob=0.2)
  rr = rbinom(length(pp),size=1,prob=0.2)
  rr[tt==1] = 0
  
  pp[rr > 0] = round(pp[rr > 0],2)
  
  
  pi0[i] = length(pnull)/length(pp)
  pi0hat[i] = calculateSwfdr(pp,tt,rr,alpha=1,beta=150,numEmIterations=10)$pi0
  print(i)
  
  #TODO: add code to calculate the u-value
  
}

### Plot our estimate of the swfdr versus the truth. 
###

#pdf(file="all-significant.pdf")
plot(pi0,pi0hat,xlim=c(0,1),ylim=c(0,1),xlab="True swfdr",ylab="Estimated swfdr",pch=22,bg="blue",col="black",cex.axis=1.5,cex.lab=1.5,cex=2)
abline(c(0,1),lwd=3,col="grey ")
#dev.off()
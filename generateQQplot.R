## Generate QQ-plot for the repport (stored in a separate .pdf file).
## Mathias C. Cronjaeger

#################################################################
##     To change output, modify the parameters below           ##

  outputFileName = "QQplot.pdf"

  pi0 = 0.14 # The false discovery rate presumed by the modell

  # the p-values associated with "true" alternatives are
  # presumed to be Beta-distributed
  
  a = 1      # First shape-parameter of the Beta distribution
  b = 100    # Seccond shape parameter of the Beta Distribution

##                                                             ##
#################################################################

load('pvalueData.rda')
pVec = as.vector(pvalueData[,1],mode = 'numeric')
pVec = pVec[pVec < 0.05] #filter out values larger than 0.05

rm(npapers,pvalueData) # don't clogg up memmory with a 12mb object after we are done with it... 

N = length(pVec)
n1 = round(pi0*N)

pFromNull = runif(n1,0,0.05)

pFromAlt = rbeta(20*N,a,b) # 20*N will generate at least N-n1 samples that are < 0.05 with high probability.
pFromAlt = pFromAlt[pFromAlt < 0.05] #filter out values larger than 0.05

if(length(pFromAlt) < N - n1){ #verify that we have enough samples
  print("Warning: not enough samples form alternative! modify program.")
} else {
  pFromAlt = pFromAlt[1:(N-n1)]
}

simPVec = c(pFromNull,pFromAlt)

ylabel = paste("q_(",round(pi0,2),"UNIF + ",round(1-pi0,2),"BETA(1,100) (truncated to < 0.05))")

pdf(file=outputFileName)
qqplot(pVec,simPVec,
       xlab = "q_(Empirical p-values <0.05 from Jager&Leek)",
       ylab = ylabel,
       main = "QQ-plot of scraped P-valued vs. Hypothetical distribution",
       bty="n",pch=".",asp=1,xlim=c(0,0.05),ylim=c(0,0.05))
abline(0,1,col="#BBBBBB")
dev.off()
attach("~/Projects/Gyanu/R/.RData")
data80 <- data80
numTAs <- numTAs
detach(2)

essential <- rep(0,4204) 
essential[sample(1:4204,1850)] <- 1
counts <- sim.mutants(data80[,1],essential,data80[,3],sum(data80[,2])+sum(data80[,4]))

output <- vector("list",5)
output[[1]] <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],n.mcmc=1000,
                       burnin=0,skip=0,startp=1)
output[[2]] <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],n.mcmc=1000,
                       burnin=0,skip=0,startp=0)
for(i in 3:5)
  output[[i]] <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],n.mcmc=1000,
                         burnin=0,skip=0,startp=0.25*(i-2))

moreoutput <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],burnin=500,
                      n.mcmc=50000,skip=0)

require(tseries)
myacf <- acf(moreoutput$n.essential,lag.max=100)

geneprob <- rep(0,4204)
for(i in 1:4) {
  yetmoreoutput <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],burnin=500,
                           n.mcmc=50000,skip=49,return=TRUE,trace=FALSE)
  geneprob <- geneprob + apply(1-yetmoreoutput$output,2,sum)
  rm(yetmoreoutput)
  cat(i,"\n")
}

finalres <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],burnin=500,
                    n.mcmc=500000,skip=49)

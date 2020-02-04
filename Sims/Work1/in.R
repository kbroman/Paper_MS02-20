attach("../.RData")
set.seed(2073870)
output <- vector("list",3)
n.sim <- 1000
for(j in 1:3) output[[j]] <- matrix(ncol=3,nrow=n.sim)
n.mut <- 750
for(i in 1:n.sim) {
  for(j in 1:3) {
    ess <- rep(0,4204)
    ess[sample(1:4204,1051*(j-1))] <- 1
    counts <- sim.mutants(data80[,1],ess,data80[,3],n.mut)
    out <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],burnin=500,
                   skip=9,n.mcmc=20000,trace=FALSE)
    output[[j]][i,] <- out$summary[-2]
    print(c(i,j,output[[j]][i,]))
  }
}

  

attach("../.RData")
set.seed(2073874)
output <- vector("list",3)
n.sim <- 1000
for(j in 1:4) output[[j]] <- matrix(ncol=3,nrow=n.sim)
n.mut <- c(750,1500,3000,4500)
for(i in 1:n.sim) {
  for(j in 1:4) {
    ess <- rep(0,4204)
    ess[sample(1:4204,1051*3)] <- 1
    counts <- sim.mutants(data80[,1],ess,data80[,3],n.mut[j])
    out <- negenes(data80[,1],counts[,1],data80[,3],counts[,2],burnin=500,
                   skip=9,n.mcmc=20000,trace=FALSE)
    output[[j]][i,] <- out$summary[-2]
    print(c(i,j,output[[j]][i,]))
  }
}

  

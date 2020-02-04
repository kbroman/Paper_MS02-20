
# early version of results
#out <- vector("list",5)
#for(i in 1:5)
#  out[[i]] <- read.table(paste("../Sims/Work",i,"/out.txt",sep=""),header=FALSE)
#m <- min(sapply(out,nrow)/c(3,3,3,3,4))
#for(i in 1:5) out[[i]] <- out[[i]][1:(m*c(3,3,3,3,4)[i]),]
# 1, 2, 3, 4 = 750, 1500, 3000, 4500 mutants
# 1,2,3 in 2nd col of each = 25%, 50% or 75% (out of 4204 genes) ess'l
# shit!  actually I did 0%, 25% and 50%
# out #5 contains the results for 75%,
#     with the 2nd col = 1, 2, 3, 4 for no. mutants

temp <- output
rm(output)
out <- vector("list",5)
for(i in 1:5) {
  attach(paste("../Sims/Work", i, "/.RData",sep=""))
  out[[i]] <- output
  detach(2)
}
output <- temp
rm(temp)
m <- 1000


sim.res <- array(dim=c(4,4,m,3))
dimnames(sim.res) <- list(paste(c(0,25,50,75),"%",sep=""),
                          paste(c(750,1500,3000,4500)), 
                          NULL, c("est","lo","high"))
for(i in 1:4) 
  for(j in 1:3) 
    sim.res[j,i,,] <- out[[i]][[j]]
for(i in 1:4)
  sim.res[4,i,,] <- out[[5]][[i]]

sim.bias <- matrix(ncol=12,nrow=2)
sim.cov <- matrix(ncol=12,nrow=3)
sim.len <- matrix(ncol=12,nrow=2)

sim.bias[1,] <- as.numeric(t(apply(sim.res[-1,,,1],c(1,2),mean))) -
  1051*rep(1:3,rep(4,3))
sim.bias[2,] <- as.numeric(t(apply(sim.res[-1,,,1],c(1,2),
                                   function(a) sd(a)/sqrt(length(a)))))
sim.bias <- sim.bias/42.04 # convert to percent

sim.len[1,] <- as.numeric(t(apply(sim.res[-1,,,3]-sim.res[-1,,,2],c(1,2),mean)))
sim.len[2,] <- as.numeric(t(apply(sim.res[-1,,,3]-sim.res[-1,,,2],c(1,2),
                                  function(a) sd(a)/sqrt(length(a)))))
sim.len <- sim.len/42.04 # convert to percent

truth <- array(rep(1051*(1:3),4*m),dim=c(3,4,m))
sim.cov[1,] <- as.numeric(t(apply((sim.res[-1,,,2] <= truth) &
                                  (sim.res[-1,,,3] >= truth), c(1,2),sum)))
for(i in 1:ncol(sim.cov)) 
  sim.cov[2:3,i] <- binom.test(sim.cov[1,i],dim(sim.res)[3])$conf.int*100
sim.cov[1,] <- sim.cov[1,]/dim(sim.res)[3]*100
rm(truth,out,i,j)


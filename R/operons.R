attach("/home/kbroman/Projects/Gyanu/Operons/.RData")
op.output <- op.output
detach(2)
temp <- sapply(op.output,function(a) (a[,3]-a[,1])/4207)
op.bias <- rbind(bias=apply(temp,2,mean)*100,
                 se=apply(temp,2,function(a) sd(a*100)/sqrt(length(a))))
colnames(op.bias) <- paste(seq(20,80,len=5))
rm(temp)
temp <- sapply(op.output,function(a) sum(a[,1] >= a[,5] & a[,1] <= a[,6]))
op.cov <- matrix(ncol=5,nrow=3)
op.cov[1,] <- temp/10
for(i in 1:5) op.cov[2:3,i] <- binom.test(op.cov[1,i]*10,1000)$conf.int*100
colnames(op.cov) <- paste(seq(20,80,len=5))
rownames(op.cov) <- c("cov","lo","hi")

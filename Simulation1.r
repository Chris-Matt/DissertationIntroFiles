#Simple simulation for introduction
#Part 1
library(foreach)
library(doParallel)
store=store1=store2=storesd=NULL;

myfn1=function(nt){
  print(nt) 
  set.seed(nt) 
#sample size
n=500
#probability for x
px=.55
X=rbinom(n,1,px)
beta0=-1
beta1=0.5
linear.pt=beta0+beta1*X
py.x=1/(1+exp(-linear.pt))
Y=rbinom(n,1,py.x)

#Consider W, misclassified version of X
pv=0.7
V=rbinom(n,1,pv)
W=X*V+(1-V)*(1-X)

#Run regressions on X, Y and W
yx.out=glm(Y~X,family=binomial)
yx.coef=summary(yx.out)$coef[,1]
yx.sd=summary(yx.out)$coef[,2]
yw.out=glm(Y~W,family=binomial)
yw.coef=summary(yw.out)$coef[,1]
yw.sd=summary(yw.out)$coef[,2]
ourcoef=c(as.numeric(yx.coef), as.numeric(yw.coef))
oursd=c(as.numeric(yx.sd), as.numeric(yw.sd))
write.table(matrix(as.numeric(c(nt, ourcoef,  oursd)), nrow=1), 'intro_sim1.txt', row.names=F,  col.names=F, append=T)
return(c(ourcoef,  oursd))}

library(doParallel) 
no_cores <- detectCores() - 1 
registerDoParallel(cores=no_cores) 
cl <- makeCluster(no_cores, type="FORK") 
store <- foreach(it = 1:1000, .combine="rbind") %dopar% myfn1(nt = it)

######################estimate bias
#bias
#beta0
round(mean(store[,1]-(beta0)),digits=3)
round(mean(store[,3]-(beta0)),digits=3)
#beta1
round(mean(store[,2]-(beta1)),digits=3)
round(mean(store[,4]-(beta1)),digits=3)
#std deviation
round(apply(store[,1:4],2,sd),digits=3)
round(apply(store[,5:8],2, mean),digits=3)
########################Power
dim(store)
beta0=-1
beta1=0.5
n=500
txb0=abs((store[,1])/((store[5])/sqrt(n)))
txb1=abs((store[,2])/((store[6])/sqrt(n)))
twb0=abs((store[,3])/((store[7])/sqrt(n)))
twb1=abs((store[,4])/((store[8])/sqrt(n)))
head(txb0)

(length(txb0[txb0>qt(.975,499)])/1000)*100
(length(txb1[txb1>qt(.975,499)])/1000)*100
(length(twb0[twb0>qt(.975,499)])/1000)*100
(length(twb1[twb1>qt(.975,499)])/1000)*100
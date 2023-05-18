library("magic")
library("mvtnorm")
library("lars")
library("knockoff")
library("grpreg")
library("far")
library("methods")
library("glmnet")
library(seqknockoff)

####fitting
myest_stack<-function(datal,q,method="Max"){
  ##method Max: sign max
  ##method Diff: difference
  data=datal$data
  X=data[,-c(1,2)]
  y=data[,2]
  group=datal$group1
  XKnock <- group_seq_knock(X,group)
  p=max(datal$group1) ### how many group
  ###group: write function later to deal with each categorical variable has different levels
  which.factor <- as.numeric(which(sapply(X, is.factor)))
  LL <- nlevels(X[,which.factor[1]])
  group_cate=rep(group[which.factor],each=LL-1) ### if the group size are different, the number should be changed
  groupx <- c(group_cate, group[-which.factor])
  group <-groupx[order(groupx)]
  
  ### using group lasso for the coefficient
  XC<- model.matrix( ~ ., X)[, -1]
  XKnockC <- model.matrix( ~ ., XKnock)[, -1]
  
  fit<-grpreg(as.matrix(cbind(XC,XKnockC)),y,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)
  
  Z=abs(generateZ(fit)[(1:p)])
  Ztilde=abs(generateZ(fit)[p+(1:p)])
  
  if (method=="Max"){
    W=pmax(Z,Ztilde)*(-1)^(Z<=Ztilde)
  }
  if (method=="Diff"){
    W=Z-Ztilde
  }
  mythred=knockoff.threshold(W,fdr=q,offset=1)
  myselect=which(W>=mythred)
  return(list(myselect=myselect,ZZ=cbind(Z,Ztilde)))
}
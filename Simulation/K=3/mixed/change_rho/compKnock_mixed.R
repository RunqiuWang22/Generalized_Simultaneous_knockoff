library("magic")
library("mvtnorm")
library("lars")
library("knockoff")
library("grpreg")
library("far")
library("methods")
library("glmnet")
library(seqknockoff)

generateZ<-function(fit){
  # p is the number of group
  res<-abs(fit$beta[-1,])>0
  gg <- as.numeric(fit$group)
  N <- dim(res)[1]/2 #the number of predictors
  p=max(gg)/2 #the number of group
  np<-as.vector(table(gg))[1:p]#the number of predictors in each group
  cnp<-c(0,cumsum(np))
  
  ###decide how many variables are nonzero within each group for each lamda
  ee<-apply(res,2,cumsum)
  dd<-ee[c(cnp[-1],cnp[-1]+N),]-rbind(0,ee[c(cnp[-(p+1)],cnp[-(p+1)]+N),])
  ###find the best beta
  tmpf<-function(v){
    s<-0
    a<-which(v>0)
    if (length(a)>0){
      s<-fit$lambda[min(a)]
    }
    s
  }
  tmp<-as.vector(apply(dd,1,tmpf))
  #W<-(tmp[1:p]+tmp[p+1:p])*((tmp[1:p]-tmp[p+1:p])>0)-tmp[p+1:p]
  tmp
}

####fitting

myest<-function(datal){
	##method Max: sign max
	##method Diff: difference
  data<-datal$data
  group<-datal$group1
  
  data1<-data[which(data$C==1),]
  data2<-data[which(data$C==2),]
  data3<-data[which(data$C==3),]
 
  p=max(datal$group1) ### how many group
  X1<-data1[,-c(1:2)]
  X2<-data2[,-c(1:2)]
  X3<-data3[,-c(1:2)]
  
  
  y1<-data1[,2]
  y2<-data2[,2]
  y3<-data3[,2]
  
  X1Knock<-group_seq_knock(X1,group)
  X2Knock<-group_seq_knock(X2,group)
  X3Knock<-group_seq_knock(X3,group)
  ###group
  which.factor <- as.numeric(which(sapply(X1, is.factor)))
  LL <- nlevels(X1[,which.factor[1]]) ##the levels for each column are the same currently
  group_cate=rep(group[which.factor],each=LL-1) ### if the group size are different, the number should be changed
  groupx <- c(group_cate, group[-which.factor])
  group <-groupx[order(groupx)]
  
  ### using group lasso for the coefficient
  X1C <- model.matrix( ~ ., X1)[, -1]
  X1KnockC <- model.matrix( ~ ., X1Knock)[, -1]
  X2C <- model.matrix( ~ ., X2)[, -1]
  X2KnockC <- model.matrix( ~ ., X2Knock)[, -1]
  X3C <- model.matrix( ~ ., X3)[, -1]
  X3KnockC <- model.matrix( ~ ., X3Knock)[, -1]

  
  fit1<-grpreg(as.matrix(cbind(X1C,X1KnockC)),y1,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)
  fit2<-grpreg(as.matrix(cbind(X2C,X2KnockC)),y2,group=c(group,group+p),penalty="grLasso",family="binomial",nlambda=1000)
  fit3<-grpreg(as.matrix(cbind(X3C,X3KnockC)),y3,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)
  
  Z1=abs(generateZ(fit1)[(1:p)])
  Z1tilde=abs(generateZ(fit1)[p+(1:p)])
  Z2=abs(generateZ(fit2)[(1:p)])
  Z2tilde=abs(generateZ(fit2)[p+(1:p)])
  Z3=abs(generateZ(fit3)[(1:p)])
  Z3tilde=abs(generateZ(fit3)[p+(1:p)])
  
  return(cbind(Z1,Z1tilde,Z2,Z2tilde,Z3,Z3tilde))
}


myresult <-function(coeff,q,choose,method){
  Z1=coeff[,1]
  Z1tilde=coeff[,2]
  Z2=coeff[,3]
  Z2tilde=coeff[,4]
  Z3=coeff[,5]
  Z3tilde=coeff[,6]
  if (choose=="simultaneous"){
    Z=Z1*Z2*Z3+Z1tilde*Z2tilde*Z3+Z1tilde*Z3tilde*Z2+Z2tilde*Z3tilde*Z1
    Ztilde=Z1tilde*Z2*Z3+Z2tilde*Z1*Z3+Z3tilde*Z1*Z2+Z1tilde*Z2tilde*Z3tilde
    if (method=="Max"){
      W=pmax(Z,Ztilde)*(-1)^(Z<=Ztilde)
    }
    if (method=="Diff"){
      W=Z-Ztilde
    }
    
    mythred=knockoff.threshold(W,fdr=q,offset=1)
    myselect=which(W>=mythred)
  }
  
  if (choose=="intersection"){
    if (method=="Max"){
      W1=pmax(Z1,Z1tilde)*(-1)^(Z1<=Z1tilde)
      W2=pmax(Z2,Z2tilde)*(-1)^(Z2<=Z2tilde)
      W3=pmax(Z3,Z3tilde)*(-1)^(Z3<=Z3tilde)
    }
    if (method=="Diff"){
      W1=Z1-Z1tilde
      W2=Z2-Z2tilde
      W3=Z3-Z3tilde
    }
    mythred1=knockoff.threshold(W1,fdr=q,offset=1)
    myselect1=which(W1>=mythred1)
    mythred2=knockoff.threshold(W2,fdr=q,offset=1)
    myselect2=which(W2>=mythred2)
    mythred3=knockoff.threshold(W3,fdr=q,offset=1)
    myselect3=which(W3>=mythred3)
    
    myselect=intersect(intersect(myselect1,myselect2),myselect3)
  }
  
  return(myselect)
  #list(myselect=myselect,Z=Z,Ztilde=Ztilde,Z1=Z1,Z1tilde=Z1tilde,Z2=Z2,Z2tilde=Z2tilde)
}
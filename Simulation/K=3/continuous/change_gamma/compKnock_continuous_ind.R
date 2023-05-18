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

myknockoff <- function(datal){
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
  X1Knock<-knockoffs_seq(X1)
  X2Knock<-knockoffs_seq(X2)
  X3Knock<-knockoffs_seq(X3)
  
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
  
  data1X=cbind(y1,as.matrix(cbind(X1C,X1KnockC)))
  data2X=cbind(y2,as.matrix(cbind(X2C,X2KnockC)))
  data3X=cbind(y3,as.matrix(cbind(X3C,X3KnockC)))
  return(list(data1X=data1X,data2X=data2X,data3X=data3X,group=group))
}

### fit the model use two method: group lasso and lasso
ffit = function(datal, choose){
X1 = as.matrix(datal$data1X[,-1])
y1 = datal$data1X[,1]

X2 = as.matrix(datal$data2X[,-1])
y2 = datal$data2X[,1]

X3 = as.matrix(datal$data3X[,-1])
y3 = datal$data3X[,1]
  
group=datal$group
p=max(group)
N=dim(X1)[2]/2 ## dimension of X after transfering into dummy variables

if (choose=="grlasso"){
  fit1<-grpreg(X1,y1,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)
  fit2<-grpreg(X2,y2,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)
  fit3<-grpreg(X3,y3,group=c(group,group+p),penalty="grLasso",family="gaussian",nlambda=1000)

  Z1=abs(generateZ(fit1)[(1:p)])
  Z1tilde=abs(generateZ(fit1)[p+(1:p)])
  Z2=abs(generateZ(fit2)[(1:p)])
  Z2tilde=abs(generateZ(fit2)[p+(1:p)])
  Z3=abs(generateZ(fit3)[(1:p)])
  Z3tilde=abs(generateZ(fit3)[p+(1:p)])
  
}

if (choose=="lasso"){
  cvfit1<-cv.glmnet(X1,y1,family="gaussian",standardize=FALSE,intercept=TRUE) 
  fit1<-glmnet(X1,y1,family="gaussian",standardize=FALSE,intercept=TRUE,lambda=(cvfit1$lambda.min))
  tmp1=as.numeric(coef(fit1))[-1]
  
  cvfit2<-cv.glmnet(X2,y2,family="gaussian",standardize=FALSE,intercept=TRUE) 
  fit2<-glmnet(X2,y2,family="gaussian",standardize=FALSE,intercept=TRUE,lambda=(cvfit2$lambda.min))
  tmp2=as.numeric(coef(fit2))[-1]
  
  cvfit3<-cv.glmnet(X3,y3,family="gaussian",standardize=FALSE,intercept=TRUE) 
  fit3<-glmnet(X3,y3,family="gaussian",standardize=FALSE,intercept=TRUE,lambda=(cvfit3$lambda.min))
  tmp3=as.numeric(coef(fit3))[-1]
  
  Z1=abs(tmp1[1:N])
  Z1tilde=abs(tmp1[N+1:N])
  Z2=abs(tmp2[1:N])
  Z2tilde=abs(tmp2[N+1:N])
  Z3=abs(tmp3[1:N])
  Z3tilde=abs(tmp3[N+1:N])
  
}

  return(cbind(Z1,Z1tilde,Z2,Z2tilde,Z3,Z3tilde))
}


myresult <-function(coeff,q,choose,method,p){
  ### p is the number of group
  Z1=coeff[,1]
  Z1tilde=coeff[,2]
  Z2=coeff[,3]
  Z2tilde=coeff[,4]
  Z3=coeff[,5]
  Z3tilde=coeff[,6]
  
  if (choose=="grlasso"){
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
  
  if (choose=="lasso"){
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
    
    N=length(Z1)
    p=p #the number of group
    pi=N/p
    np<-rep(pi,p)
    cnp<-c(0,cumsum(np))
    
    myselect1<-rep(0,p)
    for (i in 1:p){
      myselect1[i]<-length(which(myselect%in%((cnp[i]+1):cnp[i+1])))>0
    }
    myselect<-which(myselect1>0)
}
	return(myselect)
	#list(myselect=myselect,Z=Z,Ztilde=Ztilde,Z1=Z1,Z1tilde=Z1tilde,Z2=Z2,Z2tilde=Z2tilde)
}
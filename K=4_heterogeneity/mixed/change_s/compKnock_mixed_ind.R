library("magic")
library("mvtnorm")
library("lars")
library("knockoff")
library("grpreg")
library("far")
library("methods")
library("glmnet")
library(seqknockoff)

expand.group <- function (X,group){
  which.factor <- as.numeric(which(sapply(X, is.factor)))
  if (length(which.factor)==0) {group=group}
  else if (length(which.factor)>0){
    LL <- nlevels(X[,which.factor[1]])
    group_cate=rep(group[which.factor],each=LL-1) ### if the group size are different, the number should be changed
    groupx <- c(group_cate, group[-which.factor])
    group <-groupx[order(groupx)]
  }
  return(group)
}

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
  group<-datal$group
  
  data1<-data[[1]]
  data2<-data[[2]]
  data3<-data[[3]]
  data4<-data[[4]]
  
  group1<-group[[1]]
  group2<-group[[2]]
  group3<-group[[3]]
  group4<-group[[4]]
  
  p=max(group1) ### how many group: p are the same across datasets
  
  X1<-data1[,-1]
  X2<-data2[,-1]
  X3<-data3[,-1]
  X4<-data4[,-1]
  
  
  y1<-data1[,1]
  y2<-data2[,1]
  y3<-data3[,1]
  y4<-data4[,1]
  
  X1Knock<-knockoffs_seq(X1)
  X2Knock<-knockoffs_seq(X2)
  X3Knock<-knockoffs_seq(X3)
  X4Knock<-knockoffs_seq(X4)
  ###expand group
  egroup1 <- expand.group(X1,group1)
  egroup2 <- expand.group(X2,group2)
  egroup3 <- expand.group(X3,group3)
  egroup4 <- expand.group(X4,group4)
  
  ### using group lasso for the coefficient
  X1C <- model.matrix( ~ ., X1)[, -1]
  X1KnockC <- model.matrix( ~ ., X1Knock)[, -1]
  X2C <- model.matrix( ~ ., X2)[, -1]
  X2KnockC <- model.matrix( ~ ., X2Knock)[, -1]
  X3C <- model.matrix( ~ ., X3)[, -1]
  X3KnockC <- model.matrix( ~ ., X3Knock)[, -1]
  X4C <- model.matrix( ~ ., X4)[, -1]
  X4KnockC <- model.matrix( ~ ., X4Knock)[, -1]
  
  data1X=cbind(y1,as.matrix(cbind(X1C,X1KnockC)))
  data2X=cbind(y2,as.matrix(cbind(X2C,X2KnockC)))
  data3X=cbind(y3,as.matrix(cbind(X3C,X3KnockC)))
  data4X=cbind(y4,as.matrix(cbind(X4C,X4KnockC)))
  return(list(data1X=data1X,data2X=data2X,data3X=data3X,data4X=data4X,egroup1=egroup1,egroup2=egroup2,egroup3=egroup3,egroup4=egroup4))
}

### fit the model use two method: group lasso and lasso
ffit = function(datal, choose){
X1 = as.matrix(datal$data1X[,-1])
y1 = datal$data1X[,1]

X2 = as.matrix(datal$data2X[,-1])
y2 = datal$data2X[,1]

X3 = as.matrix(datal$data3X[,-1])
y3 = datal$data3X[,1]

X4 = as.matrix(datal$data4X[,-1])
y4 = datal$data4X[,1]
  
group1=datal$egroup1
group2=datal$egroup2
group3=datal$egroup3
group4=datal$egroup4

p=max(group1)
N1=length(group1) ## dimension of X after transfering into dummy variables
N2=length(group2)
N3=length(group3)
N4=length(group4)

if (choose=="grlasso"){
  fit1<-grpreg(X1,y1,group=c(group1,group1+p),penalty="grLasso",family="gaussian",nlambda=1000)
  fit2<-grpreg(X2,y2,group=c(group2,group2+p),penalty="grLasso",family="binomial",nlambda=1000)
  fit3<-grpreg(X3,y3,group=c(group3,group3+p),penalty="grLasso",family="gaussian",nlambda=1000)
  fit4<-grpreg(X4,y4,group=c(group4,group4+p),penalty="grLasso",family="binomial",nlambda=1000)

  Z1=abs(generateZ(fit1)[(1:p)])
  Z1tilde=abs(generateZ(fit1)[p+(1:p)])
  Z2=abs(generateZ(fit2)[(1:p)])
  Z2tilde=abs(generateZ(fit2)[p+(1:p)])
  Z3=abs(generateZ(fit3)[(1:p)])
  Z3tilde=abs(generateZ(fit3)[p+(1:p)])
  Z4=abs(generateZ(fit4)[(1:p)])
  Z4tilde=abs(generateZ(fit4)[p+(1:p)])
  
}

if (choose=="lasso"){
  cvfit1<-cv.glmnet(X1,y1,family="gaussian",standardize=FALSE,intercept=TRUE) 
  fit1<-glmnet(X1,y1,family="gaussian",standardize=FALSE,intercept=TRUE,lambda=(cvfit1$lambda.min))
  tmp1=as.numeric(coef(fit1))[-1]
  
  cvfit2<-cv.glmnet(X2,y2,family="binomial",standardize=FALSE,intercept=TRUE) 
  fit2<-glmnet(X2,y2,family="binomial",standardize=FALSE,intercept=TRUE,lambda=(cvfit2$lambda.min))
  tmp2=as.numeric(coef(fit2))[-1]
  
  cvfit3<-cv.glmnet(X3,y3,family="gaussian",standardize=FALSE,intercept=TRUE) 
  fit3<-glmnet(X3,y3,family="gaussian",standardize=FALSE,intercept=TRUE,lambda=(cvfit3$lambda.min))
  tmp3=as.numeric(coef(fit3))[-1]
  
  cvfit4<-cv.glmnet(X4,y4,family="binomial",standardize=FALSE,intercept=TRUE) 
  fit4<-glmnet(X4,y4,family="binomial",standardize=FALSE,intercept=TRUE,lambda=(cvfit4$lambda.min))
  tmp4=as.numeric(coef(fit4))[-1]
  
  Z1=abs(tmp1[1:N1])
  Z1tilde=abs(tmp1[N1+(1:N1)])
  Z2=abs(tmp2[1:N2])
  Z2tilde=abs(tmp2[N2+(1:N2)])
  Z3=abs(tmp3[1:N3])
  Z3tilde=abs(tmp3[N3+(1:N3)])
  Z4=abs(tmp4[1:N4])
  Z4tilde=abs(tmp4[N4+(1:N4)])
  
}

  return(list(Z1,Z1tilde,Z2,Z2tilde,Z3,Z3tilde,Z4,Z4tilde))
}


myresult <-function(coeff,q,choose,method,p){
  ### p is the number of group
  Z1=coeff[[1]]
  Z1tilde=coeff[[2]]
  Z2=coeff[[3]]
  Z2tilde=coeff[[4]]
  Z3=coeff[[5]]
  Z3tilde=coeff[[6]]
  Z4=coeff[[7]]
  Z4tilde=coeff[[8]]
  if (choose=="grlasso"){
	 if (method=="Diff"){
	   W=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)*(Z4-Z4tilde)
	 }

	mythred=knockoff.threshold(W,fdr=q,offset=1)
	myselect=which(W>=mythred)
  }
  
  if (choose=="lasso"){
    if (method=="Diff"){
      W=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)*(Z4-Z4tilde)
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
library(MASS)
### function generate group and covariate matrix###
###N is number of predictors; pi is number of predictors in each group;
group_covariate_generate<-function(N,pi,gamma,rho){
  p=N/pi #number of groups 
  np<-rep(pi,p)
  cnp<-c(0,cumsum(np))
  index<-rep(NA,N)
  Sigma<-matrix(data=gamma*rho,nrow=N,ncol=N)
  for (i in 1:p){
    aa<-(cnp[i]+1):cnp[i+1]
    index[aa]<-i
    Sigma[aa,aa]<-rho
  }
  diag(Sigma)<-1
  return(list(group=index,Sigma=Sigma))
}

generate_X<-function(n,p,pi,gamma,rho,pb=0.25,LL=3){
  ### n is sample size
  ### p is the number of group
  ### pi is the number of group
  ### gamma rho*gamma is the between group correlation
  ### rho within-group correlation 
  ### the proportion of categorical variable
  ### LL the level of categorical variable
  N=pi*p
  group<-group_covariate_generate(N=N,pi=pi,gamma=gamma,rho=rho)$group
  Sigma<-group_covariate_generate(N=N,pi=pi,gamma=gamma,rho=rho)$Sigma
  
  X<-as.data.frame(mvrnorm(n=n,mu=rep(0,N),Sigma=Sigma))
  
  ###randomly choose the index to generate categorical variable in each group
  ###in each data, the index of categorical variable can be different for different populations
  ###since we need to compare with pooling method, we set all the index of categorical variables are the same in different populations.
  
  if (pb>0){        
    ### the index of categorical variable
    index_b <- list()
    ### number of categotical variable in each group
    T <- pi*pb
    for (i in 1:p){
      index_b[[i]] <- sample(1:pi,size=T,replace=F)+ (i-1)*pi
      for (j in 1:length(index_b[[i]])) {
        newgroup <- rep(group[index_b[[i]][j]],(LL-2))
        X[,index_b[[i]][j]] <- cut(X[,index_b[[i]][j]],breaks=quantile(X[,index_b[[i]][j]],probs=seq(0, 1, length.out = LL + 1)),include.lowest=T, labels=1:LL)
      }
    }
  }  
  return(list(X=X,group=group))
  #which.factor <- as.numeric(which(sapply(X, is.factor)))
}

datagen<-function(p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4){
  ###n1 sample size in population 1
  ###n2 sample size in population 2
  ###n3 sample size in population 3
  ###n4 sample size in population 4
  ###N number of predictors; 
  ###pi number of predictors in each group in each population: are the same due to pooling method
  ###ps0 proportion of true signals
  ###ps1 proportion of signals shown in population 1 only
  ###ps2 proportion of signals shown in population 2 only
  ###ps3 proportion of signals shown in population 3 only
  ###ps4 proportion of signals shown in population 4 only
  
  ###ps12 proportion of signals shown in population 1 and 2 only
  ###ps13 proportion of signals shown in population 1 and 3 only
  ###ps23 proportion of signals shown in population 2 and 3 only
  ###ps14 proportion of signals shown in population 1 and 4 only
  ###ps24 proportion of signals shown in population 2 and 4 only
  ###ps34 proportion of signals shown in population 3 and 4 only
  
  ###ps123 proportion of signals shown in population 1, 2 and 3 only
  ###ps124 proportion of signals shown in population 1, 2 and 4 only
  ###ps134 proportion of signals shown in population 1, 3 and 4 only
  ###ps234 proprotion of signals shown in population 2, 3 and 4 only
  
  ###rho1 within-group correlation for design matrix in population 1
  ###rho2 within-group correlation for design matrix in population 2
  ###rho3 within-group correlation for design matrix in population 3
  ###rho4 within-group correlation for design matrix in population 4
  
  ###gamma1 rho1*gamma1 is the between group correlation for design matrix in population 1
  ###gamma2 rho2*gamma2 is the between group correlation for design matrix in population 2
  ###gamma3 rho3*gamma3 is the between group correlation for design matrix in population 3
  ###gamma4 rho4*gamma4 is the between group correlation for design matrix in population 4
  
  ###sigma1 noise parameter for population 1
  ###sigma2 noise parameter for population 2
  ###sigma3 noise parameter for population 3
  ###sigma4 noise parameter for population 4
  
  ###samesig=1 true signals magnitude same for both data
  ###samesig=0 true signals magnitude different for both data
  ###effect scale multiplier
  ###pb the proportion of categorical variable
  ###LL the level of categorical variable
  N1 <- pi1*p
  N2 <- pi2*p
  N3 <- pi3*p
  N4 <- pi4*p
  
  X1 <- generate_X(n=n1,p=p,pi=pi1,gamma=gamma1,rho=rho1,pb=pb1,LL=LL1)$X
  X2 <- generate_X(n=n2,p=p,pi=pi2,gamma=gamma2,rho=rho2,pb=pb2,LL=LL2)$X
  X3 <- generate_X(n=n3,p=p,pi=pi3,gamma=gamma3,rho=rho3,pb=pb3,LL=LL3)$X
  X4 <- generate_X(n=n4,p=p,pi=pi4,gamma=gamma4,rho=rho4,pb=pb4,LL=LL4)$X
  
  group1 <- generate_X(n=n1,p=p,pi=pi1,gamma=gamma1,rho=rho1,pb=pb1,LL=LL1)$group
  group2 <- generate_X(n=n2,p=p,pi=pi2,gamma=gamma2,rho=rho2,pb=pb2,LL=LL2)$group
  group3 <- generate_X(n=n3,p=p,pi=pi3,gamma=gamma3,rho=rho3,pb=pb3,LL=LL3)$group
  group4 <- generate_X(n=n4,p=p,pi=pi4,gamma=gamma4,rho=rho4,pb=pb4,LL=LL4)$group
  
  RX1 <- model.matrix( ~ ., X1)[, -1] ### transfer design X into dummy matrix
  RX2 <- model.matrix( ~ ., X2)[, -1]
  RX3 <- model.matrix( ~ ., X3)[, -1]
  RX4 <- model.matrix( ~ ., X4)[, -1]
  
  ### new group after creating the dummy variable
  new_pi1 <- (pi1*pb1)*(LL1-1) + pi1*(1-pb1)
  new_pi2 <- (pi2*pb2)*(LL2-1) + pi2*(1-pb2)
  new_pi3 <- (pi3*pb3)*(LL3-1) + pi3*(1-pb3)
  new_pi4 <- (pi4*pb4)*(LL4-1) + pi4*(1-pb4)
  
  s0=round(p*ps0,0) 
  s1=round(p*ps1,0)
  s2=round(p*ps2,0)
  s3=round(p*ps3,0)
  s4=round(p*ps4,0)
  s12=round(p*ps12,0)
  s13=round(p*ps13,0)
  s14=round(p*ps14,0)
  s23=round(p*ps23,0)
  s24=round(p*ps24,0)
  s34=round(p*ps34,0)
  s123=round(p*ps123,0)
  s124=round(p*ps124,0)
  s134=round(p*ps134,0)
  s234=round(p*ps234,0)
  
  b0.1<-runif(s0)*scale
  b0.2<-runif(s0)*scale
  b0.3<-runif(s0)*scale
  b0.4<-runif(s0)*scale
  if (samesig){b0.2<-b0.1;b0.3<-b0.1;b0.4<-b0.1}
  
  b1<-runif(s1)
  b2<-runif(s2)
  b3<-runif(s3)
  b4<-runif(s4)
  b12.1<-runif(s12)
  b12.2<-runif(s12)
  b13.1<-runif(s13)
  b13.3<-runif(s13)
  b14.1<-runif(s14)
  b14.4<-runif(s14)
  b23.2<-runif(s23)
  b23.3<-runif(s23)
  b24.2<-runif(s24)
  b24.4<-runif(s24)
  b34.3<-runif(s34)
  b34.4<-runif(s34)
  
  if (samesig){b12.2<-b12.1;b13.3<-b13.1;b14.4<-b14.1;b23.3<-b23.2;b24.4<-b24.2;b34.3<-b34.4}
  
  b123.1<-runif(s123)
  b123.2<-runif(s123)
  b123.3<-runif(s123)
  
  b124.1<-runif(s124)
  b124.2<-runif(s124)
  b124.4<-runif(s124)
  
  b134.1<-runif(s134)
  b134.3<-runif(s134)
  b134.4<-runif(s134)
  
  b234.2<-runif(s234)
  b234.3<-runif(s234)
  b234.4<-runif(s234)
  
  if (samesig){b123.1<-b123.2<-b123.3;b124.1<-b124.2<-b124.4;b134.1<-b134.3<-b134.4; b234.2<-b234.3<-b234.4}
  
  ss0<-sample(c(-1,1),s0,prob=c(0.5,0.5),replace=TRUE)
  ss1<-sample(c(-1,1),s1,prob=c(0.5,0.5),replace=TRUE)
  ss2<-sample(c(-1,1),s2,prob=c(0.5,0.5),replace=TRUE)
  ss3<-sample(c(-1,1),s3,prob=c(0.5,0.5),replace=TRUE)
  ss4<-sample(c(-1,1),s4,prob=c(0.5,0.5),replace=TRUE)
  
  ss12<-sample(c(-1,1),s12,prob=c(0.5,0.5),replace=TRUE)
  ss13<-sample(c(-1,1),s13,prob=c(0.5,0.5),replace=TRUE)
  ss14<-sample(c(-1,1),s14,prob=c(0.5,0.5),replace=TRUE)
  ss23<-sample(c(-1,1),s23,prob=c(0.5,0.5),replace=TRUE)
  ss24<-sample(c(-1,1),s24,prob=c(0.5,0.5),replace=TRUE)
  ss34<-sample(c(-1,1),s34,prob=c(0.5,0.5),replace=TRUE)
  
  ss123<-sample(c(-1,1),s123,prob=c(0.5,0.5),replace=TRUE)
  ss124<-sample(c(-1,1),s124,prob=c(0.5,0.5),replace=TRUE)
  ss134<-sample(c(-1,1),s134,prob=c(0.5,0.5),replace=TRUE)
  ss234<-sample(c(-1,1),s234,prob=c(0.5,0.5),replace=TRUE)
  
  beta1<-c(rep(c(b0.1*ss0,b1*ss1,0*ss2,0*ss3,0*ss4,b12.1*ss12,b13.1*ss13,b14.1*ss14,0*ss23,0*ss24,0*ss34,b123.1*ss123,b124.1*ss124,b134.1*ss134,     0*ss234),each=new_pi1),rep(0,(p-s0-s1-s2-s3-s4-s12-s13-s14-s23-s24-s34-s123-s124-s234-s134)*new_pi1))
  beta2<-c(rep(c(b0.2*ss0,0*ss1,b2*ss2,0*ss3,0*ss4,b12.2*ss12,0*ss13,0*ss14,b23.2*ss23,b24.2*ss24,0*ss34,b123.2*ss123,b124.2*ss124,     0*ss134,b234.2*ss234),each=new_pi2),rep(0,(p-s0-s1-s2-s3-s4-s12-s13-s14-s23-s24-s34-s123-s124-s234-s134)*new_pi2))
  beta3<-c(rep(c(b0.3*ss0,0*ss1,0*ss2,b3*ss3,0*ss4,0*ss12,b13.3*ss13,0*ss14,b23.3*ss23,0*ss24,b34.3*ss34,b123.3*ss123,     0*ss124,b134.3*ss134,b234.3*ss234),each=new_pi3),rep(0,(p-s0-s1-s2-s3-s4-s12-s13-s14-s23-s24-s34-s123-s124-s234-s134)*new_pi3))
  beta4<-c(rep(c(b0.4*ss0,0*ss1,0*ss2,0*ss3,b4*ss4,0*ss12,0*ss13,b14.4*ss14,0*ss23,b24.4*ss24,b34.4*ss34,     0*ss123,b124.4*ss124,b134.4*ss134,b234.4*ss234),each=new_pi4),rep(0,(p-s0-s1-s2-s3-s4-s12-s13-s14-s23-s24-s34-s123-s124-s234-s134)*new_pi4))
  
  
  y1<-RX1%*%beta1+rnorm(n1)*sigma1
  y2<-RX2%*%beta2+rnorm(n2)*sigma2
  y3<-RX3%*%beta3+rnorm(n3)*sigma3
  y4<-RX4%*%beta4+rnorm(n4)*sigma4
  ####dichotomize y2 (same as probit model)
  y2<-as.numeric(y2>0)
  y4<-as.numeric(y4>0)
  
  data1<-cbind(y1,X1)
  data2<-cbind(y2,X2)
  data3<-cbind(y3,X3)
  data4<-cbind(y4,X4)
  names(data1)=c("Y",paste("X",1:N1,sep=""))
  names(data2)=c("Y",paste("X",1:N2,sep=""))
  names(data3)=c("Y",paste("X",1:N3,sep=""))
  names(data4)=c("Y",paste("X",1:N4,sep=""))
  
  data<-list(data1,data2,data3,data4)
  group<-list(group1,group2,group3,group4)
  datal <- list(data=data,group=group)
}

#datal=datagen(p=20,n1=1000,n2=1000,n3=1000,n4=1000,pi1=4,pi2=8,pi3=4,pi4=8,ps0=0.3,ps1=0.05,ps2=0.05,ps3=0.05,ps4=0.05,ps12=0.05,ps13=0.05,ps14=0,ps23=0,ps24=0,ps34=0,rho1=0.5,rho2=0.5,rho3=0.5,rho4=0.5,gamma1=0.05,gamma2=0.05,gamma3=0.05,gamma4=0.05,sigma1=1,sigma2=2,sigma3=1,sigma4=2,samesig=0,scale=2,pb1=0.25,pb2=0,pb3=0.5,pb4=0.25,LL1=3,LL2=0,LL3=2,LL4=2)




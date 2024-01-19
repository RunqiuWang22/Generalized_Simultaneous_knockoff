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

generate_X<-function(n,N,pi,gamma,rho,pb=0.25,LL=3,seed){
  ### n is sample size
  ### N is the number of predictor
  ### pi is the number of group
  ### gamma rho*gamma is the between group correlation
  ###rho within-group correlation 
  p<-N/pi
  group<-group_covariate_generate(N=N,pi=pi,gamma=gamma,rho=rho)$group
  Sigma<-group_covariate_generate(N=N,pi=pi,gamma=gamma,rho=rho)$Sigma
  
  X<-as.data.frame(mvrnorm(n=n,mu=rep(0,N),Sigma=Sigma))
  
  ###randomly choose the index to generate categorical variable in each group
  ###in each data, the index of categorical variable can be different for different populations
  ###since we need to compare with pooling method, we set all the index of categorical variables are the same in different populations.
  
  ### the level of categorical variable
  index_b <-c()
  ###set seed to keep all the column 
  set.seed(seed)
  for (i in 1:p){
    index_b[i] <- sample(1:pi,size=(N*pb)/p,replace=F)+ (i-1)*pi
  }
  
  for (i in 1:p){
    X[,index_b[i]] = cut(X[,index_b[i]],breaks=quantile(X[,index_b[i]],probs=seq(0, 1, length.out = LL + 1)),include.lowest=T, labels=1:LL)
  }
  return(list(X=X,group=group))
  #which.factor <- as.numeric(which(sapply(X, is.factor)))
}

datagen<-function(seed,n1,n2,n3,n4,n5,N,pi,ps0,ps1,ps2,ps3,ps4,ps5,ps1234,ps1235,ps1245,ps1345,ps2345,ps123,ps124,ps125,ps134,ps135,ps145,ps234,ps235,ps245,ps345,rho1,rho2,rho3,rho4,rho5,gamma1,gamma2,gamma3,gamma4,gamma5,sigma1,sigma2,sigma3,sigma4,sigma5,samesig,scale,pb=0.25,LL=3){
 
   
  ###n1 sample size in population 1
  ###n2 sample size in population 2
  ###n3 sample size in population 3
  ###N number of predictors; 
  ###pi number of predictors in each group in each population: are the same due to pooling method
  ###ps0 proportion of true signals
  ###ps1 proportion of signals shown in population 1 only
  ###ps2 proportion of signals shown in population 2 only
  ###ps3 proportion of signals shown in population 3 only
  ###ps12 proportion of signals shown in population 1 and 2 only
  ###ps13 proportion of signals shown in population 1 and 3 only
  ###ps23 proportion of signals shown in population 2 and 3 only
  ###rho1 within-group correlation for design matrix in population 1
  ###rho2 within-group correlation for design matrix in population 2
  ###rho3 within-group correlation for design matrix in population 3
  ###gamma1 rho1*gamma1 is the between group correlation for design matrix in population 1
  ###gamma2 rho2*gamma2 is the between group correlation for design matrix in population 2
  ###sigma1 noise parameter for population 1
  ###sigma2 noise parameter for population 2
  ###samesig=1 true signals magnitude same for both data
  ###samesig=0 true signals magnitude different for both data
  ###effect scale multiplier
  ###pb the proportion of categorical variable
  ###LL the level of categorical variable
  p<-N/pi ###number of group
  
  X1 <- generate_X(n=n1,N=N,pi=pi,gamma=gamma1,rho=rho1,pb=pb,LL=LL,seed)$X
  X2 <- generate_X(n=n2,N=N,pi=pi,gamma=gamma2,rho=rho2,pb=pb,LL=LL,seed)$X
  X3 <- generate_X(n=n3,N=N,pi=pi,gamma=gamma3,rho=rho3,pb=pb,LL=LL,seed)$X
  X4 <- generate_X(n=n4,N=N,pi=pi,gamma=gamma4,rho=rho4,pb=pb,LL=LL,seed)$X
  X5 <- generate_X(n=n5,N=N,pi=pi,gamma=gamma5,rho=rho5,pb=pb,LL=LL,seed)$X
  
  group1 <- generate_X(n=n1,N=N,pi=pi,gamma=gamma1,rho=rho1,pb=pb,LL=LL,seed)$group
  group2 <- generate_X(n=n2,N=N,pi=pi,gamma=gamma2,rho=rho2,pb=pb,LL=LL,seed)$group
  group3 <- generate_X(n=n3,N=N,pi=pi,gamma=gamma3,rho=rho3,pb=pb,LL=LL,seed)$group
  group4 <- generate_X(n=n4,N=N,pi=pi,gamma=gamma4,rho=rho4,pb=pb,LL=LL,seed)$group
  group5 <- generate_X(n=n5,N=N,pi=pi,gamma=gamma5,rho=rho5,pb=pb,LL=LL,seed)$group
  
  RX1 <- model.matrix( ~ ., X1)[, -1] ### transfer design X into dummy matrix
  RX2 <- model.matrix( ~ ., X2)[, -1]
  RX3 <- model.matrix( ~ ., X3)[, -1]
  RX4 <- model.matrix( ~ ., X4)[, -1]
  RX5 <- model.matrix( ~ ., X5)[, -1]
  
  s0=round(p*ps0,0) 
  
  s1=round(p*ps1,0)
  s2=round(p*ps2,0)
  s3=round(p*ps3,0)
  s4=round(p*ps4,0)
  s5=round(p*ps5,0)
  
  s123=round(p*ps123,0)
  s124=round(p*ps124,0)
  s125=round(p*ps125,0)
  s134=round(p*ps134,0)
  s135=round(p*ps135,0)
  s145=round(p*ps145,0)
  s234=round(p*ps234,0)
  s235=round(p*ps235,0)
  s245=round(p*ps245,0)
  s345=round(p*ps345,0)
  
  s1234=round(p*ps1234,0)
  s1235=round(p*ps1235,0)
  s1245=round(p*ps1245,0)
  s1345=round(p*ps1345,0)
  s2345=round(p*ps2345,0)
  

  b0.1<-runif(s0)*scale
  b0.2<-runif(s0)*scale
  b0.3<-runif(s0)*scale
  b0.4<-runif(s0)*scale
  b0.5<-runif(s0)*scale
  
  if (samesig){b0.2<-b0.1;b0.3<-b0.1;b0.4<-b0.1;b0.5<-b0.1}
  
  b1<-runif(s1)
  b2<-runif(s2)
  b3<-runif(s3)
  b4<-runif(s4)
  b5<-runif(s5)
  
  b1234.1<-runif(s1234)
  b1234.2<-runif(s1234)
  b1234.3<-runif(s1234)
  b1234.4<-runif(s1234)
  
  b1235.1<-runif(s1235)
  b1235.2<-runif(s1235)
  b1235.3<-runif(s1235)
  b1235.5<-runif(s1235)
  
  b1245.1<-runif(s1245)
  b1245.2<-runif(s1245)
  b1245.4<-runif(s1245)
  b1245.5<-runif(s1245)
  
  b1345.1<-runif(s1345)
  b1345.3<-runif(s1345)
  b1345.4<-runif(s1345)
  b1345.5<-runif(s1345)
  
  b2345.2<-runif(s2345)
  b2345.3<-runif(s2345)
  b2345.4<-runif(s2345)
  b2345.5<-runif(s2345)
  
  if (samesig){b1234.1<-b1234.2<-b1234.3<-b1234.4;b1235.1<-b1235.2<-b1235.3<-b1235.5;b1245.1<-b1245.2<-b1245.4<-b1245.5; b1345.1<-b1345.3<-b1345.4<-b1345.5;
  b2345.2<-b2345.3<-b2345.4<-b2345.5}
  
  b123.1<-runif(s123)
  b123.2<-runif(s123)
  b123.3<-runif(s123)
  
  b124.1<-runif(s124)
  b124.2<-runif(s124)
  b124.4<-runif(s124)
  
  b125.1<-runif(s125)
  b125.2<-runif(s125)
  b125.5<-runif(s125)
  
  b134.1<-runif(s134)
  b134.3<-runif(s134)
  b134.4<-runif(s134)
  
  b135.1<-runif(s135)
  b135.3<-runif(s135)
  b135.5<-runif(s135)
  
  b145.1<-runif(s145)
  b145.4<-runif(s145)
  b145.5<-runif(s145)
  
  b234.2<-runif(s234)
  b234.3<-runif(s234)
  b234.4<-runif(s234)
  
  b235.2<-runif(s235)
  b235.3<-runif(s235)
  b235.5<-runif(s235)
  
  b245.2<-runif(s245)
  b245.4<-runif(s245)
  b245.5<-runif(s245)
  
  b345.3<-runif(s345)
  b345.4<-runif(s345)
  b345.5<-runif(s345)
  
  if (samesig){b123.1<-b123.2<-b123.3; b124.1<-b124.2<-b124.4;b125.1<-b125.2<-b125.5; b134.1<-b134.3<-b134.4; b135.1<-b135.3<-b135.5;
  b145.1<-b145.4<-b145.5; b234.2<-b234.3<-b234.4; b235.2<-b235.3<-b235.5; b245.2<-b245.4<-b245.5; b345.3<-b345.4<-b345.5}
  
  ss0<-sample(c(-1,1),s0,prob=c(0.5,0.5),replace=TRUE)
  ss1<-sample(c(-1,1),s1,prob=c(0.5,0.5),replace=TRUE)
  ss2<-sample(c(-1,1),s2,prob=c(0.5,0.5),replace=TRUE)
  ss3<-sample(c(-1,1),s3,prob=c(0.5,0.5),replace=TRUE)
  ss4<-sample(c(-1,1),s4,prob=c(0.5,0.5),replace=TRUE)
  ss5<-sample(c(-1,1),s5,prob=c(0.5,0.5),replace=TRUE)
  
  ss123<-sample(c(-1,1),s123,prob=c(0.5,0.5),replace=TRUE)
  ss124<-sample(c(-1,1),s124,prob=c(0.5,0.5),replace=TRUE)
  ss125<-sample(c(-1,1),s125,prob=c(0.5,0.5),replace=TRUE)
  ss134<-sample(c(-1,1),s134,prob=c(0.5,0.5),replace=TRUE)
  ss135<-sample(c(-1,1),s135,prob=c(0.5,0.5),replace=TRUE)
  ss145<-sample(c(-1,1),s145,prob=c(0.5,0.5),replace=TRUE)
  ss234<-sample(c(-1,1),s234,prob=c(0.5,0.5),replace=TRUE)
  ss235<-sample(c(-1,1),s235,prob=c(0.5,0.5),replace=TRUE)
  ss245<-sample(c(-1,1),s245,prob=c(0.5,0.5),replace=TRUE)
  ss345<-sample(c(-1,1),s345,prob=c(0.5,0.5),replace=TRUE)
  
  ss1234<-sample(c(-1,1),s1234,prob=c(0.5,0.5),replace=TRUE)
  ss1235<-sample(c(-1,1),s1235,prob=c(0.5,0.5),replace=TRUE)
  ss1245<-sample(c(-1,1),s1245,prob=c(0.5,0.5),replace=TRUE)
  ss1345<-sample(c(-1,1),s1345,prob=c(0.5,0.5),replace=TRUE)
  ss2345<-sample(c(-1,1),s2345,prob=c(0.5,0.5),replace=TRUE)
  
  ### new group after creating the dummy variable
  new_pi <- (pi*pb)*(LL-1) + pi*(1-pb)
  
  beta1<-c(rep(c(b0.1*ss0,b1*ss1,  0*ss2,  0*ss3, 0*ss4, 0*ss5,b123.1*ss123,b124.1*ss124,b125.1*ss125,b134.1*ss134,b135.1*ss135,b145.1*ss145,0*ss234,0*ss235,0*ss245,0*ss345,b1234.1*ss1234,b1235.1*ss1235,b1245.1*ss1245,b1345.1*ss1345,0*ss2345
),each=new_pi),rep(0,(p-s0-s1-s2-s3-s4-s5-s123-s124-s125-s134-s135-s145-s234-s235-s245-s345-s1234-s1235-s1245-s1345-s2345)*new_pi))
  beta2<-c(rep(c(b0.2*ss0, 0*ss1, b2*ss2,  0*ss3, 0*ss4, 0*ss5, b123.2*ss123,b124.2*ss124,b125.2*ss125,0*ss134,0*ss135,0*ss145,b234.2*ss234,b235.2*ss235,b245.2*ss245,0*ss345,b1234.2*ss1234,b1235.2*ss1235,b1245.2*ss1245,0*ss1345,b2345.2*ss2345
),each=new_pi),rep(0,(p-s0-s1-s2-s3-s4-s5-s123-s124-s125-s134-s135-s145-s234-s235-s245-s345-s1234-s1235-s1245-s1345-s2345)*new_pi))
  beta3<-c(rep(c(b0.3*ss0, 0*ss1,  0*ss2, b3*ss3, 0*ss4, 0*ss5, b123.3*ss123,0*ss124,0*ss125,b134.3*ss134,b135.3*ss135,0*ss145,b234.3*ss234,b235.3*ss235,0*ss245,b345.3*ss345,b1234.3*ss1234,b1235.3*ss1235,0*ss1245,b1345.3*ss1345,b2345.3*ss2345
),each=new_pi),rep(0,(p-s0-s1-s2-s3-s4-s5-s123-s124-s125-s134-s135-s145-s234-s235-s245-s345-s1234-s1235-s1245-s1345-s2345)*new_pi))
  beta4<-c(rep(c(b0.4*ss0, 0*ss1,  0*ss2,  0*ss3,b4*ss4, 0*ss5, 0*ss123,b124.4*ss124,0*ss125,b134.4*ss134,0*ss135,b145.4*ss145,b234.4*ss234,0*ss235,b245.4*ss245,b345.4*ss345,b1234.4*ss1234,0*ss1235,b1245.4*ss1245,b1345.4*ss1345,b2345.4*ss2345
),each=new_pi),rep(0,(p-s0-s1-s2-s3-s4-s5-s123-s124-s125-s134-s135-s145-s234-s235-s245-s345-s1234-s1235-s1245-s1345-s2345)*new_pi))
  beta5<-c(rep(c(b0.5*ss0, 0*ss1,  0*ss2,  0*ss3, 0*ss4,b5*ss5, 0*ss123,0*ss124,b125.5*ss125,0*ss134,b135.5*ss135,b145.5*ss145,0*ss234,b235.5*ss235,b245.5*ss245,b345.5*ss345,0*ss1234,b1235.5*ss1235,b1245.5*ss1245,b1345.5*ss1345,b2345.5*ss2345
),each=new_pi),rep(0,(p-s0-s1-s2-s3-s4-s5-s123-s124-s125-s134-s135-s145-s234-s235-s245-s345-s1234-s1235-s1245-s1345-s2345)*new_pi))
  
  
  y1<-RX1%*%beta1+rnorm(n1)*sigma1
  y2<-RX2%*%beta2+rnorm(n2)*sigma2
  y3<-RX3%*%beta3+rnorm(n3)*sigma3
  y4<-RX4%*%beta4+rnorm(n4)*sigma4
  y5<-RX5%*%beta5+rnorm(n5)*sigma5
  
  ####dichotomize y2,y4,y5 (same as probit model)
  y2<-as.numeric(y2>0)
  y4<-as.numeric(y4>0)
  y5<-as.numeric(y5>0)
  
  data1<-cbind(1,y1,X1)
  data2<-cbind(2,y2,X2)
  data3<-cbind(3,y3,X3)
  data4<-cbind(4,y4,X4)
  data5<-cbind(5,y5,X5)
  names(data1)=c("C","Y",paste("X",1:N,sep=""))
  names(data2)=c("C","Y",paste("X",1:N,sep=""))
  names(data3)=c("C","Y",paste("X",1:N,sep=""))
  names(data4)=c("C","Y",paste("X",1:N,sep=""))
  names(data5)=c("C","Y",paste("X",1:N,sep=""))
  
  data<-data.frame(rbind(data1,data2,data3,data4,data5))
  datal <- list(data=data,group1=group1,group2=group2,group3=group3,group4=group4,group5=group5)
}

#set.seed(1111)
#datal=datagen(seed=1111,n1=1000,n2=1000,n3=1000,n4=1000,n5=1000,N=80,pi=4,ps0=0.3,ps1=0,ps2=0,ps3=0,ps4=0,ps5=0,ps1234=0.1,ps1235=0.1,ps1245=0.1,ps1345=0.1,ps2345=0.1,ps123=0,ps124=0,ps125=0,ps134=0,ps135=0,ps145=0,ps234=0,ps235=0,ps245=0,ps345=0,rho1=0.5,rho2=0.5,rho3=0.5,rho4=0.5,rho5=0.5,gamma1=0.05,gamma2=0.05,gamma3=0.05,gamma4=0.05,gamma5=0.05,sigma1=1,sigma2=-1,sigma3=1,sigma4=-1,sigma5=1,samesig=0,scale=2,pb=0.25,LL=3)




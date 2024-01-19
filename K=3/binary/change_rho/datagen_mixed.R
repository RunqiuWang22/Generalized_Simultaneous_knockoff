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

datagen<-function(seed,n1,n2,n3,N,pi,ps0, ps1, ps2, ps3, ps12, ps13, ps23,rho1,rho2,rho3,gamma1,gamma2,gamma3,sigma1,sigma2,sigma3,samesig,scale,pb=0.25,LL=3){
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
  
  group1 <- generate_X(n=n1,N=N,pi=pi,gamma=gamma1,rho=rho1,pb=pb,LL=LL,seed)$group
  group2 <- generate_X(n=n2,N=N,pi=pi,gamma=gamma2,rho=rho2,pb=pb,LL=LL,seed)$group
  group3 <- generate_X(n=n3,N=N,pi=pi,gamma=gamma3,rho=rho3,pb=pb,LL=LL,seed)$group
	
	RX1 <- model.matrix( ~ ., X1)[, -1] ### transfer design X into dummy matrix
	RX2 <- model.matrix( ~ ., X2)[, -1]
	RX3 <- model.matrix( ~ ., X3)[, -1]
	
	s0=round(p*ps0,0) 
	s1=round(p*ps1,0)
	s2=round(p*ps2,0)
	s3=round(p*ps3,0)
	s12=round(p*ps12,0)
	s13=round(p*ps13,0)
	s23=round(p*ps23,0)
	
	b0.1<-runif(s0)*scale
	b0.2<-runif(s0)*scale
	b0.3<-runif(s0)*scale
	if (samesig){b0.2<-b0.1;b0.3<-b0.1}
	
	b1<-runif(s1)
	b2<-runif(s2)
	b3<-runif(s3)
	b12.1<-runif(s12)
	b12.2<-runif(s12)
	b13.1<-runif(s13)
	b13.3<-runif(s13)
	b23.2<-runif(s23)
	b23.3<-runif(s23)
	if (samesig){b12.2<-b12.1;b13.3<-b13.1;b23.3<-b23.2}
	
	ss0<-sample(c(-1,1),s0,prob=c(0.5,0.5),replace=TRUE)
	ss1<-sample(c(-1,1),s1,prob=c(0.5,0.5),replace=TRUE)
	ss2<-sample(c(-1,1),s2,prob=c(0.5,0.5),replace=TRUE)
	ss3<-sample(c(-1,1),s3,prob=c(0.5,0.5),replace=TRUE)
	ss12<-sample(c(-1,1),s12,prob=c(0.5,0.5),replace=TRUE)
	ss13<-sample(c(-1,1),s13,prob=c(0.5,0.5),replace=TRUE)
	ss23<-sample(c(-1,1),s23,prob=c(0.5,0.5),replace=TRUE)
	
	### new group after creating the dummy variable
	new_pi <- (pi*pb)*(LL-1) + pi*(1-pb)
	
	
	beta1<-c(rep(c(b0.1*ss0,b1*ss1,0*ss2,0*ss3,b12.1*ss12,b13.1*ss13,0*ss23),each=new_pi),rep(0,(p-s0-s1-s2-s3-s12-s13-s23)*new_pi))
	beta2<-c(rep(c(b0.2*ss0,0*ss1,b2*ss2,0*ss3,b12.2*ss12,0*ss13,b23.2*ss23),each=new_pi),rep(0,(p-s0-s1-s2-s3-s12-s13-s23)*new_pi))
	beta3<-c(rep(c(b0.3*ss0,0*ss1,0*ss2,b3*ss3,0*ss12,b13.3*ss13,b23.3*ss23),each=new_pi),rep(0,(p-s0-s1-s2-s3-s12-s13-s23)*new_pi))
	
	y1<-rbinom(n1,size=1,prob=exp(sigma1+RX1%*%beta1)/(1+exp(sigma1+RX1%*%beta1)))  
	y2<-rbinom(n2,size=1,prob=exp(sigma2+RX2%*%beta2)/(1+exp(sigma2+RX2%*%beta2)))
	y3<-rbinom(n3,size=1,prob=exp(sigma3+RX3%*%beta3)/(1+exp(sigma3+RX3%*%beta3)))
	
	data1<-cbind(1,y1,X1)
	data2<-cbind(2,y2,X2)
	data3<-cbind(3,y3,X3)
	names(data1)=c("C","Y",paste("X",1:N,sep=""))
	names(data2)=c("C","Y",paste("X",1:N,sep=""))
	names(data3)=c("C","Y",paste("X",1:N,sep=""))
	
	data<-data.frame(rbind(data1,data2,data3))
	datal <- list(data=data,group1=group1,group2=group2,group=group3)
}

#datal=datagen(seed=1112,n1=100,n2=100,n3=100,N=40,pi=4,ps0=0.3,ps1=0.1,ps2=0.1,ps3=0.1,ps12=0.1,ps13=0,ps23=0,rho1=0.5,rho2=0.5,rho3=0.5,gamma1=0.05,gamma2=0.05,gamma3=0.05,sigma1=1,sigma2=-1,sigma3=1,samesig=0,scale=1.2,pb=0.25,LL=3)


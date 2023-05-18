#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
  mm= N/pi #number of group
  s0=round(mm*ps0,0) 
  s1=round(mm*ps1,0)
  s2=round(mm*ps2,0)
  s3=round(mm*ps3,0)
  s4=round(mm*ps4,0)
  s5=round(mm*ps5,0)
  s12=round(mm*ps12,0)
  s13=round(mm*ps13,0)
  s14=round(mm*ps14,0)
  s15=round(mm*ps15,0)
  s23=round(mm*ps23,0)
  s24=round(mm*ps24,0)
  s25=round(mm*ps25,0)
  s34=round(mm*ps34,0)
  s35=round(mm*ps35,0)
  s45=round(mm*ps45,0)
  
result=foreach (m=1:M, .combine=cbind)%dopar%{
	source("datagen_mixed.R")
  source("group_sequential.R")
	source("compKnock_mixed.R")
	source("compKnockStack_mixed.R")
  seed=iniseed+m
	set.seed(seed)
	datal=datagen(seed,n1,n2,n3,n4,n5,N,pi,ps0, ps1, ps2, ps3, ps4,ps5,ps12,ps13,ps14,ps15,ps23,ps24,ps25,ps34,ps35,ps45,rho1,rho2,rho3,rho4,rho5,gamma1,gamma2,gamma3,gamma4,gamma5,sigma1,sigma2,sigma3,sigma4,sigma5,samesig,scale,pb,LL)
	  
	### generate Z and Ztilde for the simultaneous and intersection
	coeff=myest(datal)
	myselect=myresult(coeff=coeff,q=q,choose="simultaneous",method="Diff")
	myselect_int=myresult(coeff=coeff,q=q,choose="intersection",method="Diff")
	stack=myest_stack(datal,q,method="Diff")
	
	myselect_stack=stack$myselect
	Z_Ztilde=cbind(coeff,stack$ZZ)
	colnames(Z_Ztilde)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde","Z5","Z5tilde","Z","Ztilde")
	write.csv(Z_Ztilde,paste("Z","s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s4=",as.character(s4),"s5=",as.character(s5),"s12=",as.character(s12),"s13=",as.character(s13),"s14=",as.character(s14),"s15=",as.character(s15),"s23=",as.character(s23),"s24=",as.character(s24),"s25=",as.character(s25),"s34=",as.character(s34),"s35=",as.character(s35),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"rho4=",as.character(rho4),"rho5=",as.character(rho5),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"gamma4=",as.character(gamma4),"gamma5=",as.character(gamma5),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"sigma4=",as.character(sigma4),"sigma5=",as.character(sigma5),"samesig=",as.character(samesig),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
	
  TP=FDP=TP_stack=FDP_stack=TP_int=FDP_int=NA
  
  TP=length(which(myselect%in%c(1:s0)))
  FDP=(length(myselect)-TP)/max(1,length(myselect))
  TP_stack=length(which(myselect_stack%in%c(1:s0)))
  FDP_stack=(length(myselect_stack)-TP_stack)/max(1,length(myselect_stack))
  TP_int=length(which(myselect_int%in%c(1:s0)))
  FDP_int=(length(myselect_int)-TP_int)/max(1,length(myselect_int))
  
  c(FDP,TP,FDP_stack,TP_stack,FDP_int,TP_int)
}

FDP=result[1,]
TP=result[2,]
FDP_stack=result[3,]
TP_stack=result[4,]
FDP_int=result[5,]
TP_int=result[6,]

FDR=mean(FDP,na.rm=TRUE)
power=mean(TP,na.rm=TRUE)/s0
FDR_stack=mean(FDP_stack,na.rm=TRUE)
power_stack=mean(TP_stack,na.rm=TRUE)/s0
FDR_int=mean(FDP_int,na.rm=TRUE)
power_int=mean(TP_int,na.rm=TRUE)/s0

result=c(FDR,power,FDR_stack,power_stack,FDR_int,power_int)
write.csv(result,paste("result=","s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s4=",as.character(s4),"s5=",as.character(s5),"s12=",as.character(s12),"s13=",as.character(s13),"s14=",as.character(s14),"s15=",as.character(s15),"s23=",as.character(s23),"s24=",as.character(s24),"s25=",as.character(s25),"s34=",as.character(s34),"s35=",as.character(s35),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"rho4=",as.character(rho4),"rho5=",as.character(rho5),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"gamma4=",as.character(gamma4),"gamma5=",as.character(gamma5),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"sigma4=",as.character(sigma4),"sigma5=",as.character(sigma5),"samesig=",as.character(samesig),"scale=",as.character(scale),".csv",sep=""))







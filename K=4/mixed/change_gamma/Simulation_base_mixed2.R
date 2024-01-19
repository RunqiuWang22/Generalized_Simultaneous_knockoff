#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0.15
p= N/pi #number of group
s0=round(p*ps0,0) 

result=foreach (m=1:M, .combine=cbind)%dopar%{
  source("datagen_mixed.R")
  source("group_sequential.R")
  source("compKnock_mixed.R")
  source("compKnockStack_mixed.R")
  seed=iniseed+m
  set.seed(seed)
  datal=datagen(seed,n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb,LL)
  
  ### generate Z and Ztilde for the simultaneous and intersection
  coeff=myest(datal)
  myselect=myresult(coeff=coeff,q=q,choose="simultaneous",method="Diff")
  myselect_int=myresult(coeff=coeff,q=q,choose="intersection",method="Diff")
  stack=myest_stack(datal,q,method="Diff")
  
  myselect_stack=stack$myselect
  Z_Ztilde=cbind(coeff,stack$ZZ)
  colnames(Z_Ztilde)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde","Z","Ztilde")
  File01=sprintf("Z_Ztilde_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  write.csv(Z_Ztilde, File01)
  
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

File00=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
               n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
write.csv(result, File00)






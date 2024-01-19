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
  source("compKnock_mixed_ind.R")
  seed=iniseed+m
  set.seed(seed)
  datal=datagen(seed,n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb,LL)
  
  ## generate Z and Ztilde for the simultaneous and intersection
  newdatal = myknockoff(datal)
  coeff_grlasso = ffit(newdatal,choose="grlasso")
  myselect_grlasso=myresult(coeff_grlasso,q=q,choose="grlasso",method="Diff",p=p)
  
  coeff_lasso = ffit(newdatal,choose="lasso")
  myselect_lasso=myresult(coeff_lasso,q=q,choose="lasso",method="Diff",p=p)
  
  colnames(coeff_grlasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde")
  colnames(coeff_lasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde")
  File01=sprintf("Z_Ztilde_grlasso_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  write.csv(coeff_grlasso, File01)
  
  File02=sprintf("Z_Ztilde_lasso_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  write.csv(coeff_lasso, File02)
  TP_grlasso=FDP_grlasso=TP_lasso=FDP_lasso=NA
  
  TP_grlasso=length(which(myselect_grlasso%in%c(1:s0)))
  FDP_grlasso=(length(myselect_grlasso)-TP_grlasso)/max(1,length(myselect_grlasso))
  
  TP_lasso=length(which(myselect_lasso%in%c(1:s0)))
  FDP_lasso=(length(myselect_lasso)-TP_lasso)/max(1,length(myselect_lasso))
  
  c(TP_grlasso,FDP_grlasso,TP_lasso,FDP_lasso)
}


TP_grlasso=result[1,]
FDP_grlasso=result[2,]
TP_lasso=result[3,]
FDP_lasso=result[4,]

FDR_grlasso=mean(FDP_grlasso,na.rm=TRUE)
power_grlasso=mean(TP_grlasso,na.rm=TRUE)/s0
FDR_lasso=mean(FDP_lasso,na.rm=TRUE)
power_lasso=mean(TP_lasso,na.rm=TRUE)/s0


result=c(FDR_grlasso,power_grlasso,FDR_lasso,power_lasso)
File00=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
               n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
write.csv(result, File00)









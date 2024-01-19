#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0.125
s0=round(p*ps0,0) 


result=foreach (m=1:M, .combine=cbind)%dopar%{
  source("datagen_mixed.R")
  source("compKnock_mixed_ind.R")
  seed=iniseed+m
  set.seed(seed)
  datal=datagen(p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4)
  
  ## generate Z and Ztilde for the simultaneous and intersection
  newdatal = myknockoff(datal)
  coeff_grlasso = ffit(newdatal,choose="grlasso")
  myselect_grlasso=myresult(coeff_grlasso,q=q,choose="grlasso",method="Diff",p=p)
  
  #colnames(coeff_grlasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde")
  #colnames(coeff_lasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde")
  #write.csv(coeff_grlasso,paste("Z_Ztilde_grlasso_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"n4=",as.character(n4),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s4=",as.character(s4),"s12=",as.character(s12),"s13=",as.character(s13),"s14=",as.character(s14),"s23=",as.character(s23),"s24=",as.character(s24),"s34=",as.character(s34),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"rho4=",as.character(rho4),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"gamma4=",as.character(gamma4),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"sigma4=",as.character(sigma4),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
  #write.csv(coeff_lasso,paste("Z_Ztilde_lasso_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"n4=",as.character(n4),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s4=",as.character(s4),"s12=",as.character(s12),"s13=",as.character(s13),"s14=",as.character(s14),"s23=",as.character(s23),"s24=",as.character(s24),"s34=",as.character(s34),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"rho4=",as.character(rho4),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"gamma4=",as.character(gamma4),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"sigma4=",as.character(sigma4),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
  
  TP_grlasso=FDP_grlasso=NA
  
  TP_grlasso=length(which(myselect_grlasso%in%c(1:s0)))
  FDP_grlasso=(length(myselect_grlasso)-TP_grlasso)/max(1,length(myselect_grlasso))
  
  
  
  c(TP_grlasso,FDP_grlasso)
}


TP_grlasso=result[1,]
FDP_grlasso=result[2,]

FDR_grlasso=mean(FDP_grlasso,na.rm=TRUE)
power_grlasso=mean(TP_grlasso,na.rm=TRUE)/s0



result=c(FDR_grlasso,power_grlasso)
File0=sprintf("Mixed_resultin=%d_%d_%d_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d.csv",
              p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4)

write.csv(result, File0)









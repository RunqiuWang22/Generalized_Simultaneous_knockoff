#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0.15
mm= N/pi #number of group
s0=round(mm*ps0,0)
s1=round(mm*ps1,0)
s2=round(mm*ps2,0)
s3=round(mm*ps3,0)
s12=round(mm*ps12,0)
s13=round(mm*ps13,0)
s23=round(mm*ps23,0)

result=foreach (m=1:M, .combine=cbind)%dopar%{
  source("datagen_mixed.R")
  source("compKnock_mixed_ind.R")
  seed=iniseed+m
  set.seed(seed)
  datal=datagen(seed,n1,n2,n3,N,pi,ps0,ps1,ps2,ps3,ps12,ps13,ps23,rho1,rho2,rho3,gamma1,gamma2,gamma3,sigma1,sigma2,sigma3,samesig,scale,pb,LL)
  ### generate Z and Ztilde for the simultaneous and intersection
  newdatal = myknockoff(datal)
  coeff_grlasso = ffit(newdatal,choose="grlasso")
  myselect_grlasso=myresult(coeff_grlasso,q=q,choose="grlasso",method="Diff",p=mm)
  
  coeff_lasso = ffit(newdatal,choose="lasso")
  myselect_lasso=myresult(coeff_lasso,q=q,choose="lasso",method="Diff",p=mm)
  
  colnames(coeff_grlasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde")
  colnames(coeff_lasso)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde")
  write.csv(coeff_grlasso,paste("Z_Ztilde_grlasso_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
  write.csv(coeff_lasso,paste("Z_Ztilde_lasso_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
  
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
write.csv(result,paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep=""))










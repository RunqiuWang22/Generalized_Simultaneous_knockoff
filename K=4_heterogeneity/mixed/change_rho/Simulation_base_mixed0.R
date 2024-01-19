#####do parallel setting on s0 with 6 core
library(doParallel)
registerDoParallel(cores=10)
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0
s0=round(p*ps0,0) 
  
  
  result=foreach (m=1:M, .combine=cbind)%dopar%{
    source("datagen_mixed.R")
    source("group_sequential.R")
    source("compKnock_mixed.R")
    seed=iniseed+m
    set.seed(seed)
    datal=datagen(p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4)
    
    ### generate Z and Ztilde for the simultaneous and intersection
    coeff=myest(datal)
    myselect=myresult(coeff=coeff,q=q,choose="simultaneous",method="Diff")
    myselect_int=myresult(coeff=coeff,q=q,choose="intersection",method="Diff")
    
    Z_Ztilde=coeff
    colnames(Z_Ztilde)=c("Z1","Z1tilde","Z2","Z2tilde","Z3","Z3tilde","Z4","Z4tilde")
    #write.csv(Z_Ztilde,paste("Z_Ztilde_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"n4=",as.character(n4),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s4=",as.character(s4),"s12=",as.character(s12),"s13=",as.character(s13),"s14=",as.character(s14),"s23=",as.character(s23),"s24=",as.character(s24),"s34=",as.character(s34),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"rho4=",as.character(rho4),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"gamma4=",as.character(gamma4),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"sigma4=",as.character(sigma4),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep=""))
    File00=sprintf("Z_Ztilde=%d_%d_%d_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d.csv",
                   p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4,m)
    
    write.csv(Z_Ztilde, File00)
    
    TP=FDP=TP_int=FDP_int=NA
    
    TP=length(which(myselect%in%c(1:s0)))
    FDP=(length(myselect)-TP)/max(1,length(myselect))
    TP_int=length(which(myselect_int%in%c(1:s0)))
    FDP_int=(length(myselect_int)-TP_int)/max(1,length(myselect_int))
    
    c(FDP,TP,FDP_int,TP_int)
  }
  
  FDP=result[1,]
  TP=result[2,]
  FDP_int=result[3,]
  TP_int=result[4,]
  
  FDR=mean(FDP,na.rm=TRUE)
  power=mean(TP,na.rm=TRUE)/s0
  FDR_int=mean(FDP_int,na.rm=TRUE)
  power_int=mean(TP_int,na.rm=TRUE)/s0
  
  result=c(FDR,power,FDR_int,power_int)
  
  File0=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d.csv",
                p,n1,n2,n3,n4,pi1,pi2,pi3,pi4,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps134,ps234,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,pb1,pb2,pb3,pb4,LL1,LL2,LL3,LL4)
  
  write.csv(result, File0)
  
  
  
  
  
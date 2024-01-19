setwd("~/Library/CloudStorage/OneDrive-UniversityofNebraskaMedicalCenter/OneDrive - University of Nebraska Medical Center 2/group R_new_20231003/mixed_new_group_K=3_V2/K=3/binary/result/")
source("FDR_power_plot.R")
tiff("FigureS2.tif",width = 15, height = 15,units = 'in',res = 600)
par(mfrow = c(3, 3)) 
####figure 1 change s0
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0
q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.15,0.5,by=0.05)*p
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="s0 "
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  s0=vary_list[i]
  s1=round(p*ps1,0)
  s2=round(p*ps2,0)
  s3=round(p*ps3,0)
  s12=round(p*ps12,0)
  s13=round(p*ps13,0)
  s23=round(p*ps23,0)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=T,labelsize=2,legsize=2,cexsize=2.3)


####figure 2 change s1=s2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps23=0.05
ps13=0
q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.05,0.2,by=0.025)*p
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="s1=s2=s3"
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  s0=round(p*ps0,0)
  s1=s2=s3=round(vary_list[i],0)
  s12=round(p*ps12,0)
  s13=round(p*ps13,0)
  s23=round(p*ps23,0)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


###change S2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.05,0.2,by=0.025)*p
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="s12=s13=s23"
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  s0=round(p*ps0,0)
  s1=round(p*ps1,0)
  s2=round(p*ps2,0)
  s3=round(p*ps3,0)
  s12=s13=s23=round(vary_list[i],0)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)

### change rho1: ps0=0.3 ps1=0 ps2=0
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.05,1,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho3=rho2=round(rho1,2)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


### change rho2: ps0=0.3 ps1=0.2 ps2=0.2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0.15
ps12=ps23=0.05
ps13=0
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.05,1,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho3=rho2=round(rho1,2)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


### change rho3: ps0=0.3 ps1=0 ps2=0.2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0.15
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0.05,1,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho3=rho2=round(rho1,2)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


### change gamma1: ps0=0.3 ps1=0 ps2=0
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0,0.5,by=0.05)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma3=gamma2=gamma1
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


### change gamma2: ps0=0.3 ps1=0.2 ps2=0.2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0.15
ps12=ps23=0.05
ps13=0
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0,0.5,by=0.05)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma3=gamma2=gamma1
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)


### change gamma3: ps0=0.3 ps1=0 ps2=0.2
n1=1000
n2=1000
n3=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0.15
s0=round(p*ps0,0)
s1=round(p*ps1,0)
s2=round(p*ps2,0)
s3=round(p*ps3,0)
s12=round(p*ps12,0)
s13=round(p*ps13,0)
s23=round(p*ps23,0)

q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
samesig=1
iniseed=1111

scale=2
vary_list=seq(0,0.5,by=0.05)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma3=gamma2=round(gamma1,2)
  filename1=paste("Mixed_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  filename2=paste("Mixed_ind_result=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),".csv",sep="")
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}
FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.5),show_legend=F,labelsize=2,legsize=1.5,cexsize=2.3)



dev.off()




setwd("~/Library/CloudStorage/OneDrive-UniversityofNebraskaMedicalCenter/OneDrive - University of Nebraska Medical Center 2/group R_new_20231003/mixed_new_group_K=4/K=4_new_M=40/result/")
source("FDR_power_plot.R")
tiff("FigureS4.tif",width = 15, height = 15,units = 'in',res = 600)
par(mfrow = c(3, 3)) 
####figure 1 change s0
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps234=ps134=0

q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0.15,0.5,by=0.05)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="s0"
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  ps0=vary_list[i]
  
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list*p,xlab=xlab, title='',leg_coords=c(min(vary_list*p),0.7),show_legend=T,labelsize=2,legsize=2,cexsize=2.3)

###figure 4 change s1
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps12=ps14=ps34=ps13=ps23=ps24=0
ps123=ps124=ps134=ps234=0
q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=round(seq(0,0.15,by=0.05),2)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="S1=S2=S3=S4"
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  ps4=ps3=ps2=ps1=vary_list[i]
  
  
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list*p,xlab=xlab, title='',leg_coords=c(min(vary_list*p),0.5),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)

####figure 2 change s3
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0

q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0,0.15,by=0.025)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab="s123=s124=s234=s134"
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  ps123=ps124=ps234=ps134=vary_list[i]
  
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list*p,xlab=xlab, title='',leg_coords=c(min(vary_list*p),0.5),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)


### change rho0: ps0
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0
q=0.2
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0.05,0.95,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho4=rho3=rho2=round(rho1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)


### change rho1: ps0=0.3,ps1=ps2=ps3=ps4
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0.1
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0
q=0.2
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0.05,0.95,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho4=rho3=rho2=round(rho1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)


### change rho3: ps0=0.3 ,ps123=ps124=ps134=ps234=0.1
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0.15
q=0.2
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0.05,0.95,by=0.1)
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Within group correlation (',rho,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  rho1=vary_list[i]
  rho4=rho3=rho2=round(rho1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)



### change gamma0: ps0
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0
q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0,0.5,by=0.05) 
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma4=gamma3=gamma2=round(gamma1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)


### change gamma1: ps0=0.3,ps1=ps2=ps3=ps4
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0.1
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0
q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0,0.5,by=0.05) 
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma4=gamma3=gamma2=round(gamma1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)


### change gamma3: ps0=0.3 ,ps123=ps124=ps134=ps234=0.1
n1=1000
n2=1000
n3=1000
n4=1000
N=160
pi=4
p=N/pi
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps13=ps14=ps23=ps24=ps34=0
ps123=ps124=ps134=ps234=0.15
q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
sigma1=1
sigma2=2
sigma3=1
sigma4=1
samesig=1
iniseed=1111

scale=4
vary_list=seq(0,0.5,by=0.05) 
cols=c("black","red","blue","orange","green")
pchs=c(15,16,17,18,19)
xlab=expression(paste('Correlation ratio (',gamma,')',sep=''))
names=c("GS Knockoff","Pooling","Intersection","Individual (Group_Lasso)","Individual (Lasso)")
FDRmat=powermat=matrix(data=NA,nrow=length(vary_list),ncol=length(names))
for (i in 1:length(vary_list)){
  gamma1=vary_list[i]
  gamma4=gamma3=gamma2=round(gamma1,2)
  filename1=sprintf("Mixed_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  
  filename2=sprintf("Mixed_ind_result=%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f.csv",
                    n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps12,ps13,ps14,ps23,ps24,ps34,ps123,ps124,ps234,ps134,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale)
  tmp1=c(read.csv(filename1)[,2])
  tmp2=c(read.csv(filename2)[,2])
  tmp=c(tmp1,tmp2)
  FDRmat[i,]=tmp[c(1,3,5,7,9)]
  powermat[i,]=tmp[c(2,4,6,8,10)]
}

FDR_and_power_plot(FDRmat=FDRmat, powermat=powermat, names=names, cols=cols, pchs=pchs, alpha=q, vary_list,xlab=xlab, title='',leg_coords=c(min(vary_list),0.65),show_legend=F,labelsize=2,legsize=2,cexsize=2.3)



dev.off()

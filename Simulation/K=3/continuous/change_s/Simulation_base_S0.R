
args=commandArgs(trailingOnly=TRUE)
iii=as.numeric(args[1])

library("magic")
library("mvtnorm")
library("lars")
library("knockoff")
library("grpreg")
library("far")
library("methods")
library("doParallel")
library("glmnet")
library(seqknockoff)

####two scenario of signal strengths: 
samesig_list=c(0,1)
samesig=samesig_list[iii%%length(samesig_list)+1]
iii=iii%/%length(samesig_list)

#### different scale:previous one is 1.2
scale_list=2##length is 5
scale=scale_list[iii%%length(scale_list)+1]
iii=iii%/%length(scale_list)

###different dimension N
N_list=c(80) ##length is 2
N=N_list[iii%%length(N_list)+1]
iii=iii%/%length(N_list)


ps0_list=seq(0.15,0.5,by=0.05) ##length is 8
ps0=ps0_list[iii%%length(ps0_list)+1]
iii=iii%/%length(ps0_list)

#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
pi=4
ps1=ps2=ps3=0
ps12=ps13=ps23=0
q=0.2
rho1=rho2=rho3=0.5
gamma1=gamma2=gamma3=0.1
sigma1=1
sigma2=2
sigma3=1
iniseed=1111
M=100
setwd("./")
filename=paste("Simulation_base_continuous",as.character(iii),".R",sep="")
source(filename)
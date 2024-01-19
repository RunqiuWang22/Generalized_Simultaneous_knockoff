
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

####two scenario of signal strengths: 
samesig_list=c(0,1)
samesig=samesig_list[iii%%length(samesig_list)+1]
iii=iii%/%length(samesig_list)

#### different scale:previous one is 1.2
scale_list=c(4)##length is 5
scale=scale_list[iii%%length(scale_list)+1]
iii=iii%/%length(scale_list)

###different dimension N
N_list=c(160) ##length is 2
N=N_list[iii%%length(N_list)+1]
iii=iii%/%length(N_list)

#ps0=0.2
ps2345_list=seq(0,0.125,by=0.025) ##length is 4
ps2345=ps2345_list[iii%%length(ps2345_list)+1]
iii=iii%/%length(ps2345_list)

ps1234=ps1235=ps1245=ps1345=ps2345

#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
n4=1000
n5=1000
pi=4
ps0=0.3
ps5=ps4=ps3=ps2=ps1=0
ps123=ps124=ps125=ps134=ps135=ps145=ps234=ps235=ps245=ps345=0
q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
rho5=0.5
gamma1=0.1
gamma2=0.15
gamma3=0.1
gamma4=0.05
gamma5=0.1
sigma1=1
sigma2=2
sigma3=1
sigma4=1
sigma5=1
iniseed=1111
M=100
setwd("./")
filename=paste("Simulation_base_mixed",as.character(iii),"_ind.R",sep="")
source(filename)
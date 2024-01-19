
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

###different sample size
n1_list=c(1000) ##length is 3
n1=n1_list[iii%%length(n1_list)+1]
iii=iii%/%length(n1_list)
n4=n3=n2=n1

###different dimension N
N_list=c(160) ##length is 2
N=N_list[iii%%length(N_list)+1]
iii=iii%/%length(N_list)

#ps0=0.2
ps134_list=seq(0,0.15,by=0.025) ##length is 7
ps134=ps134_list[iii%%length(ps134_list)+1]
iii=iii%/%length(ps134_list)

ps123=ps124=ps234=ps134

#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

pi=4
ps0=0.3
ps1=ps2=ps3=ps4=0
ps12=ps14=ps34=ps13=ps23=ps24=0

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
iniseed=1111
M=100
setwd("./")
filename=paste("Simulation_base_mixed",as.character(iii),"_ind.R",sep="")
source(filename)
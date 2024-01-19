
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
scale_list=4##length is 5
scale=scale_list[iii%%length(scale_list)+1]
iii=iii%/%length(scale_list)

###different dimension N
N_list=c(160) ##length is 2
N=N_list[iii%%length(N_list)+1]
iii=iii%/%length(N_list)

### different rho1
rho1_list=seq(0.05,0.95,by=0.1) #10
rho1=rho1_list[iii%%length(rho1_list)+1]
iii=iii%/%length(rho1_list)

rho4=rho3=rho2=rho1

#####baseline setting
n1=1000
n2=1000
n3=1000
n4=1000
pb=0.25 #proportion of categorical variables
LL = 3
pi=4
q=0.2
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
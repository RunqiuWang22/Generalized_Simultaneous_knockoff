
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
p_list=c(40) ##length is 2
p=p_list[iii%%length(p_list)+1]
iii=iii%/%length(p_list)

### different gamma1
gamma1_list=seq(0,0.5,by=0.05) #11
gamma1=gamma1_list[iii%%length(gamma1_list)+1]
iii=iii%/%length(gamma1_list)

gamma4=gamma3=gamma2=gamma1

#####baseline setting
pb1=0 #proportion of categorical variables
LL1 =0

pb2=0.25 #proportion of categorical variables
LL2 = 4

pb3=1 #proportion of categorical variables
LL3 = 3

pb4=0.5 #proportion of categorical variables
LL4 = 2

n1=2000
n2=1200
n3=700
n4=600

pi1=4
pi2=4
pi3=2
pi4=4


q=0.2
rho1=0.5
rho2=0.4
rho3=0.5
rho4=0.6
sigma1=1
sigma2=2
sigma3=1
sigma4=1
iniseed=1111
M=100
setwd("./")
filename=paste("Simulation_base_mixed",as.character(iii),".R",sep="")
source(filename)
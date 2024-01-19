setwd("./")
samesig=1
scale=4
N=160
#####baseline setting
pb=0.3 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
n4=1000
n5=1000
pi=4
ps0=0.3
ps1=ps2=ps3=ps4=ps5=0
ps1234=ps1235=ps1245=ps1345=ps2345=0
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

W = matrix(NA,N/pi,M)
for (m in 1:M){
  File01=sprintf("Z_Ztilde_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps5,ps1234,ps1235,ps1245,ps1345,ps2345,ps123,ps124,ps125,ps134,ps135,ps145,ps234,ps235,ps245,ps345,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  
  if (file.exists(File01)==T){
    Z_Ztilde = read.csv(File01,header=T)
    Z1 = Z_Ztilde$Z1
    Z1tilde = Z_Ztilde$Z1tilde
    Z2 = Z_Ztilde$Z2
    Z2tilde = Z_Ztilde$Z2tilde
    Z3 = Z_Ztilde$Z3
    Z3tilde = Z_Ztilde$Z3tilde
    Z4 = Z_Ztilde$Z4
    Z4tilde = Z_Ztilde$Z4tilde
    Z5 = Z_Ztilde$Z5
    Z5tilde = Z_Ztilde$Z5tilde
    W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)*(Z4-Z4tilde)*(Z5-Z5tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  W1 = W1[!is.na(W1)]
  WW = na.omit(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[4]
  
  ###observed value
  n1 = sum(WW<= -Q4)
  n2 = sum(WW> -Q4 & WW<= -Q3)
  n3 = sum(WW> -Q3 &WW<= -Q2)
  n4 = sum(WW> -Q2 & WW<= -Q1)
  n5 = sum(WW> -Q1 & WW<= 0)
  n6 = sum(WW> 0 & WW<= Q1)
  n7 = sum(WW> Q1 & WW<= Q2)
  n8 = sum(WW> Q2 & WW<= Q3)
  n9 = sum(WW> Q3 & WW<= Q4)
  n10 = sum(WW> Q4)
  O = c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
  ###expected value
  E5 = (n1+n10)/2
  E4 = (n2+n9)/2
  E3 = (n3+n8)/2
  E2 = (n4+n7)/2
  E1 = (n5+n6)/2
  
  E = c(E5,E4,E3,E2,E1,E1,E2,E3,E4,E5)
  
  chi_sq[j] = sum((O-E)^2/E)
  
  # RR[j] = chi_sq[j]>qchisq(0.95,df=5) ###Ture means reject
  
  RR1[j] = 1-pchisq(chi_sq[j], df=5) <= 0.05/40 ###using bonferroni
}

sum(which(RR1==TRUE) %in% 1:12) ###true signals not symmetric
sum(which(RR1==FALSE) %in% 1:12) ###true signals symmetric
sum(which(RR1==TRUE) %in% setdiff(1:40,1:12)) ###nulls not symmetric
sum(which(RR1==FALSE) %in% setdiff(1:40,1:12))###nulls symmetric


#####################
samesig=1
scale=4
N=160
#####baseline setting
pb=0.3 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
n4=1000
n5=1000
pi=4
ps0=0.3
ps1=ps2=ps3=ps4=ps5=0
ps1234=ps1235=ps1245=ps1345=ps2345=0
ps123=ps124=ps125=ps134=ps135=ps145=0
ps234=ps235=ps245=ps345=0.1
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

W = matrix(NA,N/pi,M)
for (m in 1:M){
  File01=sprintf("Z_Ztilde_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps5,ps1234,ps1235,ps1245,ps1345,ps2345,ps123,ps124,ps125,ps134,ps135,ps145,ps234,ps235,ps245,ps345,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  
  if (file.exists(File01)==T){
    Z_Ztilde = read.csv(File01,header=T)
    Z1 = Z_Ztilde$Z1
    Z1tilde = Z_Ztilde$Z1tilde
    Z2 = Z_Ztilde$Z2
    Z2tilde = Z_Ztilde$Z2tilde
    Z3 = Z_Ztilde$Z3
    Z3tilde = Z_Ztilde$Z3tilde
    Z4 = Z_Ztilde$Z4
    Z4tilde = Z_Ztilde$Z4tilde
    Z5 = Z_Ztilde$Z5
    Z5tilde = Z_Ztilde$Z5tilde
    W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)*(Z4-Z4tilde)*(Z5-Z5tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  W1 = W1[!is.na(W1)]
  WW = na.omit(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[4]
  
  ###observed value
  n1 = sum(WW<= -Q4)
  n2 = sum(WW> -Q4 & WW<= -Q3)
  n3 = sum(WW> -Q3 &WW<= -Q2)
  n4 = sum(WW> -Q2 & WW<= -Q1)
  n5 = sum(WW> -Q1 & WW<= 0)
  n6 = sum(WW> 0 & WW<= Q1)
  n7 = sum(WW> Q1 & WW<= Q2)
  n8 = sum(WW> Q2 & WW<= Q3)
  n9 = sum(WW> Q3 & WW<= Q4)
  n10 = sum(WW> Q4)
  O = c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
  ###expected value
  E5 = (n1+n10)/2
  E4 = (n2+n9)/2
  E3 = (n3+n8)/2
  E2 = (n4+n7)/2
  E1 = (n5+n6)/2
  
  E = c(E5,E4,E3,E2,E1,E1,E2,E3,E4,E5)
  
  chi_sq[j] = sum((O-E)^2/E)
  
  # RR[j] = chi_sq[j]>qchisq(0.95,df=5) ###Ture means reject
  
  RR1[j] = 1-pchisq(chi_sq[j], df=5) <= 0.05/40 ###using bonferroni
}

sum(which(RR1==TRUE) %in% 1:12) ###true signals not symmetric
sum(which(RR1==FALSE) %in% 1:12) ###true signals symmetric
sum(which(RR1==TRUE) %in% setdiff(1:40,1:12)) ###nulls not symmetric
sum(which(RR1==FALSE) %in% setdiff(1:40,1:12))###nulls symmetric



#######
samesig=1
scale=4
N=160
#####baseline setting
pb=0.3 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
n4=1000
n5=1000
pi=4
ps0=0.3
ps1=ps2=ps3=ps4=ps5=0
ps1234=ps1235=ps1245=ps1345=ps2345=0.1
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

W = matrix(NA,N/pi,M)
for (m in 1:M){
  File01=sprintf("Z_Ztilde_%d_%d_%d_%d_%d_%d_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%.3f_%d_%d_%d_%d_%d_%.3f_%d.csv",
                 n1,n2,n3,n4,N,pi,ps0,ps1,ps2,ps3,ps4,ps5,ps1234,ps1235,ps1245,ps1345,ps2345,ps123,ps124,ps125,ps134,ps135,ps145,ps234,ps235,ps245,ps345,rho1,rho2,rho3,rho4,gamma1,gamma2,gamma3,gamma4,sigma1,sigma2,sigma3,sigma4,samesig,scale,m)
  
  if (file.exists(File01)==T){
    Z_Ztilde = read.csv(File01,header=T)
    Z1 = Z_Ztilde$Z1
    Z1tilde = Z_Ztilde$Z1tilde
    Z2 = Z_Ztilde$Z2
    Z2tilde = Z_Ztilde$Z2tilde
    Z3 = Z_Ztilde$Z3
    Z3tilde = Z_Ztilde$Z3tilde
    Z4 = Z_Ztilde$Z4
    Z4tilde = Z_Ztilde$Z4tilde
    Z5 = Z_Ztilde$Z5
    Z5tilde = Z_Ztilde$Z5tilde
    W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)*(Z4-Z4tilde)*(Z5-Z5tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  W1 = W1[!is.na(W1)]
  WW = na.omit(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8))[4]
  
  ###observed value
  n1 = sum(WW<= -Q4)
  n2 = sum(WW> -Q4 & WW<= -Q3)
  n3 = sum(WW> -Q3 &WW<= -Q2)
  n4 = sum(WW> -Q2 & WW<= -Q1)
  n5 = sum(WW> -Q1 & WW<= 0)
  n6 = sum(WW> 0 & WW<= Q1)
  n7 = sum(WW> Q1 & WW<= Q2)
  n8 = sum(WW> Q2 & WW<= Q3)
  n9 = sum(WW> Q3 & WW<= Q4)
  n10 = sum(WW> Q4)
  O = c(n1,n2,n3,n4,n5,n6,n7,n8,n9,n10)
  ###expected value
  E5 = (n1+n10)/2
  E4 = (n2+n9)/2
  E3 = (n3+n8)/2
  E2 = (n4+n7)/2
  E1 = (n5+n6)/2
  
  E = c(E5,E4,E3,E2,E1,E1,E2,E3,E4,E5)
  
  chi_sq[j] = sum((O-E)^2/E)
  
  # RR[j] = chi_sq[j]>qchisq(0.95,df=5) ###Ture means reject
  
  RR1[j] = 1-pchisq(chi_sq[j], df=5) <= 0.05/40 ###using bonferroni
}

sum(which(RR1==TRUE) %in% 1:12) ###true signals not symmetric
sum(which(RR1==FALSE) %in% 1:12) ###true signals symmetric
sum(which(RR1==TRUE) %in% setdiff(1:40,1:12)) ###nulls not symmetric
sum(which(RR1==FALSE) %in% setdiff(1:40,1:12))###nulls symmetric

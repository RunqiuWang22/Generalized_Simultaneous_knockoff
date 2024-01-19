setwd("./")
samesig=1
scale=2
N=160
#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
pi=4
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0
p= N/pi #number of group
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
iniseed=1111
M=100

W = matrix(NA,p,M)
for (m in 1:M){
  File01 = paste("Z_Ztilde_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep="")

  if (file.exists(File01)==T){
  Z_Ztilde = read.csv(File01,header=T)
  Z1 = Z_Ztilde$Z1
  Z1tilde = Z_Ztilde$Z1tilde
  Z2 = Z_Ztilde$Z2
  Z2tilde = Z_Ztilde$Z2tilde
  Z3 = Z_Ztilde$Z3
  Z3tilde = Z_Ztilde$Z3tilde
  W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[4]
  
  ###observed value
  n1 = sum(W[j,]<= -Q4)
  n2 = sum(W[j,]> -Q4 & W[j,]<= -Q3)
  n3 = sum(W[j,]> -Q3 & W[j,]<= -Q2)
  n4 = sum(W[j,]> -Q2 & W[j,]<= -Q1)
  n5 = sum(W[j,]> -Q1 & W[j,]<= 0)
  n6 = sum(W[j,]> 0 & W[j,]<= Q1)
  n7 = sum(W[j,]> Q1 & W[j,]<= Q2)
  n8 = sum(W[j,]> Q2 & W[j,]<= Q3)
  n9 = sum(W[j,]> Q3 & W[j,]<= Q4)
  n10 = sum(W[j,]> Q4)
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
scale=2
N=160
#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
pi=4
ps0=0.3
ps1=ps2=ps3=0.15
ps12=ps23=0.05
ps13=0
p= N/pi #number of group
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
iniseed=1111
M=100

W = matrix(NA,p,M)
for (m in 1:M){
  File01 = paste("Z_Ztilde_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep="")
  
  if (file.exists(File01)==T){
    Z_Ztilde = read.csv(File01,header=T)
    Z1 = Z_Ztilde$Z1
    Z1tilde = Z_Ztilde$Z1tilde
    Z2 = Z_Ztilde$Z2
    Z2tilde = Z_Ztilde$Z2tilde
    Z3 = Z_Ztilde$Z3
    Z3tilde = Z_Ztilde$Z3tilde
    W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[4]
  
  ###observed value
  n1 = sum(W[j,]<= -Q4)
  n2 = sum(W[j,]> -Q4 & W[j,]<= -Q3)
  n3 = sum(W[j,]> -Q3 & W[j,]<= -Q2)
  n4 = sum(W[j,]> -Q2 & W[j,]<= -Q1)
  n5 = sum(W[j,]> -Q1 & W[j,]<= 0)
  n6 = sum(W[j,]> 0 & W[j,]<= Q1)
  n7 = sum(W[j,]> Q1 & W[j,]<= Q2)
  n8 = sum(W[j,]> Q2 & W[j,]<= Q3)
  n9 = sum(W[j,]> Q3 & W[j,]<= Q4)
  n10 = sum(W[j,]> Q4)
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


###########################################
samesig=1
scale=2
N=160
#####baseline setting
pb=0.25 #proportion of categorical variables
LL = 3

n1=1000
n2=1000
n3=1000
pi=4
ps0=0.3
ps1=ps2=ps3=0
ps12=ps13=ps23=0.15
p= N/pi #number of group
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
iniseed=1111
M=100

W = matrix(NA,p,M)
for (m in 1:M){
  File01 = paste("Z_Ztilde_n1=",as.character(n1),"n2=",as.character(n2),"n3=",as.character(n3),"N=",as.character(N),"pi=",as.character(pi),"s0=",as.character(s0),"s1=",as.character(s1),"s2=",as.character(s2),"s3=",as.character(s3),"s12=",as.character(s12),"s13=",as.character(s13),"s23=",as.character(s23),"rho1=",as.character(rho1),"rho2=",as.character(rho2),"rho3=",as.character(rho3),"gamma1=",as.character(gamma1),"gamma2=",as.character(gamma2),"gamma3=",as.character(gamma3),"sigma1=",as.character(sigma1),"sigma2=",as.character(sigma2),"sigma3=",as.character(sigma3),"samesig=",as.character(samesig),"q=",as.character(q),"scale=",as.character(scale),"m=",as.character(m),".csv",sep="")
  
  if (file.exists(File01)==T){
    Z_Ztilde = read.csv(File01,header=T)
    Z1 = Z_Ztilde$Z1
    Z1tilde = Z_Ztilde$Z1tilde
    Z2 = Z_Ztilde$Z2
    Z2tilde = Z_Ztilde$Z2tilde
    Z3 = Z_Ztilde$Z3
    Z3tilde = Z_Ztilde$Z3tilde
    W[,m]=(Z1-Z1tilde)*(Z2-Z2tilde)*(Z3-Z3tilde)
  }
  else {W[,m]=NA}
}

RR = rep(NA,40)
RR1 = rep(NA,40)
chi_sq = rep(NA,40)

for (j in 1:40){
  
  W1 = abs(W[j,])
  Q1 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[1]
  Q2 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[2]
  Q3 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[3]
  Q4 = quantile(W1,probs=c(0.2,0.4,0.6,0.8),na.rm=T)[4]
  
  ###observed value
  n1 = sum(W[j,]<= -Q4)
  n2 = sum(W[j,]> -Q4 & W[j,]<= -Q3)
  n3 = sum(W[j,]> -Q3 & W[j,]<= -Q2)
  n4 = sum(W[j,]> -Q2 & W[j,]<= -Q1)
  n5 = sum(W[j,]> -Q1 & W[j,]<= 0)
  n6 = sum(W[j,]> 0 & W[j,]<= Q1)
  n7 = sum(W[j,]> Q1 & W[j,]<= Q2)
  n8 = sum(W[j,]> Q2 & W[j,]<= Q3)
  n9 = sum(W[j,]> Q3 & W[j,]<= Q4)
  n10 = sum(W[j,]> Q4)
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
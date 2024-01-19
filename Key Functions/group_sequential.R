library(glmnet)
library(MASS)

create.groupknockoff_con<-function(X,group){
  N=dim(X)[2] ###updated N: N is number of predictor
  n=dim(X)[1] ###updated n: n is number of observations
  p=nlevels(as.factor(group)) #how many group 
  pi=N/p #the number of predictor in each group
  np<-rep(pi,p)
  cnp<-c(0,cumsum(np))
  
  Sigmahat<-crossprod(X)
  Utilde<-qr.Q(qr(cbind(X,matrix(0,nrow=n,ncol=n-N))))[,(n-N+1):n]
  Utilde<-orthonormalization(Utilde,basis=FALSE)
  A<-list()
  for (i in 1:p){
    Sigmahat_i<-Sigmahat[(cnp[i]+1):cnp[i+1],(cnp[i]+1):cnp[i+1]]
    A[[i]]<-chol(Sigmahat_i)
  }
  AA<-do.call(adiag, A)
  s1<-min(eigen(solve(t(AA))%*%Sigmahat%*%solve(AA))$values)*1.99
  C1<-chol(2*t(AA)%*%diag(rep(s1,sum(np)))%*%AA-t(AA)%*%diag(rep(s1,sum(np)))%*%AA%*%solve(Sigmahat,t(AA)%*%diag(rep(s1,sum(np)))%*%AA))
  Xtilde1<-X%*%(diag(rep(1,sum(np)))-solve(Sigmahat,t(AA)%*%diag(rep(s1,sum(np)))%*%AA))+Utilde%*%C1
  return(Xtilde1)
}

create.groupknockoff_cat<-function(X,group){
  p=dim(X)[2]
  n=dim(X)[1]
  group <- group
  M <- nlevels(as.factor(group))
  
  G <- lapply(1:M, function(i) which(group==i))
  
  Xtilde <- matrix(NA, nrow=n, ncol=p)
  Xtilde <- as.data.frame(Xtilde)
  
  
  XX=list() ###dummy X
  XXtilde=list() ###dummy Xtilde
  
  X_minus_Gm=list() ###factor
  XX_minus_Gm=list() ###dummy
  
  X_Gm_cat=list()
  X_cat_fit=list()
  X_cat_pred=list()
  
  for (m in 1:M){
    
    X_minus_Gm[[m]] <- X[,-G[[m]]]
    ### transfer factor into dummy variable
    XX[[m]]<- as.matrix(model.matrix( ~ ., as.data.frame(X[,G[[m]]]))[, -1])
    XX_minus_Gm[[m]] <- as.matrix(model.matrix( ~ ., as.data.frame(X_minus_Gm[[m]]))[, -1])
    
    
    ###categorical variables
    X_Gm_cat[[m]] <- X[, G[[m]]]
    
    
    if (m==1){
      X_cat_fit[[m]] <- as.matrix(XX_minus_Gm[[m]])
      X_cat_pred[[m]]<- as.matrix(XX_minus_Gm[[m]])
    }
    if (m>1){
      XXtilde[[m]] <- as.matrix(model.matrix(~ ., as.data.frame(Xtilde[, unlist(G[1:(m - 1)])]))[, -1])
      X_cat_fit[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]],XXtilde[[m]]))
      X_cat_pred[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]],XXtilde[[m]]))
    }
    
    ### check the dim of X_Gm_cat[[m]]
    for (j in 1:ncol(X_Gm_cat[[m]])){
      Xtilde[,G[[m]]][,j] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]][,j])
    }
  }  
  return(Xtilde)
}
### regression for continuous variables
sim_con <- function(X,Y){
  n=dim(X)[1]
  # fit using penalized linear regression
  if (ncol(Y)==1){
    cv.con.fit <- cv.glmnet(X,Y, family = "gaussian")
  }
  #fit penalized multitask linear regression
  if (ncol(Y)>1){
  cv.con.fit <- cv.glmnet(X,Y, family = "mgaussian")
  }
  ##predict
  pred<- predict(cv.con.fit, newx=X,s="lambda.min")
  mu_hat <- matrix(pred,nrow=dim(pred)[1], ncol=dim(pred)[2])
  sigma_hat <- (1/n)*t(Y-mu_hat)%*%(Y-mu_hat)
  
  Xtilde_con <- matrix(NA, nrow=n, ncol=dim(Y)[2])
  for (i in 1:n){
    Xtilde_con[i,] <- mvrnorm(n=1, mu_hat[i,], Sigma=sigma_hat)
  }
  
  return(Xtilde_con)
}

###regression for categorical variable
sim_cat <- function(X_fit,X_pred,Y){
  ### X_fit: X used to fit the regression
  ### X_pred: X used to predit Y
  ### Y: outcome variable
  ### levels for factor variables
  classes <- levels(Y)
  K <- length(classes)
  #fit penalized multinomial logistic regression
  cv.multinom_model <- cv.glmnet(X_fit, Y, family = "multinomial")
  mu <- predict(cv.multinom_model, newx=X_pred, type="response", s="lambda.min")
  pi_hat <-  apply(mu, 1, function(prob) rmultinom(n=1, size=1, prob=prob))
  Xtilde_cate <- classes[apply((1:K)*pi_hat, 2, max)]
  Xtilde_cate <- factor(Xtilde_cate, levels=classes)
  return(Xtilde_cate)
}


group_seq_knock <- function(X,group){
  ### for categorical variables, change in to factors
  ### group should be a vector indicating the group: eg: 1111222233334444, the same number of variables should be the same group
  p=dim(X)[2]
  n=dim(X)[1]
  group <- group
  M <- nlevels(as.factor(group))
  ### sort the column as continuous, categorical variables
  index.factor <- as.numeric(which(sapply(as.data.frame(X), is.factor)))
  ### all the variables are continuous variables
  if (length(index.factor)==0){ 
    XX <- data.matrix(X)
    Xtilde <-create.groupknockoff_con(XX,group)
    Xtilde <- as.data.frame(Xtilde)
  }
  ### all the variables are categorical variables
  else if (length(index.factor)==p){
    Xtilde <- create.groupknockoff_cat(X,group)
  }
  
  ### the variable are continuous or categorical variables
  else if (length(index.factor)>0 & length(index.factor)<p){ 
  X <- X[,c(setdiff(1:p,index.factor),index.factor)]
  group <- group[c(setdiff(1:p,index.factor),index.factor)]
  G <- lapply(1:M, function(i) which(group==i))
  
  #Xtilde <- X
  Xtilde <- matrix(NA, nrow=n, ncol=p)
  Xtilde <- as.data.frame(Xtilde)
  index.factor.group=list() #the index of categorical variable within each group
  
  XX=list() ###dummy X
  XXtilde=list() ###dummy Xtilde
  
  X_minus_Gm=list() ###factor
  XX_minus_Gm=list() ###dummy
  Lcon=list() ###length of continuous variables in each group
  X_Gm_con=list()
  X_Gm_cat=list()
  X_con_pred=list()
  X_cat_fit=list()
  X_cat_pred=list()
  
  for (m in 1:M){
    
  X_minus_Gm[[m]] <- X[,-G[[m]]]
  ### transfer factor into dummy variable
  XX[[m]]<- as.matrix(model.matrix( ~ ., as.data.frame(X[,G[[m]]]))[, -1])
  XX_minus_Gm[[m]] <- as.matrix(model.matrix( ~ ., as.data.frame(X_minus_Gm[[m]]))[, -1])
  
  ###factor index in m group
  index.factor.group[[m]] <- as.numeric(which(sapply(as.data.frame(X[,G[[m]]]), is.factor)))
  ###number of continuous variables in each group
  Lcon[[m]] <- ncol(as.data.frame(X[,G[[m]]])) - length(index.factor.group[[m]])
  
  ###within the group, all the variables are categorical variable, no continuous variable
  if (Lcon[[m]]==0) {
    X_Gm_cat[[m]] <- X[, G[[m]]]
    
    if (m==1){
      X_cat_fit[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]]))
      X_cat_pred[[m]]<- as.matrix(cbind(XX_minus_Gm[[m]]))
    }
    if (m>1){
      XXtilde[[m]] <- as.matrix(model.matrix(~ ., as.data.frame(Xtilde[, unlist(G[1:(m - 1)])]))[, -1])
      X_cat_fit[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]],XXtilde[[m]]))
      X_cat_pred[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]],XXtilde[[m]]))
    }
    
    if (length(G[[m]])==1) {
      Xtilde[,G[[m]]] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]])
    }
    else if (length(G[[m]])>1) {
      for (j in 1:length(G[[m]])){
        Xtilde[,G[[m]]][,j] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]][,j])
      }
    }
  }

###within the group, all the variables are continuous variable, no categorical variable
if (length(G[[m]])==Lcon[[m]]) {
 
  X_Gm_con[[m]] <- as.matrix(XX[[m]][,1:Lcon[[m]]])
  
  if (m==1){
    X_con_pred[[m]] <- XX_minus_Gm[[m]]
  }
  
  if (m>1){
    XXtilde[[m]] <- as.matrix(model.matrix(~ ., as.data.frame(Xtilde[, unlist(G[1:(m - 1)])]))[, -1])
    X_con_pred[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]], XXtilde[[m]]))
  }
  
  Xtilde[,G[[m]][1:Lcon[[m]]]] <- sim_con(X=X_con_pred[[m]],X_Gm_con[[m]])
}
  
if (Lcon[[m]]>0 & Lcon[[m]]<length(G[[m]])) {
  ###continuous variable
    X_Gm_con[[m]] <- as.matrix(XX[[m]][,1:Lcon[[m]]])
    
    if (m==1){
      X_con_pred[[m]] <- XX_minus_Gm[[m]]
    }
    
    if (m>1){
      XXtilde[[m]] <- as.matrix(model.matrix(~ ., as.data.frame(Xtilde[, unlist(G[1:(m - 1)])]))[, -1])
      X_con_pred[[m]] <- as.matrix(cbind(XX_minus_Gm[[m]], XXtilde[[m]]))
    }
    
    Xtilde[,G[[m]][1:Lcon[[m]]]] <- sim_con(X=X_con_pred[[m]],X_Gm_con[[m]])
    
  ###categorical variables
    X_Gm_cat[[m]] <- X[, G[[m]][-(1:Lcon[[m]])]]
    
    
    if (m==1){
      X_cat_fit[[m]] <- as.matrix(cbind(X_Gm_con[[m]],XX_minus_Gm[[m]]))
      X_cat_pred[[m]]<- as.matrix(cbind(Xtilde[,G[[m]][1:Lcon[[m]]]], XX_minus_Gm[[m]]))
    }
    if (m>1){
      XXtilde[[m]] <- as.matrix(model.matrix(~ ., as.data.frame(Xtilde[, unlist(G[1:(m - 1)])]))[, -1])
      X_cat_fit[[m]] <- as.matrix(cbind(X_Gm_con[[m]],XX_minus_Gm[[m]],XXtilde[[m]]))
      X_cat_pred[[m]] <- as.matrix(cbind(Xtilde[,G[[m]][1:Lcon[[m]]]], XX_minus_Gm[[m]],XXtilde[[m]]))
    }
    
    ### check the dim of X_Gm_cat[[m]]
    if (length(G[[m]][-(1:Lcon[[m]])])==1) {
      Xtilde[,G[[m]][-(1:Lcon[[m]])]] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]])
    }
    else if (length(G[[m]][-(1:Lcon[[m]])])>1) {
     for (j in 1:length(G[[m]][-(1:Lcon[[m]])])){
     Xtilde[,G[[m]][-(1:Lcon[[m]])]][,j] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]][,j])
     }
    }
  }
}
  
  ###match the original sequence
  Xtilde <- Xtilde[,order(c(setdiff(1:p,index.factor),index.factor))]
  #Xtilde <- data.frame(Xtilde)
  #Xtilde[,index.factor] <- lapply(Xtilde[,index.factor], factor) 
  }
  colnames(Xtilde) <- paste("X",1:p,sep="")
  return(Xtilde)
}

#datal=datagen(n1=500,n2=500,p=40,ps0=0.2,ps1=0.1,ps2=0.1,rho1=0.5,rho2=0.5,sigma1=1,sigma2=-1,samesig=0,scale=1.2)
#data=datal$data
#X=data[which(data$C==1),-c(1,2)]
#group=datal$group
#Xtilde <- group_seq_knock(X,group=group)
#XXtilde=knockoffs_seq(X)

#XC=model.matrix( ~ ., X)[, -1]
#XKnockC=model.matrix( ~ ., Xtilde)[, -1]
#cov=cov(cbind(XC,XKnockC))
#setwd("~/Library/CloudStorage/OneDrive-UniversityofNebraskaMedicalCenter/Group_R/mixed_new_group_K=2/result/")
#write.csv(cov,"cov.csv")

#index.factor <- as.numeric(which(sapply(Xtilde, is.factor)))

#hist(c(as.matrix(X[,-index.factor])))
#hist(c(as.matrix(Xtilde[,-index.factor])))
#hist(c(as.matrix(XXtilde[,-index.factor])))

#X[,index.factor] <- lapply(X[,index.factor], as.numeric)
#Xtilde[,index.factor] <- lapply(Xtilde[,index.factor], as.numeric)
#XXtilde[,index.factor] <- lapply(XXtilde[,index.factor], as.numeric)

#hist(c(as.matrix(X[,index.factor])))
#hist(c(as.matrix(Xtilde[,index.factor])))
#hist(c(as.matrix(XXtilde[,index.factor])))




#cov=cov(cbind(XC,XKnockC))
#setwd("~/Library/CloudStorage/OneDrive-UniversityofNebraskaMedicalCenter/Group_R/mixed_new_group_K=2/result/")
#write.csv(X, "X.csv")
#write.csv(Xtilde, "Xtilde.csv")
#write.csv(cov,"cov.csv")
#hist(c(cov[1:50,1:50]))
#hist(c(cov[51:100,51:100]))

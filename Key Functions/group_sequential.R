library(glmnet)
library(MASS)

### regression for continuous variables
sim_con <- function(X,Y){
  n=dim(X)[1]
  #fit penalized multitask linear regression
  cv.con.fit <- cv.glmnet(X,Y, family = "mgaussian")
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
  p=dim(X)[2]
  n=dim(X)[1]
  group <- group
  M <- max(group) 
  ### sort the column as continuous, categorical variables
  index.factor <- as.numeric(which(sapply(X, is.factor)))
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
  XX[[m]]<- model.matrix( ~ ., X[,G[[m]]])[, -1]
  XX_minus_Gm[[m]] <- model.matrix( ~ ., X_minus_Gm[[m]])[, -1]
  
  ###factor index in m group
  index.factor.group[[m]] <- as.numeric(which(sapply(X[,G[[m]]], is.factor)))
  ###length of continuous variables in each group
  Lcon[[m]] <- ncol(X[,G[[m]]]) - length(index.factor.group[[m]])
  
  ###continuous variable
    X_Gm_con[[m]] <- as.matrix(XX[[m]][,1:Lcon[[m]]])
    
    if (m==1){
      X_con_pred[[m]] <- XX_minus_Gm[[m]]
    }
    
    if (m>1){
      XXtilde[[m]] <- model.matrix(~ ., Xtilde[, unlist(G[1:(m - 1)])])[, -1]
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
      XXtilde[[m]] <- model.matrix(~ ., Xtilde[, unlist(G[1:(m - 1)])])[, -1]
      X_cat_fit[[m]] <- as.matrix(cbind(X_Gm_con[[m]],XX_minus_Gm[[m]],XXtilde[[m]]))
      X_cat_pred[[m]] <- as.matrix(cbind(Xtilde[,G[[m]][1:Lcon[[m]]]], XX_minus_Gm[[m]],XXtilde[[m]]))
    }
    
    Xtilde[,G[[m]][-(1:Lcon[[m]])]] <- sim_cat(X_fit=X_cat_fit[[m]],X_pred=X_cat_pred[[m]],Y=X_Gm_cat[[m]])
}
  
  ###match the original sequence
  Xtilde <- Xtilde[,order(c(setdiff(1:p,index.factor),index.factor))]
  #Xtilde <- data.frame(Xtilde)
  #Xtilde[,index.factor] <- lapply(Xtilde[,index.factor], factor) 
  colnames(Xtilde) <- paste("X",1:p,sep="")
  return(Xtilde)
}


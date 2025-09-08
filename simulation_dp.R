library(BB)
library(cubature)
library(bbmle)
library(stats4)
library(moments)

###################### use the construction of bivariate distribution##########

alpha_1 <- 5
alpha_2 <- 10

beta_1 <- 0.7
beta_2 <- 0.5

negloglik_gamma <- function(par, data) {
  alpha<-par[1]
  beta<-par[2]
  n <- length(data)
  ll <- n * (alpha * log(beta) + lgamma(alpha)) - 
    (alpha - 1) * sum(log(data)) + sum(data) / beta
  return(ll) 
}

cr <- function(alpha_1,alpha_2,beta_1,beta_2,sz){
  shape_X <- matrix(0,1000,1)
  scale_X<- matrix(0,1000,1)
  shape_Y <- matrix(0,1000,1)
  scale_Y<- matrix(0,1000,1)
  gam <- alpha_1+alpha_2
  for (i in 1:1000){
    U <- rgamma(n = sz, shape = gam, scale = beta_1)
    V <- rgamma(n = sz, shape = gam, scale = beta_2)
    W <- rbeta(n = sz, shape1 = alpha_1, shape2 = alpha_2)
    X <- U*W
    Y <- V*W
    init_vals <- c(1, 1)
    result1 <- optim(par = init_vals, fn = negloglik_gamma, data = X, method = "L-BFGS-B",
                     lower = c(1e-6, 1e-6))
    result2 <- optim(par = init_vals, fn = negloglik_gamma, data = Y, method = "L-BFGS-B",
                     lower = c(1e-6, 1e-6))
    
    shape_X[i] <- (result1$par[1]+result2$par[1])/2
    scale_X[i] <- result1$par[2]
    scale_Y[i] <- result2$par[2]
    #shape_Y[i] <- cov(X,Y)*((result1$par[1]+result2$par[1])/2+1)/(result1$par[2]*result2$par[2]*(result1$par[1]+result2$par[1])/2-cov(X,Y))
    shape_Y[i] <- ((result1$par[1]+result2$par[1])/2+1)*cor(X,Y)/(1-cor(X,Y))
  }
  a1_hat<-mean(shape_X)
  b1_hat<- mean(scale_X)
  b2_hat<- mean(scale_Y)
  a2_hat<-mean(shape_Y)
  a1_se<-sd(shape_X)/sqrt(2)
  b1_se<- sd(scale_X)
  a2_se<-sd(shape_Y)/sqrt(12)
  b2_se<- sd(scale_Y)
  return(list(
    "Mean of alpha1_hat" = round(a1_hat, digits = 4),
    "Mean of alpha2_hat" = round(a2_hat, digits = 4),
    "Mean of beta1_hat" = round(b1_hat, digits = 4),
    "Mean of beta1_hat" = round(b2_hat, digits = 4),
    "SE of alpha1_hat" = round(a1_se, digits = 4),
    "SE of alpha2_hat" = round(a2_se, digits = 4),
    "SE of beta1_hat" = round(b1_se, digits = 4),
    "SE of beta1_hat" = round(b2_se, digits = 4)
  ))
  
}

cr1<-cr(alpha_1,alpha_2,beta_1,beta_2,50)
cr1
cr2<-cr(alpha_1,alpha_2,beta_1,beta_2,100)
cr2
cr3<-cr(alpha_1,alpha_2,beta_1,beta_2,300)
cr3
cr4<-cr(alpha_1,alpha_2,beta_1,beta_2,500)
cr4

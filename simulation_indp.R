

alpha_1<- 0.7
alpha_2<- 0.7
beta_1<-0.5
beta_2<-0.5


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
  
  for (i in 1:1000){
X <- rgamma(n = sz, shape = alpha_1, scale = beta_1)
Y <- rgamma(n = sz, shape = alpha_2, scale = beta_2)
init_vals <- c(1, 1)
result1 <- optim(par = init_vals, fn = negloglik_gamma, data = X, method = "L-BFGS-B",
                lower = c(1e-6, 1e-6))

shape_X[i] <- result1$par[1]
scale_X[i] <- result1$par[2]
result2 <- optim(par = init_vals, fn = negloglik_gamma, data = Y, method = "L-BFGS-B",
                 lower = c(1e-6, 1e-6))

shape_Y[i] <- result2$par[1]
scale_Y[i] <- result2$par[2]
  }
  a1_hat<-mean(shape_X)
  b1_hat<- mean(scale_X)
  a2_hat<-mean(shape_Y)
  b2_hat<- mean(scale_Y)
  a1_se<-sd(shape_X)
  b1_se<- sd(scale_X)
  a2_se<-sd(shape_Y)
  b2_se<- sd(scale_Y)
  return(list(
    "Mean of alpha1_hat" = a1_hat,
    "Mean of alpha2_hat" = a2_hat,
    "Mean of beta1_hat" = b1_hat,
    "Mean of beta1_hat" = b2_hat,
    "SE of alpha1_hat" = a1_se,
    "SE of alpha2_hat" = a2_se,
    "SE of beta1_hat" = b1_se,
    "SE of beta1_hat" = b2_se
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


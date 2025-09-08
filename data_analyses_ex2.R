
################## Real Example 2  #################
library(ggplot2)
getwd()
setwd("G:/Ziyuan 2025")
data<- read.csv2("Cleaned_data_Ex2.csv",header = FALSE, sep = ",")
p01 <- data[,1]   
p04 <- data[,2]  
p05<- data[,3]
p06<- data[,4]
p07<- data[,5]
p10<- data[,6]
p11<- data[,7]

X_m <- matrix(p01,nrow=1079, ncol=1)
Y_m <- matrix(p10,nrow=1079, ncol=1)

X <- as.numeric(X_m)
Y <- as.numeric(Y_m)
sd(X)
sd(Y)
mean(X)
mean(Y)
cov(X,Y)
cor(X,Y)
skewness(X)
skewness(Y)
negloglik_gamma <- function(par, data) {
  alpha<-par[1]
  beta<-par[2]
  n <- length(data)
  ll <- n * (alpha * log(beta) + lgamma(alpha)) - 
    (alpha - 1) * sum(log(data)) + sum(data) / beta
  return(ll) 
}

init_vals <- c(1, 1)
result1 <- optim(par = init_vals, fn = negloglik_gamma, data = X, method = "L-BFGS-B",
                 lower = c(1e-6, 1e-6))
result2 <- optim(par = init_vals, fn = negloglik_gamma, data = Y, method = "L-BFGS-B",
                 lower = c(1e-6, 1e-6))

shape_X <- (result1$par[1]+result2$par[1])/2
scale_X <- result1$par[2]
scale_Y <- result2$par[2]
shape_Y <- ((result1$par[1]+result2$par[1])/2+1)*cor(X,Y)/(1-cor(X,Y))

cat(paste("alpha_1 =", shape_X, 
                    ", alpha_2 =", shape_Y, 
                    ", beta_1 =", scale_X, 
                    ", beta_2 =", scale_Y))

hist(X,
     prob = TRUE,              # Show density, not frequency
     main = "Histogram of the average LOS in nervous system.",
     xlab = "Average LOS (Day)",
     ylab = "Density"
)

curve(dgamma(x, 8.938868,  scale =  0.7116,),
      col="black", lwd=2, add=TRUE, lty=1)

hist(Y,
     prob = TRUE,              # Show density, not frequency
     xlim = c(0, max(Y)-1), 
     breaks = 13,
     main = "Histogram of the average LOS in endocrine.",
     xlab = "Average LOS (Day)",
     ylab = "Density"
)

curve(dgamma(x, 8.2630,  scale =  0.6898,),
      col="black", lwd=2, add=TRUE, lty=1)



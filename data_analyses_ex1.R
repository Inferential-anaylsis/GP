library(ggplot2)
library(readxl)
library(fitdistrplus)
library(sn)

########## James_River ##########

# Read data
data <- read_excel("/Users/wangzi/Downloads/Cleaned_data_Ex1.xlsx")
flood_data <- data$James_River_peak_value * 0.0283168    # Convert unit from cubic feet/second to cubic meters/second

# Distribution fitting
fit_gamma <- fitdist(flood_data, "gamma")
fit_exp <- fitdist(flood_data, "exp")
fit_sn <- selm(flood_data ~ 1, family = "SN")
sn_params <- coef(fit_sn, "DP")

# Skew-normal density function
dnorm_skew <- function(x, xi, omega, alpha) {
  2 * dnorm((x - xi) / omega) * pnorm(alpha * (x - xi) / omega) / omega
}

# Create fitted curve dataframe
x_vals <- seq(0, max(flood_data), length.out = 1000)
dens_data <- data.frame(
  x = rep(x_vals, 3),
  density = c(
    dgamma(x_vals, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]),
    dexp(x_vals, rate = fit_gamma$estimate["rate"]),
    dnorm_skew(x_vals, xi = sn_params["xi"], omega = sn_params["omega"], alpha = sn_params["alpha"])
  ),
  Distribution = factor(rep(c("Gamma", "Exponential", "Skew-Normal"), each = length(x_vals)))
)

# Plot
ggplot(data, aes(x = flood_data)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "white", color = "black", boundary = 0) +
  geom_line(data = dens_data, aes(x = x, y = density, color = Distribution), size = 0.7) +
  scale_color_manual(values = c("Gamma" = "red", "Exponential" = "green", "Skew-Normal" = "blue")) +
  theme_minimal(base_size = 14) +
  labs(title = "James River Flood Peak Data with Fitted Distributions",
       x = "Peak Value", y = "Density", color = "Fitted Distribution") +
  theme(legend.position = "right")


# Collect AIC and BIC for all models
AIC_sn <- AIC(fit_sn)
BIC_sn <- BIC(fit_sn)

results <- data.frame(
  Distribution = c("Gamma", "Exponential", "Skew-Normal"),
  AIC = c(fit_gamma$aic, fit_exp$aic, AIC_sn),
  BIC = c(
    AIC(fit_gamma, k = log(length(flood_data))),
    AIC(fit_exp, k = log(length(flood_data))),
    BIC_sn
  )
)

print("Model Fit Comparison using AIC and BIC:")
print(results)

# Compute BIC for Skew-Normal
n <- length(flood_data)
logLik_sn <- logLik(fit_sn)
n_params_sn <- length(coef(fit_sn, "DP"))
BIC_sn_manual <- -2 * as.numeric(logLik_sn) + n_params_sn * log(n)
BIC_sn_manual


########## Rowlett_Ck ##########

# Read data
data <- read_excel("/Users/wangzi/Downloads/Cleaned_data_Ex1.xlsx")
flood_data <- data$Rowlett_Ck_peak_value * 0.0283168

# Distribution fitting
fit_gamma <- fitdist(flood_data, "gamma")
fit_exp <- fitdist(flood_data, "exp")
fit_sn <- selm(flood_data ~ 1, family = "SN")
sn_params <- coef(fit_sn, "DP")

# Skew-normal density function
dnorm_skew <- function(x, xi, omega, alpha) {
  2 * dnorm((x - xi) / omega) * pnorm(alpha * (x - xi) / omega) / omega
}

# Create fitted curve dataframe
x_vals <- seq(0, max(flood_data), length.out = 1000)
dens_data <- data.frame(
  x = rep(x_vals, 3),
  density = c(
    dgamma(x_vals, shape = fit_gamma$estimate["shape"], rate = fit_gamma$estimate["rate"]),
    dexp(x_vals, rate = fit_gamma$estimate["rate"]),
    dnorm_skew(x_vals, xi = sn_params["xi"], omega = sn_params["omega"], alpha = sn_params["alpha"])
  ),
  Distribution = factor(rep(c("Gamma", "Exponential", "Skew-Normal"), each = length(x_vals)))
)

# Plot
ggplot(data, aes(x = flood_data)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "white", color = "black", boundary = 0) +
  geom_line(data = dens_data, aes(x = x, y = density, color = Distribution), size = 0.7) +
  scale_color_manual(values = c("Gamma" = "red", "Exponential" = "green", "Skew-Normal" = "blue")) +
  theme_minimal(base_size = 14) +
  labs(title = "Rowlett Creek Flood Peak Data with Fitted Distributions",
       x = "Peak Value", y = "Density", color = "Fitted Distribution") +
  theme(legend.position = "right")


# Collect AIC and BIC for all models
AIC_sn <- AIC(fit_sn)
BIC_sn <- BIC(fit_sn)

results <- data.frame(
  Distribution = c("Gamma", "Exponential", "Skew-Normal"),
  AIC = c(fit_gamma$aic, fit_exp$aic, AIC_sn),
  BIC = c(
    AIC(fit_gamma, k = log(length(flood_data))),
    AIC(fit_exp, k = log(length(flood_data))),
    BIC_sn
  )
)

print("Model Fit Comparison using AIC and BIC:")
print(results)

# Compute BIC for Skew-Normal
n <- length(flood_data)
logLik_sn <- logLik(fit_sn)
n_params_sn <- length(coef(fit_sn, "DP"))
BIC_sn_manual <- -2 * as.numeric(logLik_sn) + n_params_sn * log(n)
BIC_sn_manual



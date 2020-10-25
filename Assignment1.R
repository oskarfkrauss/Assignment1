
## Q3 a)

Y1 <- NULL
Y2 <- NULL

for (i in 1:500){ #complete datasets
  
  Z1 <- rnorm(1)
  Z2 <- rnorm(1)

  Y1[i] <- 1 + Z1
  Y2[i] <- 5 + 2*Z1 + Z2
  
}

Y2_MAR <- NULL

for (i in 1:500){ #impose missingness based on obs (MAR)
  
  a <- 2
  b <- 0 
  Z3 <- rnorm(1)
  
  if (a*(Y1[i] - 1) + b*(Y2[i] - 5) + Z3 < 0){
  
    Y2_MAR[i] <- NA
    
    
  }else {
    
    Y2_MAR[i] <- Y2[i]
    
  }
  
}

pdf(file = 'MAR_plot.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)


plot(density(Y2), xlab = 'Value', lwd = 2, col = "black", main = "MAR", 
     ylim=c(0,max(density(na.omit(Y2_MAR))$y, density(na.omit(Y2))$y)))
lines(density(na.omit(Y2_MAR)), lwd = 2, col = "red")
lines(c(mean(Y2), mean(Y2)), c(0, 0.3), lty = 2, lwd = 1, col = "black")
lines(c(mean(na.omit(Y2_MAR)), mean(na.omit(Y2_MAR))), c(0, 0.3), lty = 2, lwd = 1, col = "red")
legend(-3, 0.2, legend = c("Complete data","Mean", "MAR data", "Mean"), 
       col = c("black", "black", "red", "red"), 
       lty = c(1, 2, 1, 2), lwd = c(2, 1, 2, 1), bty = "n")

dev.off()

# b) Stochastic imputation for MAR data

fit_sri1 <- lm(Y2_MAR ~ Y1)

data <- data.frame(Complete = Y2)

predicted_sri1 <- predict(fit_sri1, data) + rnorm(length(Y2_MAR), 0, sigma(fit_sri1))

Y2_sri1 <- ifelse(is.na(Y2_MAR), predicted_sri1, Y2)

pdf(file = 'SRI_MAR_plot.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)

plot(density(Y2), xlab = 'Value', lwd = 2, col = "black", main = "Stochastic Regression Imputation (MAR)", 
     ylim=c(0,max(density(Y2)$y, density(Y2_sri1)$y)))
lines(density(Y2_sri1), lwd = 2, col = "red")
lines(c(mean(Y2), mean(Y2)), c(0, 0.3), lty = 2, lwd = 1, col = "black")
lines(c(mean(Y2_sri1), mean(Y2_sri1)), c(0, 0.3), lty = 2, lwd = 1, col = "red")
legend(8, 0.2, legend = c("Complete data","Mean", "Stochastic Imputation", "Mean"), 
       col = c("black", "black", "red", "red"), 
       lty = c(1, 2, 1, 2), lwd = c(2, 1, 2, 1), bty = "n")

dev.off()

## c)

Y2_MNAR <- NULL

for (i in 1:500){ #impose missingness dependent on missing data (MNAR)
  
  a <- 0
  b <- 2 
  Z3 <- rnorm(1)
  
  if (a*(Y1[i] - 1) + b*(Y2[i] - 5) + Z3 < 0){
    
    Y2_MNAR[i] <- NA
    
    
  }else {
    
    Y2_MNAR[i] <- Y2[i]
    
  }
  
}

pdf(file = 'MNAR_plot.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)

plot(density(Y2), xlab = 'Value', lwd = 2, col = "black", main = "MNAR", 
     ylim=c(0,max(density(na.omit(Y2_MNAR))$y, density(na.omit(Y2))$y)))
lines(density(na.omit(Y2_MNAR)), lwd = 2, col = "red")
lines(c(mean(Y2), mean(Y2)), c(0, 0.3), lty = 2, lwd = 1, col = "black")
lines(c(mean(na.omit(Y2_MNAR)), mean(na.omit(Y2_MNAR))), c(0, 0.3), lty = 2, lwd = 1, col = "red")
legend(-3, 0.2, legend = c("Complete data","Mean", "MNAR data", "Mean"), 
       col = c("black", "black", "red", "red"), 
       lty = c(1, 2, 1, 2), lwd = c(2, 1, 2, 1), bty = "n")

dev.off()

# d) Stochastic imputation for MNAR data

fit_sri2 <- lm(Y2_MNAR ~ Y1)

predicted_sri2 <- predict(fit_sri2, data) + rnorm(length(Y2_MNAR), 0, sigma(fit_sri2))

Y2_sri2 <- ifelse(is.na(Y2_MNAR), predicted_sri2, Y2)

pdf(file = 'SRI_MNAR_plot.pdf' ,   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 4)

plot(density(Y2), xlab = 'Value', lwd = 2, col = "black", main = "Stochastic Regression Imputation (MNAR)", 
     ylim=c(0,max(density(Y2)$y, density(Y2_sri2)$y)))
lines(density(Y2_sri2), lwd = 2, col = "red")
lines(c(mean(Y2), mean(Y2)), c(0, 0.3), lty = 2, lwd = 1, col = "black")
lines(c(mean(Y2_sri2), mean(Y2_sri2)), c(0, 0.3), lty = 2, lwd = 1, col = "red")
legend(8, 0.2, legend = c("Complete data","Mean", "Stochastic Imputation", "Mean"), 
       col = c("black", "black", "red", "red"), 
       lty = c(1, 2, 1, 2), lwd = c(2, 1, 2, 1), bty = "n")

dev.off()

## Q4 a)

load("databp.Rdata")

df <- databp

rm(databp)

df_cc <- na.omit(df)
mean_cc <- mean(df_cc$recovtime)
cor_cc_1 <- cor(df_cc$recovtime, df_cc$logdose) 
cor_cc_2 <- cor(df_cc$recovtime, df_cc$bloodp)

## b)

recov_mi <- ifelse(is.na(df$recovtime), mean_cc, df$recovtime)

df_mi <- df
df_mi$recovtime <- recov_mi

mean_mi <- mean(df_mi$recovtime)
cor_mi_1 <- cor(df_mi$recovtime, df_mi$logdose)
cor_mi_2 <- cor(df_mi$recovtime, df_mi$bloodp)

## c)

fit <- lm(df$recovtime ~ df$logdose + df$bloodp)

predicted_ri <- predict(fit, df)
recov_ri <- ifelse(is.na(df$recovtime), predicted_ri, df$recovtime)

df_ri <- df
df_ri$recovtime <- recov_ri 

mean_ri <- mean(df_ri$recovtime)
cor_ri_1 <- cor(df_ri$recovtime, df_ri$logdose)
cor_ri_2 <- cor(df_ri$recovtime, df_ri$bloodp)

## d)

predicted_sri <- predict(fit, df) + rnorm(nrow(df), 0, sigma(fit))
recov_sri <- ifelse(is.na(df$recovtime), predicted_sri, df$recovtime)
recov_sri <- ifelse(recov_sri < 0, 0, recov_sri) # due to variability, recovery times may be imputed as negative

df_sri <- df
df_sri$recovtime <- recov_sri 

mean_sri <- mean(df_sri$recovtime)
cor_sri_1 <- cor(df_sri$recovtime, df_sri$logdose)
cor_sri_2 <- cor(df_sri$recovtime, df_sri$bloodp)

## e)

predicted_ppm <- predict(fit, df) # all predicted values

min_sq_diff <- NULL
candidate <- NULL

for (i in 1:length(predicted_ppm)){
  
  if (is.na(df$recovtime[i]) == T){

    min_sq_diff <- (predicted_ppm - predicted_ppm[i])^2 # find min sq difference 
    candidate[i] <- df$recovtime[min_sq_diff == min(min_sq_diff[-i])] # find observed value with lowest min sq difference to missing value
    
    '
    
    if (predicted_ppm[i] - (predicted_ppm[i]  sqrt(min_sq_diff[i])) > 0){
      
      candidate[i] <- as.vector(predicted_ppm[predicted_ppm ==(predicted_ppm[i] + sqrt(min_sq_diff[i]))])
        
    }else{
      
      candidate[i] <- as.vector(predicted_ppm[predicted_ppm ==(predicted_ppm[i] - sqrt(min_sq_diff[i]))])
      
    '
    
  }

}

recov_ppm <- ifelse(is.na(df$recovtime), candidate, df$recovtime)
df_ppm <- df
df_ppm$recovtime <- recov_ppm

mean_ppm <- mean(df_ppm$recovtime)
se_ppm <- sd(df_ppm$recovtime)/sqrt(length(df_ppm$recovtime))
cor_ppm_1 <- cor(df_ppm$recovtime, df_ppm$logdose)
cor_ppm_2 <- cor(df_ppm$recovtime, df_ppm$bloodp)



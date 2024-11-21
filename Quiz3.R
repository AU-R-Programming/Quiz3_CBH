#Good Luck

install.packages("wv")
install.packages("MASS")
install.packages("wavelets")
#wvar function



#1
theta<-2
J <- floor(log2(d)) - 1
J<-4
wv_theo <-function(theta, J) {
  nu_theta<-numeric(J)
  for (j in 1:J){
    out<- wavelets::wt.filter(rep(0,2^j), filter = "haar", level = j)
    hj <- out@h
    nu_theta[j] <- theta * sum(hj^2)
  }
  return(nu_theta)
}

#2

gmwm_loss <- function(theta, nu_hat) {
  
  nu_theta <- wv_theo(theta, J)
  loss<-sum((nu_hat-nu_theta)^2)
  return(loss)
}


#3

gmwm <- function(x,J, robust=FALSE , start = var(x)) {
  
  
  
  if(robust) {
    
    nu_hat <- wv::wvar(x, robust=TRUE)$variance
    
  } else {
    
    nu_hat <- wv::wvar(x)$variance
    
  }
  loss_function <- function(theta){
    gmwm_loss(theta,nu_hat)
  }
  theta_hat <- suppressWarnings(optim(start, loss_function)$par)
  
  return(theta_hat)
  
}
#4

B <- 100
d <- 1000
cont <- 10
true_theta <- 2

theta_Xt_std <- theta_Xt_rob <- theta_Zt_std <- theta_Zt_rob <- numeric(B)


for(b in 1:B) {
  
  X <-MASS::mvrnorm(n = 1, mu=rep(0,d), Sigma=diag(true_theta,d)) 
  Z <- X
  Z[sample(1:d, cont)] <- rexp(10, rate= 0.5)
  
  theta_Xt_rob <- gmwm(X,J = floor(log2(d)) - 1, robust = TRUE)
  theta_Xt_std <- gmwm(X, J = floor(log2(d)) - 1, robust = FALSE)
  
  theta_Zt_rob <- gmwm(Z,J = floor(log2(d)) - 1, robust = TRUE)
  theta_Zt_std <- gmwm(Z,J = floor(log2(d)) - 1, robust = FALSE)
  
}
#5

mae <- function(true_theta, estimates){
  abs_error <- abs(estimates - true_theta)
  median_abs_error<-median(abs_error)
  return(median_abs_error)
}

mae_Xt_std <- mae(true_theta, theta_Xt_std)
mae_Xt_rob <- mae(true_theta, theta_Xt_rob)
mae_Zt_std <- mae(true_theta, theta_Zt_std)
mae_Zt_rob <- mae(true_theta, theta_Zt_rob)

cat("MAE for standard estimator on X:", mae_Xt_std, "\n")
cat("MAE for robust estimator on X:", mae_Xt_rob, "\n")
cat("MAE for standard estimator on Z:", mae_Zt_std, "\n")
cat("MAE for robust estimator on Z:", mae_Zt_rob, "\n")

par(mfrow = c(2, 2))  

boxplot(theta_Xt_std, main = "Standard Estimator (X)", ylab = "Estimate")
abline(h = true_theta, col = "red", lwd = 2)  # Add true_theta line

boxplot(theta_Xt_rob, main = "Robust Estimator (X)", ylab = "Estimate")
abline(h = true_theta, col = "red", lwd = 2)  # Add true_theta line

boxplot(theta_Zt_std, main = "Standard Estimator (Z)", ylab = "Estimate")
abline(h = true_theta, col = "red", lwd = 2)  # Add true_theta line

boxplot(theta_Zt_rob, main = "Robust Estimator (Z)", ylab = "Estimate")
abline(h = true_theta, col = "red", lwd = 2)  # Add true_theta line


#There are differences in wether we include the indication of robust or not
#https://chatgpt.com/share/673f6431-655c-8009-83ea-51633a297800

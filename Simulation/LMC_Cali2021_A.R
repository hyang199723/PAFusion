# Apply lmc to california 2021 data
library(MCMCpack)
library(geodist)

exp_corr <- function(d,range){exp(-d/range)}
digamma  <- function(x,a,b){
  a*log(b) - (a+1)*log(x) - b/x - lgamma(a)
}


fft_real <- function(dat,inverse=FALSE){
  if(!inverse){
    x  <- dat
    n  <- length(x)
    n2 <- floor(n/2)
    y  <- fft(x,inverse=FALSE)
    if(n%%2==0){
      X1     <- Re(y)[1:(n2+1)]
      X2     <- Im(y)[2:(n2)]
    }
    if(n%%2!=0){
      X1     <- Re(y)[1:(n2+1)]
      X2     <- Im(y)[2:(n2+1)]
    }
    out <- c(X1,X2)
  }
  if(inverse){
    X  <- dat
    n  <- length(X)
    n2 <- floor(n/2)
    if(n%%2==0){
      Y1    <- c(X[1:(n2+1)],X[n2:2])
      Y2    <- c(0,X[(n2+2):n],0,-X[n:(n2+2)])
    }
    if(n%%2!=0){
      Y1    <- c(X[1:(n2+1)],X[(n2+1):2])
      Y2    <- c(0,X[(n2+2):n],-X[n:(n2+2)])
    }
    y   <- complex(n, real = Y1, imaginary = Y2)
    out <- Re(fft(y/n,inverse=TRUE))
  }
  return(out)}


# Input specification:
# Y1: Type 1 response value; Matrix with (number of observation) * (number of time steps)
#     Y1 can have missing values
# Y2: Type 2 response value; Matrix with (number of observation) * (number of time steps)
#     Y2 can have missing values
# s1: locations of Type 1 response
# s2: locations of Type 2 response
# X: Covariates for U and V
LMC_fit=function(Y1,Y2, X, s1,s2, 
                 rangeu=2,rangev=4,
                 initA=0,updateA=FALSE,
                 c1=100, c2=100, c3=1, eps=0.1,
                 iters=3000, burn=1000,
                 match1=NULL,match2=NULL)
{
  n1 = dim(Y1)[1]
  n2 = dim(Y2)[1]
  nt = dim(Y1)[2]
  ns = n1 + n2
  p = dim(X)[3]
  p2 = p - 3 # Without smoke indicators
  
  # Required variable
  Y_star  <- matrix(0, nrow = ns, ncol = nt)
  Y1_star <- matrix(0, nrow = n1, ncol = nt)
  Y2_star <- matrix(0, nrow = n2, ncol = nt)
  
  # Take DFT of X
  X_star = array(NA, dim=c(ns, nt, p))
  for(i in 1:(n1+n2)){for (j in 1:p) {
    X_star[i, , j] = fft_real(X[i, , j])
  }}
  X1_star = X_star[1:n1,,]
  X2_star = X_star[(n1+1):(n1+n2),,]
  coords <- rbind(s1, s2)
  
  #! Scaled this
  if (nt%%2 == 0) {
    omega <- c(1:(nt/2+1), 2:(nt/2))
  } else {
    t = floor(nt/2)
    omega <- c(1:(t+1), 2:(t+1))
  }
  omega_s <- scale(omega)
  
  # Let's add a quandratic term here
  # Now it is linear term
  # X_alpha <- cbind(1,omega_s)
  # p_alpha <- ncol(X_alpha)
  
  # Complete missing data
  m1     <- is.na(Y1)
  m2     <- is.na(Y2)
  Y1[m1] <- mean(Y1, na.rm = T)
  Y2[m2] <- mean(Y2, na.rm = T)
  
  
  # Generate Initial Values for U1, U2, V2, Al, and sigmas
  for(j in 1:n1){Y1_star[j,] <- fft_real(as.numeric(Y1[j,]))}
  for(j in 1:n2){Y2_star[j,] <- fft_real(as.numeric(Y2[j,]))}
  Y_star <- rbind(Y1_star,Y2_star)
  
  sig1  <- rep(1.0,nt)
  sig2  <- rep(0.1,nt)
  tau1  <- var(as.vector(Y1))/10
  tau2  <- var(as.vector(Y2))/10
  
  betau = c(2, 0.118, 0.064, 0.007, 0.022, 0.049)
  betav = c(1, -0.002, 0.012)
  
  A         <- rep(initA, nt)
  
  U <- Y_star
  for(j in 1:nt){
    U[,j] <- 0.5*(Y_star[,j]-X_star[,j,]%*%betau) + X_star[,j,]%*%betau
  }
  U1 <- U[1:n1,]
  U2 <- U[n1 + (1:n2),]
  V2 <- U2-initA*U2
  U  <- rbind(U1,U2)
  
  
  X_theta <- cbind(1,omega_s)
  p_theta <- ncol(X_theta)
  theta1  <- rep(0,2*p_theta)
  theta2  <- rep(0,2*p_theta)
  
  Z1     <- matrix(0,n1,nt)
  Z2     <- matrix(0,n2,nt)
  
  # Book keeping
  betau_all  <- matrix(data = 0, nrow = p, ncol = iters)
  betav_all  <- matrix(data = 0, nrow = p2, ncol = iters)
  # beta_a_all <- matrix(0, nrow = p_alpha, ncol = iters)
  U1_all     <- array(data = 0, dim = c(n1, nt, iters))
  U2_all     <- array(data = 0, dim = c(n2, nt, iters))
  V2_all     <- array(data = 0, dim = c(n2, nt, iters))
  tau_all    <- matrix(0, nrow = 2, ncol = iters)
  sig1_all   <- matrix(0, nrow = nt, ncol = iters)
  sig2_all   <- matrix(0, nrow = nt, ncol = iters)
  theta1_all <- matrix(0, nrow = 2*p_theta, ncol = iters)
  theta2_all <- matrix(0, nrow = 2*p_theta, ncol = iters)  
  Z1_all     <- array(data = 0, dim = c(n1, nt, iters))
  Z2_all     <- array(data = 0, dim = c(n2, nt, iters))
  Y1_all     <- array(data = 0, dim = c(n1, nt, iters))
  # Set up storage for big matrices
  Z1_mn      <- matrix(0,n1,nt)
  Z1_var     <- matrix(0,n1,nt)
  Z2_mn      <- matrix(0,n2,nt)
  Z2_var     <- matrix(0,n2,nt)
  match1_mn  <- rep(0,n1)
  match1_var <- rep(0,n1)
  match2_mn  <- rep(0,n2)
  match2_var <- rep(0,n2)
  
  # Get distance matrix:
  dist_full = as.matrix(dist(coords))
  #dist_full = as.matrix(geodist(coords, measure = 'geodesic' )/1000)
  dist22 = dist_full[(n1+1):(n1+n2), (n1+1):(n1+n2)]
  SigmaU <- exp_corr(dist_full, rangeu)
  eigU   <- eigen(SigmaU)
  U_G    <- eigU$vectors
  U_D    <- eigU$values
  SigmaU_inv <- U_G %*% diag(1/U_D) %*% t(U_G)
  
  # Construct S1 and S2
  SigmaU11 <- SigmaU[1:n1, 1:n1]
  SigmaU12 <- SigmaU[1:n1, (n1+1):(n1+n2)]
  SigmaU21 <- t(SigmaU12)
  SigmaU22 <- SigmaU[(n1+1):(n1+n2), (n1+1):(n1+n2)]
  S1       <- SigmaU11 - SigmaU12 %*% solve(SigmaU22) %*% SigmaU21
  S2       <- SigmaU22 - SigmaU21 %*% solve(SigmaU11) %*% SigmaU12
  A12      <- SigmaU12 %*% solve(SigmaU22)
  A21      <- SigmaU21 %*% solve(SigmaU11)
  
  # Eigen decomposition of S1 and S2
  eigS1  <- eigen(S1)
  eigS2  <- eigen(S2)
  S1_G   <- eigS1$vectors
  S1_D   <- eigS1$values
  S2_G   <- eigS2$vectors
  S2_D   <- eigS2$values
  S1_inv <- S1_G %*% diag(1/S1_D) %*% t(S1_G)
  S2_inv <- S2_G %*% diag(1/S2_D) %*% t(S2_G)
  
  # Covariance for V
  SigmaV     <- exp_corr(dist22, rangev)
  eigV       <- eigen(SigmaV)
  V_G        <- eigV$vectors
  V_D        <- eigV$values
  SigmaV_inv <- V_G %*% diag(1/V_D) %*% t(V_G)
  
  
  acc <- att <- MH <- rep(0.25,2*p_theta)
  
  for(i in 1:iters){
    print(round(100*i/iters))
    
    ##############################################:
    ####     Transform to spatial land       #####:
    ##############################################:
    
    for(j in 1:n1){Z1[j,] <- fft_real(U1[j,],inverse=TRUE)}
    #    for(j in 1:n2){Z2[j,] <- fft_real(U2[j,],inverse=TRUE)}
    
    # Remove the bias correction covariate effects from U
    # This only works when A=1!!!
    u2 <- U2
    for(j in 1:6){for(l in 1:nt){u2[,l]<-u2[,l]-A[l]*X2_star[,l,j]*betau[j]}}
    for(j in 1:n2){Z2[j,] <- fft_real(u2[j,],inverse=TRUE)}
    
    if(i>burn){
      Z1_mn   <- Z1_mn  + Z1/(iters-burn)
      Z1_var  <- Z1_var + Z1*Z1/(iters-burn)
      Z2_mn   <- Z2_mn  + Z2/(iters-burn)
      Z2_var  <- Z2_var + Z2*Z2/(iters-burn)
      
      if(!is.null(match1)){for(j in 1:n1){
        temp          <- mean(Z1[j,]-Z1[j,match1[j,]])
        match1_mn[j]  <- match1_mn[j]  + temp/(iters-burn)
        match1_var[j] <- match1_var[j] + temp*temp/(iters-burn)
      }}
      if(!is.null(match2)){for(j in 1:n2){
        temp          <- mean(Z2[j,]-Z2[j,match2[j,]])
        match2_mn[j]  <- match2_mn[j]  + temp/(iters-burn)
        match2_var[j] <- match2_var[j] + temp*temp/(iters-burn)
      }}
    } 
    
    for(j in 1:n2){Z2[j,] <- fft_real(A*U2[j,]+V2[j,],inverse=TRUE)}
    
    
    ##############################################:
    ####  IMPUTE MISSING DATA (real space)   #####:
    ##############################################:
    
    Y1[m1] <- rnorm(sum(m1),0,sqrt(tau1)) + Z1[m1]
    Y2[m2] <- rnorm(sum(m2),0,sqrt(tau2)) + Z2[m2]
    Y1_all[,,i] <- Y1
    
    tau1 <- 1/rgamma(1,n1*nt/2+eps,sum((Y1-Z1)^2)/2+eps)
    tau2 <- 1/rgamma(1,n2*nt/2+eps,sum((Y2-Z2)^2)/2+eps)
    
    tau1_star <- 0.5*nt*tau1
    tau2_star <- 0.5*nt*tau2
    
    tau_all[,i] <- c(tau1,tau2)
    
    ##############################################:
    ####     Transform to spectral land      #####:
    ##############################################:
    
    for(j in 1:n1){Y1_star[j,] <- fft_real(as.numeric(Y1[j,]))}
    for(j in 1:n2){Y2_star[j,] <- fft_real(as.numeric(Y2[j,]))}
    Y_star <- rbind(Y1_star, Y2_star)
    
    ##############################################:
    ####           Update U and V            #####:
    ##############################################:
    
    for (l in 1:nt) {
      # Matrix notation
      X_star_l  <- X_star[,l,]
      X1_star_l <- X1_star[,l,]
      X2_star_l <- X2_star[,l,]
      X2_star_l_p2 <- X2_star[,l,1:p2]
      Y_star_l  <- Y_star[,l]
      Y1_star_l <- Y1_star[,l]
      Y2_star_l <- Y2_star[,l]
      
      # Sample U1
      newDiag1    <- (1/tau1_star) + (1/sig1[l]) * (1/S1_D)
      meanU1_p1   <- (1/tau1_star) * Y1_star_l
      mu1         <- X1_star_l %*% betau + A12%*%(U2[,l] - X2_star_l %*% betau) 
      meanU1_p2   <- (1/sig1[l]) * (S1_inv %*% mu1)
      meanU1      <- t(S1_G) %*% (meanU1_p1 + meanU1_p2)
      U1[, l] <- S1_G%*%(meanU1/newDiag1 + rnorm(n1, 0, 1/sqrt(newDiag1)))
      
      # Sample U2
      newDiag2    <- (A[l]^2)/tau2_star + (1/sig1[l]) * (1/S2_D)
      meanU2_p1   <- (A[l]/tau2_star) * (Y2_star_l - V2[, l])
      mu2         <- X2_star_l %*% betau + A21%*%(U1[,l] - X1_star_l %*% betau) 
      meanU2_p2   <- (1/sig1[l]) * (S2_inv %*% mu2)
      meanU2      <- t(S2_G) %*% (meanU2_p1 + meanU2_p2)
      U2[, l]     <- S2_G%*%(meanU2/newDiag2 + rnorm(n2, 0, 1/sqrt(newDiag2)))
      
      U[, l]      <- c(U1[, l], U2[, l])
      
      # Sample V2
      newDiagV    <- (1/tau2_star) + (1/sig2[l]) * (1/V_D)
      meanV_p1    <- (1/tau2_star)*(Y2_star_l - A[l]*U2[, l])
      meanV_p2    <- (1/sig2[l]) * SigmaV_inv %*% (X2_star_l_p2 %*% betav)
      meanV       <- t(V_G) %*% (meanV_p1 + meanV_p2)
      V2[, l]     <- V_G %*% (meanV/newDiagV + rnorm(n2, 0, 1/sqrt(newDiagV)))
      
      #! Added this model for the prior variances
      aU1 <- exp(X_theta%*%theta1[1:p_theta])
      bU1 <- exp(X_theta%*%theta1[1:p_theta + p_theta])
      aU2 <- exp(X_theta%*%theta2[1:p_theta])
      bU2 <- exp(X_theta%*%theta2[1:p_theta + p_theta])
      
      # Sample sig1
      a <- ns/2 + aU1[l]
      r <- U[, l] - X_star_l %*% betau
      b <- t(r)%*%SigmaU_inv%*%r/2 + bU1[l]
      sig1[l] <- rinvgamma(1, a, b)
      
      # Sample sig2
      a <- n2/2 + aU2[l]
      r <- V2[, l] - X2_star_l_p2 %*% betav
      b <- t(r)%*%SigmaV_inv%*%r/2 + bU2[l]
      sig2[l] <- rinvgamma(1, a, b)
    }
    
    # Keep results
    U1_all[, , i] <- U1
    U2_all[, , i] <- U2
    V2_all[, , i] <- V2
    sig1_all[, i] <- sig1
    sig2_all[, i] <- sig2
    
    # Update betas
    Pu <- diag(1/c2, nrow = p, ncol = p)
    Pv <- diag(1/c2, nrow = p2, ncol = p2)
    Mu <- matrix(0, nrow = p, ncol = 1)
    Mv <- matrix(0, nrow = p2, ncol = 1)
    
    for (l in 1:nt) {
      Pu = Pu + t(X_star[,l,])%*% SigmaU_inv%*% X_star[,l,]/sig1[l]
      Pv = Pv + t(X2_star[,l,1:p2])%*% SigmaV_inv%*% X2_star[,l,1:p2]/sig2[l]
      Mu = Mu + t(X_star[,l,])%*% SigmaU_inv%*% U[, l]/sig1[l]
      Mv = Mv + t(X2_star[,l,1:p2])%*% SigmaV_inv%*% V2[, l]/sig2[l]
    }
    Pu_inv         <- solve(Pu)
    MMMu           <- Pu_inv %*% Mu
    betau          <- as.vector(t(chol(Pu_inv)) %*% rnorm(p) + MMMu)
    Pv_inv         <- solve(Pv)
    MMMv           <- Pv_inv %*% Mv
    betav          <- as.vector(t(chol(Pv_inv)) %*% rnorm(p2) + MMMv)
    betau_all[, i] <- betau
    betav_all[, i] <- betav
    
    # updates for theta1 and theta2
    aa <- 1:p_theta
    bb <- aa + p_theta
    for(j in 1:length(theta1)){
      att[j] <- att[j] +1
      can    <- theta1
      can[j] <- rnorm(1,theta1[j],MH[j])
      R      <- sum(digamma(sig1,exp(X_theta%*%can[aa]),   exp(X_theta%*%can[bb])))-
        sum(digamma(sig1,exp(X_theta%*%theta1[aa]),exp(X_theta%*%theta1[bb])))+
        dnorm(can[j],0,sqrt(c1),log=TRUE)-
        dnorm(theta1[j],0,sqrt(c1),log=TRUE)
      if(log(runif(1))<R){theta1<-can;acc[j] <- acc[j] +1}
      
      att[j] <- att[j] +1
      can    <- theta2
      can[j] <- rnorm(1,theta2[j],MH[j])
      R      <- sum(digamma(sig2,exp(X_theta%*%can[aa]),   exp(X_theta%*%can[bb])))-
        sum(digamma(sig2,exp(X_theta%*%theta2[aa]),exp(X_theta%*%theta2[bb])))+
        dnorm(can[j],0,sqrt(c1),log=TRUE)-
        dnorm(theta2[j],0,sqrt(c1),log=TRUE)
      if(log(runif(1))<R){theta2<-can;acc[j] <- acc[j]+1}
    }
    theta1_all[,i] <- theta1
    theta2_all[,i] <- theta2
    
    if(i<burn){for(j in 1:length(acc)){if(att[j]>25){
      if(acc[j]/att[j]<0.2){MH[j] <- MH[j]*0.8}
      if(acc[j]/att[j]>0.5){MH[j] <- MH[j]*1.2}
      acc[j] <- att[j] <- 0
    }}}
    
    
    # beta_alpha and Al
    if(updateA){
      sumV <- diag(p_alpha)/c3
      sumM    <- rep(0,p_alpha)
      sumM[1] <- 1
      sumM    <- as.vector(sumV%*%sumM)
      for (j in 1:nt) {
        Zj    <- matrix(U2[, j],n2,1) %*% matrix(X_alpha[j, ],1,p_alpha)
        Rj    <- Y2_star[, j] - V2[, j]
        sumV  <- sumV + t(Zj)%*%Zj/tau2_star
        sumM  <- sumM + Rj%*%Zj/tau2_star
      }
      VVVa   <- solve(sumV)
      MMMa   <- VVVa %*% t(sumM)
      beta_a <- as.vector(MMMa + t(chol(VVVa))%*%rnorm(p_alpha))
      A      <- as.vector(X_alpha %*% beta_a)
      beta_a_all[, i] <- beta_a
    }
    
    par(mfrow=c(3,2))
    for(j in 3:6){
      plot(betau_all[j,1:i],type="l")
    } 
  }
  
  
  out=list(betau=betau_all, betav=betav_all,
           tau=tau_all, sig1=sig1_all, sig2=sig2_all,
           Z1_mn = Z1_mn, Z1_var = Z1_var-Z1_mn^2,
           Z2_mn = Z2_mn, Z2_var = Z2_var-Z2_mn^2,
           match1_mn = match1_mn, match1_var = match1_var-match1_mn^2,
           match2_mn = match2_mn, match2_var = match2_var-match2_mn^2,
           Z1=Z1_all, Z2=Z2_all, 
           omega=omega,theta1=theta1_all, theta2=theta2_all,
           Y1 = Y1_all)
  return(out)}

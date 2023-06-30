# One Type LMC algorithm
library(geodist)
library(MCMCpack)
exp_corr=function(d,range)
{
  out=exp(-d/range)
  return(out)
}
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

LMC_fit=function(Y1, X, s1, 
                 rangeu=1.8,
                 c1=10, c2=100, c3=1, eps=1,
                 iters=3000, burn=1000)
{
  n1 = dim(Y1)[1]
  nt = dim(Y1)[2]
  p = dim(X)[3]
  
  # Required variable
  Y1_star <- matrix(0, nrow = n1, ncol = nt)
  
  # Take DFT of X
  X_star = array(NA, dim=c(n1, nt, p))
  for(i in 1:n1) {for (j in 1:p) {
    X_star[i, , j] = fft_real(X[i, , j])
  }}
  X1_star = X_star[1:n1,,]
  coords <- s1
  #! Scaled this
  if (nt%%2 == 0) {
    omega <- c(1:(nt/2+1), 2:(nt/2))
  } else {
    t = floor(nt/2)
    omega <- c(1:(t+1), 2:(t+1))
  }
  # omega <- c(1:(nt/2+1), )
  X_alpha <- cbind(1, scale(omega))
  p_alpha <- ncol(X_alpha)
  
  # Complete missing data
  m1     <- is.na(Y1)
  Y1[m1] <- mean(Y1, na.rm = T)
  
  # Generate Initial Values for U1, U2, V2, Al, and sigmas
  for(j in 1:n1){Y1_star[j,] <- fft_real(as.numeric(Y1[j,]))}
  Y_star <- Y1_star
  
  sig1  <- rep(1.0,nt)
  tau1  <- var(as.vector(Y1))/10
  
  Xlong <- NULL
  for(j in 1:p){Xlong <- cbind(Xlong,as.vector(X[1:n1,,j]))}
  betau <- lm(as.vector(Y1)~Xlong-1)$coef
  
  U <- Y_star
  for(j in 1:nt){
    U[,j] <- 0.5*(Y_star[,j]-X_star[,j,]%*%betau) + X_star[,j,]%*%betau
  }
  U1 <- U[1:n1,]
  U  <- U1
  
  
  #! Adding this part to the model
  X_theta <- cbind(1,scale(omega))
  p_theta <- ncol(X_theta)
  theta1  <- rep(0,2*p_theta)
  # theta2  <- rep(0,2*p_theta)
  
  Z1     <- matrix(0,n1,nt)
  
  # Book keeping
  betau_all  <- matrix(data = 0, nrow = p, ncol = iters)
  Y_all <- array(data = 0, dim = c(n1, nt, iters))
  U1_all     <- array(data = 0, dim = c(n1, nt, iters))
  sig1_all   <- matrix(0, nrow = nt, ncol = iters)
  theta1_all <- matrix(0, nrow = 2*p_theta, ncol = iters)
  # theta2_all <- matrix(0, nrow = 2*p_theta, ncol = iters)  
  Z1_all     <- array(data = 0, dim = c(n1, nt, iters))
  
  # Get distance matrix:
  dist_full <- as.matrix(geodist(coords, measure = 'geodesic' )/1000)
  # dist22 = dist_full[(n1+1):(n1+n2), (n1+1):(n1+n2)]
  SigmaU <- exp_corr(dist_full, rangeu)
  eigU   <- eigen(SigmaU)
  U_G    <- eigU$vectors
  U_D    <- eigU$values
  SigmaU_inv <- U_G %*% diag(1/U_D) %*% t(U_G)
  
  # Construct S1 and S2
  SigmaU11 <- SigmaU[1:n1, 1:n1]
  SigmaU12 <- SigmaU11
  SigmaU21 <- t(SigmaU12)
  # SigmaU22 <- SigmaU[(n1+1):(n1+n2), (n1+1):(n1+n2)]
  S1       <- SigmaU11
  # S2       <- SigmaU22 - SigmaU21 %*% solve(SigmaU11) %*% SigmaU12
  A12      <- SigmaU11
  A21      <- SigmaU11
  # Eigen decomposition of S1 and S2
  eigS1  <- eigen(S1)
  # eigS2  <- eigen(S2)
  S1_G   <- eigS1$vectors
  S1_D   <- eigS1$values
  #S2_G   <- eigS2$vectors
  #S2_D   <- eigS2$values
  S1_inv <- S1_G %*% diag(1/S1_D) %*% t(S1_G)
  # S2_inv <- S2_G %*% diag(1/S2_D) %*% t(S2_G)
  
  
  acc <- att <- MH <- rep(0.25,2*p_theta)
  
  for(i in 1:iters){
    print(round(100*i/iters))
    
    ##############################################:
    ####     Transform to spatial land       #####:
    ##############################################:
    
    for(j in 1:n1){Z1[j,] <- fft_real(U1[j,],inverse=TRUE)}
    # for(j in 1:n2){Z2[j,] <- fft_real(A*U2[j,]+V2[j,],inverse=TRUE)}
    Z1_all[,,i] <- Z1
    # Z2_all[,,i] <- Z2
    
    ##############################################:
    ####  IMPUTE MISSING DATA (real space)   #####:
    ##############################################:
    
    Y1[m1] <- rnorm(sum(m1),0,sqrt(tau1)) + Z1[m1]
    # Y2[m2] <- rnorm(sum(m2),0,sqrt(tau2)) + Z2[m2]
    Y_all[,,i] = Y1
    
    #! Added updates for tau
    tau1 <- 1/rgamma(1,n1*nt/2+eps,sum((Y1-Z1)^2)/2+eps)
    # tau2 <- 1/rgamma(1,n2*nt/2+eps,sum((Y2-Z2)^2)/2+eps)
    
    #! Account for changing scale in spectral domain
    tau1_star <- 0.5*nt*tau1
    # tau2_star <- 0.5*nt*tau2
    
    ##############################################:
    ####     Transform to spectral land      #####:
    ##############################################:
    
    for(j in 1:n1){Y1_star[j,] <- fft_real(as.numeric(Y1[j,]))}
    # for(j in 1:n2){Y2_star[j,] <- fft_real(as.numeric(Y2[j,]))}
    Y_star <- Y1_star
    
    ##############################################:
    ####           Update U and V            #####:
    ##############################################:
    
    for (l in 1:nt) {
      # Matrix notation
      X_star_l  <- X_star[,l,]
      X1_star_l <- X1_star[,l,]
      # X2_star_l <- X2_star[,l,]
      Y_star_l  <- Y_star[,l]
      Y1_star_l <- Y1_star[,l]
      # Y2_star_l <- Y2_star[,l]
      
      # Sample U1
      #! Make these updates involve U2
      newDiag1    <- (1/tau1_star) + (1/sig1[l]) * (1/S1_D)
      meanU1_p1   <- (1/tau1_star) * Y1_star_l
      mu1         <- X1_star_l %*% betau
      meanU1_p2   <- (1/sig1[l]) * (S1_inv %*% mu1)
      meanU1      <- t(S1_G) %*% (meanU1_p1 + meanU1_p2)
      U1[, l] <- S1_G%*%(meanU1/newDiag1 + rnorm(n1, 0, 1/sqrt(newDiag1)))
      
      U[, l]      <- U1[, l]
      #! Added this model for the prior variances
      aU1 <- exp(X_theta%*%theta1[1:p_theta])
      bU1 <- exp(X_theta%*%theta1[1:p_theta + p_theta])
      # aU2 <- exp(X_theta%*%theta2[1:p_theta])
      # bU2 <- exp(X_theta%*%theta2[1:p_theta + p_theta])

      
      
      # Sample sig1
      a <- n1/2 + aU1[l]
      r <- U[, l] - X_star_l %*% betau
      b <- t(r)%*%SigmaU_inv%*%r/2 + bU1[l]
      sig1[l] <- rinvgamma(1, a, b)
    }
    
    # Keep results
    U1_all[, , i] <- U1
    sig1_all[, i] <- sig1
    
    # Update betas
    Pu <- diag(1/c2, nrow = p, ncol = p)
    Mu <- matrix(0, nrow = p, ncol = 1)
    for (l in 1:nt) {
      Pu = Pu + t(X_star[,l,])%*% SigmaU_inv%*% X_star[,l,]/sig1[l]
      Mu = Mu + t(X_star[,l,])%*% SigmaU_inv%*% U[, l]/sig1[l]
    }
    Pu_inv             <- solve(Pu)
    MMMu               <- Pu_inv %*% Mu
    betau          <- as.vector(t(chol(Pu_inv)) %*% rnorm(p)) + MMMu
    betau_all[, i] <- betau
    
    #! added updates for theta1 and theta2
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
      
      # att[j] <- att[j] +1
      # can    <- theta2
      # can[j] <- rnorm(1,theta2[j],MH[j])
      # R      <- sum(digamma(sig2,exp(X_theta%*%can[aa]),   exp(X_theta%*%can[bb])))-
      #   sum(digamma(sig2,exp(X_theta%*%theta2[aa]),exp(X_theta%*%theta2[bb])))+
      #   dnorm(can[j],0,sqrt(c1),log=TRUE)-
      #   dnorm(theta2[j],0,sqrt(c1),log=TRUE)
      # if(log(runif(1))<R){theta2<-can;acc[j] <- acc[j]+1}
    }
    theta1_all[,i] <- theta1
    # theta2_all[,i] <- theta2
    
    if(i<burn){for(j in 1:length(acc)){if(att[j]>25){
      if(acc[j]/att[j]<0.2){MH[j] <- MH[j]*0.8}
      if(acc[j]/att[j]>0.5){MH[j] <- MH[j]*1.2}
      acc[j] <- att[j] <- 0
    }}}
    
    # beta_alpha and Al
    # if(updateA){
    #   sumV <- diag(p_alpha)/c3
    #   sumM    <- rep(0,p_alpha)
    #   sumM[1] <- 1
    #   sumM    <- as.vector(sumV%*%sumM)
    #   for (j in 1:nt) {
    #     Zj    <- matrix(U2[, j],n2,1) %*% matrix(X_alpha[j, ],1,p_alpha)
    #     Rj    <- Y2_star[, j] - V2[, j]
    #     sumV  <- sumV + t(Zj)%*%Zj/tau2_star
    #     sumM  <- sumM + Rj%*%Zj/tau2_star
    #   }
    #   VVVa   <- solve(sumV)
    #   MMMa   <- VVVa %*% t(sumM)
    #   beta_a <- as.vector(MMMa + t(chol(VVVa))%*%rnorm(p_alpha))
    #   A      <- as.vector(X_alpha %*% beta_a)
    #   beta_a_all[, i] <- beta_a
    #   print(beta_a)
    # }
  }
  
  
  out=list(betau = betau_all, Y1 = Y_all)
  #names(out)=c('rangeU','rangeV','sigmaU','sigmaV','tau1','tau2','A','Y1.m','Y2.m','U1','U2','V2','aru','arv','Y1.p')
  return(out)
}

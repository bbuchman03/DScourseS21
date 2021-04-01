# 4
set.seed(100)
N <- 100000
K <- 10
sigma <- 0.5 #standard deviation or variance
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
X[,1] <- 1 # first column of X should be all ones
eps <- rnorm(N,mean=0,sd=sigma)
betaTrue <- as.vector(runif(K))
Y <- X%*%betaTrue + eps

# 5
library(tidyverse)
estimates <- lm(Y~X -1)
print(summary(estimates))
results <- tibble(truth = betaTrue, estimates = coef(estimates))

# 6
alpha <- 0.0000003
maxiter <- 500000
objfun <- function(b,Y,X) {
  return ((Y - X%*%b)^2)
}
gradient <- function(b,Y,X) {
  return ( as.vector(-2%*%b*(Y-X%*%b)) )
}
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
Y <- X%*%betaTrue + eps
beta <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
set.seed(100)
beta.All <- matrix("numeric",length(beta),maxiter)
iter  <- 1
beta0 <- 0*beta
while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}
print(iter)

# 7
# BFGS
# Our objective function
library(nloptr)
objfun <- function(b,Y,X) {
  return (sum((Y-X%*%beta)^2))
  # equivalently, if we want to use matrix algebra:
  # return ( crossprod(y-X%*%beta) )
}
# Gradient of our objective function
gradient <- function(b,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%b)) )
}
# read in the data
Y <- X%*%betaTrue + eps
X <- matrix(rnorm(N*K,mean=0,sd=sigma),N,K)
# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients
# Algorithm parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)
# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

# 8
gradient <- function (b,Y,X) {
  grad <- as.vector ( rep (0, length (b)))
  beta <- theta [1:( length (b) -1)]
  sig <- theta [ length (b)]
  grad [1:( length (b) -1)] <- -t(X)%*%(Y - X%*%beta )/(sig ^2)
  grad[ length (theta )] <- dim (X)[1] /sig - crossprod (Y-X%*%beta)/(sig
                                                                       ^3)
  return ( grad )
}

# 9
lm(Y ~ X -1)

##
## Manual Implementation;
##

manual_logistic_regression = function(X, y, threshold = 1e-10, max_iter = 100)
  # A function to find logistic regression coeffiecients 
  # Takes three inputs: 
{
  # A function to return p, given X and beta
  # We'll need this function in the iterative section
  calc_p = function(X, beta)
  {
    beta = as.vector(beta)
    return(exp(X %*% beta) / (1 + exp(X %*% beta)))
  }  
  
  #### setup bit ####
  
  # initial guess for beta
  beta = rep(0, ncol(X))
  
  # initial value bigger than threshold so that we can enter our while loop 
  diff = 10000 
  
  # counter to ensure we're not stuck in an infinite loop
  iter_count = 0
  
  #### iterative bit ####
  while(diff > threshold ) # tests for convergence
  {
    # calculate probabilities using current estimate of beta
    p = as.vector(calc_p(X, beta))
    
    # calculate matrix of weights W
    W =  diag(p * (1 - p)) 
    
    # calculate the change in beta
    beta_change = solve(t(X) %*% W %*% X) %*% t(X) %*% (y - p)
    
    # update beta
    beta = beta + beta_change
    
    # calculate how much we changed beta by in this iteration 
    # if this is less than threshold, we'll break the while loop 
    diff = sum(beta_change ^ 2)
    
    # see if we've hit the maximum number of iterations
    iter_count = iter_count + 1
    if(iter_count > max_iter) {
      stop("This isn't converging, mate.")
    }
  }
  # make it pretty 
  coef = c("(Intercept)" = beta[1], x1 = beta[2], x2 = beta[3], x3 = beta[4])
  return(coef)
};

##
## Simulation;
##

set.seed(2016)
# simulate data 
# independent variables
x1 = rnorm(30, 3, 2) + 0.1 * c(1:30)
x2 = rbinom(30, 1, 0.3)
x3 = rpois(n = 30, lambda = 4)
x3[16:30] = x3[16:30] - rpois(n = 15, lambda = 2)

# dependent variable 
y = c(rbinom(5, 1, 0.1), rbinom(10, 1, 0.25), rbinom(10, 1, 0.75), rbinom(5, 1, 0.9))

x0 = rep(1, 30) # bias
X = cbind(x0, x1, x2, x3)

# using R package 
M1 = glm(y ~ x1 + x2 + x3, family = "binomial")
M1$coefficients
# (Intercept)         x1          x2          x3 
# -1.3512086   0.3191309   0.2033449  -0.0832102 

#our version
manual_logistic_regression(X, y)
# (Intercept)         x1          x2          x3 
# -1.3512086   0.3191309   0.2033449  -0.0832102 

# they're the same! Amazing!
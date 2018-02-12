
#Simple Comparison
# 1/M * sum(indicator(y>x))
#Above ran 2000 times
mc_approx <- function(x){
holding <- sapply(1:2000, function(y){sum(rnorm(x)<rnorm(x))/x})
  return(holding)
}


mc_approx_up <- function(x){
  holding <- matrix(ncol=11, nrow = 2000)
  holding <- sapply(1:2000, function(y){sum(rnorm(x)<rnorm(x))/x})
  return(holding)
}

#Double Comparison
#1/M*sum_i(1/M*sum_j(y_i>x_j))
#Above ran 2000 times
#simplification of 1/M^2 * sum(expanded(y) > x)
#x cycled


mc_approx_double_up <- function(x){
  holding <- matrix(ncol = 11, nrow = 2000)
  fake_function <- function(y){sum(kronecker(rnorm(x), rep(1,x)) <rnorm(x))/x^2}
  holding <- sapply(1:2000, fake_function)
  return(holding)
}

cl <- makeCluster(2)
mc_approx_double_par <- function(x){
  cl <- makeCluster(2)
  holding <- matrix(ncol = 11, nrow = 2000)
  fake_function <- function(y){sum(kronecker(rnorm(x), rep(1,x)) <rnorm(x))/x^2}
  holding <- parSapply(cl, 1:2000, fake_function)
  return(holding)
}



mm <- c(100,200,300,400,500,600,700,800,900,1000,2000)
ka <- sapply(mm,  mc_approx_up)
ki <- sapply(mm, mc_approx_double_up)
double_mc <- ki
mc <- ka

#sample variance vs believed variance for single mc
sample_est <- apply(mc, 2, var)
j <- apply(mc, 2, mean)
binomial_est <- j*(1-j)/mm
sample_est
binomial_est
binomial_est/sample_est




#sample variance vs believed variance for double mc
sample_est_double <- apply(double_mc, 2, var)
j_double <- apply(double_mc, 2, mean)
binomial_est_double <- j_double*(1-j_double)/mm
sample_est_double
binomial_est_double
binomial_est_double/sample_est_double




#Final Solution
sample_ratio <- sample_est/sample_est_double
bernoulli_ratio <- bernoulli_est/bernoulli_est_double


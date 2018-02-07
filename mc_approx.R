
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

ki <- sapply(mm, mc_approx_double_old_up)

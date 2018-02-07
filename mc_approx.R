

mc_approx <- function(x){
  holding <- NULL
for(i in 1:2000){
j <- rnorm(x)
k <- rnorm(x)
holding[i] <- sum(j<k)/x
}
  return(holding)
}
mean(holding)


mc_approx_double <- function(x){
  holding <- NULL
for(i in 1:2000){
j <- rnorm(x)
jj <- kronecker(j, rep(1, x))
k <- rnorm(x)
holding[i] <- sum(jj<k)/(x^2)
}
  return(holding)
}


mm <- c(100,200,300,400,500,600,700,800,900,1000,2000)
small_loop <- 


jack <- c(100,200)
ka <- sapply(jack,  mc_approx)

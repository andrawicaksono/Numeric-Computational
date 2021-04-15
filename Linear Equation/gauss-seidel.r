a <- matrix(c(4,4,-2,-1,-8,1,1,1,5), nrow = 3)
n <- nrow(a)
b <- c(7,-21,15)
p0 <- c(1,2,2)
eps <- 1e-8
maxit <- 100
x <- p0
conv <- TRUE

for (i in 1:n){
  c <- 0
  for (j in 1:n){
    if(i != j){
      c <- abs(c) + abs(a[i,j])
    }
  }
  if(abs(a[i,i]) < abs(c)){
    conv <- FALSE
    break
  }
}

for(k in 1:maxit){
  if(conv == FALSE)
    stop("Divergent")
  if(k == maxit)
    warning("Maximum Iteration is reached")
  print(paste("Iteration",k))
  for (i in 1:n) {
    s <- 0
    for(j in 1:n){
      if (i != j){
        s <- s + a[i,j]*x[j]
      }
    }
    temp <- (b[i]-s)/a[i,i]
    abserr <- abs((temp - x[i]))
    x[i] <- temp
    print(paste("x[", i, "] = ", x[i]))
  }

  if (abserr < eps)
    break
}

library(Deriv)
x0 <- 0.5
f <- function(x){x*exp(x) - 2}
fd <- Deriv(f)
eps <- 1e-5
maxit <- 3

for (i in 1:maxit) {
  x <- x0 - (f(x0)/fd(x0))
  if (f(x0) == 0){
    print(paste("akar adalah", x0))
    break
  }
  if (f(x) == 0){
    print(paste("akar adalah", x))
    break
  }
  print(paste("iterasi ke", i))
  print(paste("x0   f(x)   xn    |xn-x0|"))
  print(paste(x0, f(x), x, abs(x-x0)))
  if (abs(x-x0) <= eps){
    print(paste("akar adalah", x))
    break
  }
  x0 <- x
}
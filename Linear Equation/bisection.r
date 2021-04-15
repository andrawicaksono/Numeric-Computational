options(digits = 4)
a <- -2
b <- 5
f <- function(x){(5^x + 2*x)/(exp(x) - 1)}
eps <- 1e-2
maxit <- 100

for (i in 1:maxit) {
  c <- (a+b)/2
  if (f(c) == 0){
    print(paste("akar adalah", c))
    break
  }
  print(paste("iterasi ke", i))
  print(paste("a   b   c   f(a)   f(c)    |b-c|"))
  print(paste(a, b, c, f(a), f(c), abs(b-a)))
  if (f(a)*f(c) < 0)
    b <- c
  else
    a <- c
  if (abs(b-a) <= eps){
    print(paste("akar adalah", c))
    break
  }
}

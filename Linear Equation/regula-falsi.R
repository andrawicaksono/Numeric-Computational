options(digits = 4)
a <- 0
b <- 1
f <- function(x){x*exp(x) - 2}
f(0.851)
eps <- 1e-2
maxit <- 3

for (i in 1:maxit) {
  c <- b-((f(b)*(b-a))/(f(b)-f(a)))
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

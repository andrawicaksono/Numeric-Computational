a <- matrix(NULL, nrow = NULL)
n <- nrow(a)

for (p in 1:(n-1)){
  for(r in (p+1):n){
    m <- a[r,p] / a[p,p]
    a[r,p] <- 0
    
    for(c in (p+1):(n+1)){
      a[r,c] <- a[r,c] - m*a[p,c]
    }
  }
}

t <- a[n,n+1] / a[n,n]
z <- (a[n-1, n+1] - a[n-1, n]*t) / a[n-1,n-1]
y <- (a[n-2, n+1] - a[n-2, n]*t - a[n-2,n-1]*z) / a[n-2,n-2]
x <- (a[n-3, n+1] - a[n-3, n]*t - a[n-3,n-1]*z - a[n-3,n-2]*y) / a[n-3,n-3]

print(x)
print(y)
print(z)
print(t)
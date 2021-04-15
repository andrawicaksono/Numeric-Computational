a <- matrix(c(1.464,2.283,2.283,5), nrow=2)
b <- matrix(c(0.151,0.315), nrow=2)
ab <- cbind(a,b)
n <- nrow(ab)

for (p in 1:(n-1)){
  m1 <- ab[p,p]
  for (c in p:(n+1)){
    ab[p,c] <- ab[p,c]/m1
  }
  if(p>1){
    m2 <- ab[p-1,p]
    for(c in p:(n+1)){
      ab[p-1,c] <- ab[p-1,c] - m2*ab[p,c]
    }
  }
  for(r in (p+1):n){
    m3 <- ab[r,p]
    for(c in p:(n+1)){
      ab[r,c] <- ab[r,c] - m3*ab[p,c]
    }
  }
}

for (p in n){
  m1 <- ab[p,p]
  for (c in p:(n+1)){
    ab[p,c] <- ab[p,c]/m1
  }
  for(r in 1:(n-1)){
    m2 <- ab[r,p]
    
    for(c in p:(n+1)){
      ab[r,c] <- ab[r,c] - m2*ab[p,c]
    }
  }
}

x <- ab[, n+1]
x[1]
x[2]
x[3]

a <- matrix(c(NULL), nrow = NULL)
i <- matrix(c(1,0,0,0,1,0,0,0,1), nrow = 3)
b <- matrix(c(NULL), nrow=NULL)
ai <- cbind(a,i)
n <- nrow(a)

for (p in 1:(n-1)){
  m1 <- ai[p,p]
  for (c in p:(n+n)){
    ai[p,c] <- ai[p,c]/m1
  }
  if(p>1){
    m2 <- ai[p-1,p]
    for(c in p:(n+n)){
      ai[p-1,c] <- ai[p-1,c] - m2*ai[p,c]
    }
  }
  for(r in (p+1):n){
    m3 <- ai[r,p]
    for(c in p:(n+n)){
      ai[r,c] <- ai[r,c] - m3*ai[p,c]
    }
  }
}

for (p in n){
  m1 <- ai[p,p]
  for (c in p:(n+n)){
    ai[p,c] <- ai[p,c]/m1
  }
  for(r in 1:(n-1)){
    m2 <- ai[r,p]
    
    for(c in p:(n+n)){
      ai[r,c] <- ai[r,c] - m2*ai[p,c]
    }
  }
}

a_1 <- ai[,(n+1):(n+n)]
x <- a_1 %*% b
x[1]
x[2]
x[3]
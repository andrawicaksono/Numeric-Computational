# Inisialisasi nilai tebakan awal, nilai epsilon
a <- matrix(c(4,4,-2,-1,-8,1,1,1,5), nrow = 3)
n <- nrow(a)
b <- c(7,-21,15)
p0 <- c(1,2,2)
eps <- 1e-8
maxit <- 100
x <- p0
conv <- TRUE

# Cek kondisi kekonvergenan solusi dengan syarat cukup dominan secara diagonal
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

# Penghitungan iterasi
for(k in 1:maxit){
  if(conv == FALSE)
    stop("Solusi Divergen")
  if(k == maxit)
    warning("Iterasi sudah maksimal")
  print(paste("iterasi ke",k))
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

  # Cek kondisi berhenti untuk proses iterasi
  if (abserr < eps)
    break
}
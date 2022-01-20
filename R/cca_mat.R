cca_mat = function(sigma,p){
  #does canonical correlation analysis, starting from a covariance matrix
  
  #sigma is the covariance matrix for p+q variables
  #p is the number of variables in the first set
  
  #the expm library is required for the sqrtm function to calculate sq root matrices
  library(expm)
  #partition the matrix sigma
  m = ncol(sigma)
  s11 = sigma[1:p,1:p]
  s12 = sigma[1:p,(p+1):m]
  s22 = sigma[(p+1):m,(p+1):m]
  s21 = sigma[(p+1):m,1:p]
  # calculate the matrix x and find its eigenvalues and eigenvectors
  x = solve(sqrtm(s11)) %*% s12 %*% solve(s22) %*% s21 %*% solve(sqrtm(s11))
  rho =eigen(x)$values
  e = eigen(x)$vectors
  #now calculate y and find its eigenvectors
  y = solve(sqrtm(s22)) %*% s21 %*% solve(s11) %*% s12 %*% solve(sqrtm(s22))
  f = eigen(y)$vectors
  #calculate the matrices of coefficients a and b
  a = apply(e,2, function(col){t(col) %*% solve(sqrtm(s11)) })
  b = apply(f,2, function(col){t(col) %*% solve(sqrtm(s22)) })
  return(list(rho=sqrt(rho),
              a=a,
              b=b))
}
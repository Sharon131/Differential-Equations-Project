# Iloœæ elementów
n <- 3
# D³ugoœæ jednego podprzedzia³u
l <- 2/(n-1)
# Iloœæ linii prostych:
m <- 2*(n-1)

derivative <- function(f, x) {
  h <- 0.01
  return((f(x+h)-f(x-h))/(2*h))
}

integral <- function(f, x) {
  
  return(0)
}

x1 <- seq(0,2-l,l)
x2 <- seq(1,2,l)


a1 <- 1/(x2-x1)
b1 <- x1/(x1-x2)

a2 <- 1/(x1-x2)
b2 <- x2/(x2-x1)

a <- c(a2,a1)
b <- c(b2,b1)


Ei <- function(x, i) {
  if (i == 1) {
    result <- a[1]*x + b[1]
    result[result<0] <- 0
    return(result)
  } 
  else if (i == n) {
    result <- a[m]*x+b[m]
    result[result<0] <- 0
    return(result)
  } 
  else{
    D_lower <- 2*(i-2)/(n-1)
    D_upper <- 2*i/(n-1)
    D_middle <- 2*(i-1)/(n-1)
    
    values <- a[i+n-2]*x[x>D_lower & x<D_middle] + b[i+n-2]
    val <- a[i]*x[x>D_middle & x<D_upper] + b[i]
    values <- c(values, val)
  }
  
  result <- integer(length(x[x<D_lower]))
  result <- c(result,values)
  
  result <- c(result, integer(length(x[x>D_upper])))
  
  return(result)
}

# dodaæ obliczanie macierzy A z ca³ek (f. B(ei, ei))
# matrix_A <- matrix(nrow=n, ncol=n)
matrix_A <- matrix(nrow = 3, ncol = 3)

matrix_A[,1] <- c(0, -1, 0)
matrix_A[,2] <- c(-1, 3, -2)
matrix_A[,3] <- c(0, -2, 2)

#dodaæ obliczanie macierzy B z f. L(ei)
matrix_B  <- c(-20,0,0)

e1 <- function(x) {
  result <- -x+1
  result[result<0] <- 0
  result[result>1] <- 0
  return(result)
}

e2 <- function(x) {
  result <- x
  result[result>=1] <- (-result[result>=1])+2
  return(result)
}

e3 <- function(x) {
  result <- x-1
  result[result<0] <- 0
  return(result)
}

L <- function(v) {
  return(-20*v(0))
}

B <- function(u,v) {
  result <- -u(0)*v(0) #ca³ka z k(x)*u'(x)*v'(x)
}

k <- function(x) {
  if (x <= 1 && x >=0) {
    return(1)
  } else if (x <= 2) {
    return(2)
  } else {
    return(0)
  }
}

x <- seq(0,2,0.1)

print("My matrix:")
print(matrix_A)

coeffs <- solve(matrix_A, matrix_B)
print("Solution:")
print(coeffs)

x <- seq(0,2,0.1)
y <- coeffs[1]*e1(x)+coeffs[2]*e2(x)+coeffs[3]*e3(x)

plot(x, y, main="Function u(x)", ylab="u(x)", type="o", col="blue")

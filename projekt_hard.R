# Iloœæ elementów
n <- 3
# D³ugoœæ jednego podprzedzia³u
l <- 2/(n-1)
# Iloœæ linii prostych:
m <- 2*(n-1)

x <- seq(0,2,0.1)

derivative2 <- function(f, x) {
  h <- 0.01
  return((f(x+h)-f(x-h))/(2*h))
}

derivative <- function(Ei, x, i) {
  h <- function(x, i){
    if (i == 1) {
      D_lower <- 0
      D_upper <- 2*i/(n-1)
      result <- integer(length(x[x>=D_lower & x <=D_upper]))+a[1]
      result <- c(result, integer(length(x[x>D_upper])))
      return(result)
    } 
    else if (i == n) {
      D_lower <- 2*(i-2)/(n-1)
      D_upper <- 2
      result <- integer(length(x[x<D_lower]))
      result <- c(result,integer(length(x[x>=D_lower & x <=D_upper]))+a[m])
      
      return(result)
    } 
    else{
      D_lower <- 2*(i-2)/(n-1)
      D_upper <- 2*i/(n-1)
      D_middle <- 2*(i-1)/(n-1)
      
      values <- a[i+n-2]+integer(length(x[x>=D_lower & x<=D_middle]))
      val <- a[i]+integer(length(x[x>D_middle & x<=D_upper]))
      values <- c(values, val)
    }
    
    result <- integer(length(x[x<D_lower]))
    result <- c(result,values)
    
    result <- c(result, integer(length(x[x>D_upper])))
    
    return(result)  
  }
  return(h)
}

integral <- function(Ei, x, i) {
  x1 <- -1/(sqrt(3))
  x2 <- 1/(sqrt(3))
  
  integr <- Ei((x1+1)*2/(n-1), i)+Ei((x2+1)*2/(n-1), i)
  return(integr*(n-1)/2)
}

integral2 <- function(f, x, D_middle) {
  x1 <- -1/(sqrt(3))
  x2 <- 1/(sqrt(3))
  
  integr <- f((x1+1)*2/(n-1))+f((x2+1)*2/(n-1))
  return(integr*(n-1)/2)
}

x1 <- seq(0,2-l,l)
x2 <- seq(l,2,l)


a1 <- 1/(x2-x1)
b1 <- x1/(x1-x2)

a2 <- 1/(x1-x2)
b2 <- x2/(x2-x1)

a <- c(a2,a1)
b <- c(b2,b1)


Ei <- function(x, i) {
  if (i == 1) {
    result <- a[1]*x + b[1]
    result[result<0 & result>1] <- 0
    return(result)
  } 
  else if (i == n) {
    result <- a[m]*x+b[m]
    result[result<0 & result>1] <- 0
    return(result)
  } 
  else{
    D_lower <- 2*(i-2)/(n-1)
    D_upper <- 2*i/(n-1)
    D_middle <- 2*(i-1)/(n-1)
    
    values <- a[i+n-2]*x[x>=D_lower & x<=D_middle] + b[i+n-2]
    val <- a[i]*x[x>D_middle & x<=D_upper] + b[i]
    values <- c(values, val)
  }
  
  result <- integer(length(x[x<D_lower]))
  result <- c(result,values)
  
  result <- c(result, integer(length(x[x>D_upper])))
  
  return(result)
}

L <- function(Ei, i) {
  return(-20*Ei(0, i))
}

B <- function(u,v, i_u, i_v) {
  u_ <- derivative(u,x,i_u)
  v_ <- derivative(v,x,i_v)
  
  t <- function(x) {
    return(k(x)*u_(x, i_u)*v_(x,i_v))
  }
  
  D_middle = (i_u+i_v-2)/(n-1)
  result <- -u(0, i_u)*v(0, i_v)+integral2(t, x, D_middle) #ca³ka z k(x)*u'(x)*v'(x)
  return(result)
}

k <- function(x) {
  result <- integer(length(x[x<=1 & x>=0]))+1
  result <- c(result, integer(length(x[x>1 & x<=2]))+2)
  
  return(result)
}

L_matrix <- L(Ei, 1)

for (i in 2:(n-1)) {
  L_matrix <- c(L_matrix, L(Ei, i))
}

# Tworzenie macierzy B
B_matrix <- matrix(nrow=n-1, ncol=n-1)

for (i in 1:(n-1)){
  buffer <- B(Ei, Ei, i, 1)
  for (j in 2:(n-1)){
    #B_matrix[i][j] <- B(Ei, Ei, i, j)
    buffer <- c(buffer, B(Ei, Ei, i, j))
  }
  print(buffer)
  B_matrix[,i] <- buffer
}

print("L_matrix:")
print(L_matrix)

print("B_matrix:")
print(B_matrix)

coeffs <- solve(B_matrix, L_matrix)
print("Solution:")
print(coeffs)

x <- seq(0,2,0.1)
y <- coeffs[1]*Ei(x, 1)

for (i in 2:(n-1)) {
  y <- y + coeffs[i]*Ei(x, i)
}

plot(x, y, main="Function u(x)", ylab="u(x)", type="o", col="blue")

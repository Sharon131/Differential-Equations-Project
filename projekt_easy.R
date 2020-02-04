
matrix_A <- matrix(nrow = 3, ncol = 3)

matrix_A[,1] <- c(0, -1, 0)
matrix_A[,2] <- c(-1, 3, -2)
matrix_A[,3] <- c(0, -2, 2)

matrix_B  <- c(-20,0,0)

e1 <- function(x) {
  result <- -x+1
  result[result<0] <- 0
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

print("My matrix:")
print(matrix_A)

coeffs <- solve(matrix_A, matrix_B)
print("Solution:")
print(coeffs)

funcs <- matrix(nrow = 21, ncol = 3)
funcs[,1] <- c(e1(x))
funcs[,2] <- c(e2(x))
funcs[,3] <- c(e3(x))

x <- seq(0,2,0.1)
y <- coeffs[1]*e1(x)+coeffs[2]*e2(x)+coeffs[3]*e3(x)

plot(x, y, main="Function u(x)", ylab="u(x)", type="o", col="blue")
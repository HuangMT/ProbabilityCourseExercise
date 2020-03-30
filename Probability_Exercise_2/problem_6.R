# The R code of 6th problem: Multi-things Monty Hall Problem

N <- 10
avg <- mean((1:N)^2)
total <- sum((1:N)^2)

E <- matrix(avg,N,N) # a 10-by-10 matrix and each element was assigned avg
valid <- matrix(F,N-1,N-1)

for (s in 1:(N-1)) {
  for (t in 1:(N-s) ) {
    E[s,t] <- s*avg
    valid[s,t-1] <- T
  }
}

persp(1:10, 1:10, E, theta = -60, phi = 40,
      expand = 0.5, col = 200-170*t(valid), xlab = "S", ylab = "T", zlab = "Expectation", xlim = c(1,10), ylim = c(1,10),
      zlim = c(0,500))



# When S = 2, T = 3, we simulate the expectation
s <- 2
t <- 3

N_sim <- 10000
obtain <- numeric(N_sim)

for (n in 1:N_sim) {
  chosen <- sample(1:N, s)
  tmp <- 1:N
  tmp <- tmp[-chosen] # delete the chosen doors
  open <- sample(tmp, t)
  if (mean(open^2) > avg) {
    obtain[n] <- sum(chosen^2)
  } else {
    tmp <- 1:N
    tmp <- tmp[-c(chosen,open)] # delete the chosen doors
    chosen <- sample(tmp, s)
    obtain[n] <- sum(chosen^2)
  }
}


print(E[s,t])
print(sum(obtain)/sum(obtain>1))
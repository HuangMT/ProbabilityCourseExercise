N_samples <- 100000

x <- runif(N_samples, min=-1, max=2)
y = (cos(x))^2*sqrt(x^3+1)*3
Egx <- mean(y)

print(Egx)

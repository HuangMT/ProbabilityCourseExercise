N_times <- 10000
N_each_time <- 100
p <- 0.2

x <- (rbinom(N_times, N_each_time, p)-p*N_each_time)/sqrt(N_each_time*(p*(1-p)))
hist(x)




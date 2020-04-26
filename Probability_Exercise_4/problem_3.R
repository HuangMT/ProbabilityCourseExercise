

x <- seq(0.3,5,by=0.1)
erfcx <- erfc(x)

N <- 2
theta <- seq(0,N)*(pi/2/N)
U2x <- 1/N*exp(-x^2/(sin(theta[2]))^2)
for (n in 2:N) {
  U2x <- U2x + 1/N*exp(-x^2/(sin(theta[n+1]))^2)
}

N <- 3
theta <- seq(0,N)*(pi/2/N)
U3x <- 1/N*exp(-x^2/(sin(theta[2]))^2)
for (n in 2:N) {
  U3x <- U3x + 1/N*exp(-x^2/(sin(theta[n+1]))^2)
}

N <- 5
theta <- seq(0,N)*(pi/2/N)
U5x <- 1/N*exp(-x^2/(sin(theta[2]))^2)
for (n in 2:N) {
  U5x <- U5x + 1/N*exp(-x^2/(sin(theta[n+1]))^2)
}

plot(x, erfcx, type="o", col="blue", pch=".", lty=1, ylim=c(0,0.5) )
lines(x, U2x, col="red", lty=1)
lines(x, U3x, col="dark red", lty=1)
lines(x, U5x, col="green", lty=1)

legend(4,0.45,legend=c("erfc(x)","U2(x)","U3(x)","U5(x)"), col=c("blue","red","dark red",'green'),
       pch=c(".",".",".","."),lty=c(1,2,3,4), ncol=1)

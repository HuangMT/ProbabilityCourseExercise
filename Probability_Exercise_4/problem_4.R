
x <- seq(-5,5,by=0.01)
Qx <- 1/2*erfc(x/sqrt(2))

T <- 20
omega <- 2*pi/T

Ax <- x*0+1/2
for ( k in 1:20) {
  Ax <- Ax - 2/pi*(exp(-(2*k-1)^2*omega^2/2)*sin((2*k-1)*omega*x))/(2*k-1)
}

plot(x, Qx, type="o", col="blue", pch=".", lty=1, ylim=c(0,1) )
lines(x, Ax, col="red", lty=1)

legend(2,1,legend=c("Q(x)","ApproxQ(x)"), col=c("blue","red"),
       pch=c(".","."),lty=c(1,2), ncol=1)

print(max(Qx-Ax))

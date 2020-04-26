library("pracma")

x <- seq(0.3,5,by=0.1)
Qx <- 1/2*erfc(x/sqrt(2))
Ux <- exp(-x^2/2)/(sqrt(2*pi)*x)
Lx <- Ux * (1-1/x^2)

plot(x, Qx, type="o", col="blue", pch=".", lty=1, ylim=c(0,0.5) )
lines(x, Ux, col="red", lty=1)
lines(x, Lx, col="dark red", lty=1)

legend(4,0.45,legend=c("Q(x)","U(x)","L(x)"), col=c("blue","red","dark red"),
       pch=c(".",".","."),lty=c(1,2,3), ncol=1)







  
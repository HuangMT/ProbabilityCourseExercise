# The R code of 4th problem: Lying Monty Hall Problem

#   Actually the index of doors can be arbitrarily changed, the only function is to distinguish the different
# doors. So in this problem we identify the door chosen by compotitor as "0", and the door chosen by Monty as "1".
#   And we need to consider two case, i.e. the Monty Hall said there is a car/goat behind door "1".
#   We repeat the simulation for different p, from 0 to 1 with step 0.1 .

p_of_lie <- seq(fr=0,to=1,by=0.1)
N <- length(p_of_lie)

p_car_of_choose_0_when_NH_pointed_car <- numeric(N)
p_car_of_choose_1_when_NH_pointed_car <- numeric(N)
p_car_of_choose_2_when_NH_pointed_car <- numeric(N)
p_car_of_choose_0_when_NH_pointed_goat <- numeric(N)
p_car_of_choose_1_when_NH_pointed_goat <- numeric(N)
p_car_of_choose_2_when_NH_pointed_goat <- numeric(N)


for (n in 1:N) {
  
  N_sim <- 10000
  car_door <- sample(0:2, N_sim, repl = T)
  lie <- runif(N_sim, min=0,max=1) < p_of_lie[n]
  
  pointed_car <- ((car_door==1)&(!lie)) | ((car_door!=1)&(lie))
  pointed_goat <- ((car_door==1)&(lie)) | ((car_door!=1)&(!lie))
  
  p_car_of_choose_0_when_NH_pointed_car[n] <- sum((car_door==0)&pointed_car ) / sum(pointed_car)
  p_car_of_choose_1_when_NH_pointed_car[n] <- sum((car_door==1)&pointed_car ) / sum(pointed_car)
  p_car_of_choose_2_when_NH_pointed_car[n] <- sum((car_door==2)&pointed_car ) / sum(pointed_car)
  
  p_car_of_choose_0_when_NH_pointed_goat[n] <- sum((car_door==0)&pointed_goat ) / sum(pointed_goat)
  p_car_of_choose_1_when_NH_pointed_goat[n] <- sum((car_door==1)&pointed_goat ) / sum(pointed_goat)
  p_car_of_choose_2_when_NH_pointed_goat[n] <- sum((car_door==2)&pointed_goat ) / sum(pointed_goat)
  
}

plot(p_of_lie, p_car_of_choose_0_when_NH_pointed_car, type="o", col="blue", pch="o", lty=1, ylim=c(0,1),
     xlab = "probability of lying", ylab = "probability of obtaining car")
title("When Monty said \"car\"")

points(p_of_lie, p_car_of_choose_1_when_NH_pointed_car, col="red", pch="*")
lines(p_of_lie, p_car_of_choose_1_when_NH_pointed_car, col="red",lty=2)

points(p_of_lie, p_car_of_choose_2_when_NH_pointed_car, col="dark red",pch="+")
lines(p_of_lie, p_car_of_choose_2_when_NH_pointed_car, col="dark red", lty=3)

legend(0.75,1.0,legend=c("p_0_car","p_1_car","p_2_car"),col=c("blue","red","dark red"),
       pch=c("o","*","+"),lty=c(1,2,3))



plot(p_of_lie, p_car_of_choose_0_when_NH_pointed_goat, type="o", col="blue", pch="o", lty=1, ylim=c(0,1),
     xlab = "probability of lying", ylab = "probability of obtaining car")
title("When Monty said \"goat\"")

points(p_of_lie, p_car_of_choose_1_when_NH_pointed_goat, col="red", pch="*")
lines(p_of_lie, p_car_of_choose_1_when_NH_pointed_goat, col="red",lty=2)

points(p_of_lie, p_car_of_choose_2_when_NH_pointed_goat, col="dark red",pch="+")
lines(p_of_lie, p_car_of_choose_2_when_NH_pointed_goat, col="dark red", lty=3)

legend(0.75,1.0,legend=c("p_0_goat","p_1_goat","p_2_goat"),col=c("blue","red","dark red"),
       pch=c("o","*","+"),lty=c(1,2,3))


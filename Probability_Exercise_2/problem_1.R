# The R code of 1st problem: Monty Hall problem

# In each simulation, we randomly place the car in a door, and the competitor randomly choose a door,
# then Monty Hall decide which door to open. If the "car door" was chosen, Monty Hall open the left 
# 2 doors with equal probability.

N_sim <- 10000

car_door <- sample(0:2, N_sim, repl = T)
chosen <- sample(0:2, N_sim, repl = T)
open_door <- numeric(N_sim)

# the index if the competitor changed his chosen door.
changed <- numeric(N_sim)

for (n in 1:N_sim) {
  hit <- car_door[n] == chosen[n]
  if (hit) {
    bias = sample(1:2, 1)
    open_door[n] <- (car_door[n] + bias) %% 3
  }
  else {
    # since the sum of three door is 0+1+2=3, 3-car_door[n]-chosen[n] is the index of the left door
    open_door[n] <- 3 - car_door[n] - chosen[n]
  }
  changed[n] <- 3 - open_door[n] - chosen[n]
}

prob_of_get_car_when_no_changed <- sum(chosen == car_door) / N_sim
prob_of_get_car_when_changed <- sum(changed == car_door) / N_sim

print(prob_of_get_car_when_no_changed)
print(prob_of_get_car_when_changed)






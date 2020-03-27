# The R code of 2nd problem: Dual Monty Hall problem

# In each simulation, we choose two of three doors as the choices of two competitors. Then Monty randomly open a "goat door"
# in the chosen doors.

N_sim <- 10000

car_door <- sample(0:2, N_sim, repl=T)

A_chosen <- sample(0:2, N_sim, repl=T)
B_chosen_bias <- sample(1:2, N_sim, repl=T)
B_chosen <- (A_chosen+B_chosen_bias) %% 3

left_door <- 3-A_chosen-B_chosen
open_door <- numeric(N_sim);

no_changed <- numeric(N_sim)
changed <- numeric(N_sim)

for (n in 1:N_sim) {
  if (left_door[n] == car_door[n]) {
    # thus the choice of A and B are all "goat"
    MH_choice <- sample(1:2, 1)
    if (MH_choice == 1) {
      open_door[n] <- A_chosen[n]
    }
    else {
      open_door[n] <- B_chosen[n]
    }
  }
  else {
    # thus one of the competitors hit the car
    open_door[n] <- 3 - car_door[n] - left_door[n]
  }
}

no_changed <- 3-open_door-left_door
changed <- left_door

prob_of_get_car_when_no_changed <- sum(no_changed == car_door) / N_sim
prob_of_get_car_when_changed <- sum(changed == car_door) / N_sim

print(prob_of_get_car_when_no_changed)
print(prob_of_get_car_when_changed)



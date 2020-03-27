# The R code of 3rd problem: Random Monty Hall Problem

# In each simulation, the Monty Hall decided a door to be open, and then the competitor chose a door. Since the condition is 
# "the door chosen by Monty is different from the door chosen by competitor", so only this case is valid. We only care about
# this case.

# NOTICE: I will stop using the for-loop if not necessary, the loops in previous problem are just examples.

N_sim <- 10000

car_door <- sample(0:2, N_sim, repl = T)
MH_decided_door <- (car_door + sample(1:2, N_sim, repl = T)) %% 3
chosen_door <- sample(0:2, N_sim, repl = T)
changed <- 3 - MH_decided_door - chosen_door

valid_case <- MH_decided_door != chosen_door

prob_of_get_car_when_no_changed <- sum(valid_case & (chosen_door == car_door)) / sum(valid_case)
prob_of_get_car_when_changed <- sum(valid_case & (changed == car_door)) / sum(valid_case)

print(prob_of_get_car_when_no_changed)
print(prob_of_get_car_when_changed)







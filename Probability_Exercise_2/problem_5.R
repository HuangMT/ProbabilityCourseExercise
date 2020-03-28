# The R code of 5th problem: Progressive Monty Hall Problem

# In each simulation, we record the chosing/open series, and count the number of cases satisfied the condition.
# The indexes of doors in this script are "0,1,2,3,4" which was identified with "1,2,3,4,5" in the homework.

N_sim <- 100000

# Step 1: the car was randomly placed behind a door
car_door <- sample(0:4, N_sim, repl=T)

# Step 2: the competitor choose the door 0 (1)

# Step 3: Monty randomly open an unchosen "goat" door
open_1 <- numeric(N_sim)
for (n in 1:N_sim) {
  if (car_door[n] == 0) {
    open_1[n] <- sample(1:4, 1)
  } else {
    open_1[n] <- (car_door[n]-1+sample(1:3,1)) %% 4 + 1
  }
}
valid <- open_1 == 1

#   Now we calculate the probability of the doors with car behind it
prob_of_car_in_each_door <- numeric(5)
for (n in 1:5) {
  prob_of_car_in_each_door[n] <- sum(valid&(car_door==n-1)) / sum(valid)
}

# Setp 4: the competitor choose door 2 (3)

# Step 5: the Monty opened a door again
#   the case is a little complicated, let explain it in detail
#   door 1 was open, and door 2 was chosen, so in fact, Monty randomly opened a "goat" door in 3,4,0
open_2 <- numeric(N_sim)
for (n in 1:N_sim) {
  if (car_door[n] == 2) {
    open_2[n] <- sample(c(3,4,0),1)
  } else {
    tmp <- 0:4
    tmp <- tmp[-c(car_door[n],1,2)-1]
    open_2[n] <- sample(tmp, 1)
  }
}

#   Now we calculate the probability of Monty opening each door
prob_of_open_each_door <- numeric(5)
for (n in 1:5) {
  prob_of_open_each_door[n] <- sum(valid&(open_2==n-1)) / sum(valid)
}

valid <- valid&(open_2 == 3)

#   At last we calculate the probability of car behind each door
prob_of_car_in_each_door_2 <- numeric(5)
for (n in 1:5) {
  prob_of_car_in_each_door_2[n] <- sum(valid&(car_door==n-1)) / sum(valid)
}


print(prob_of_car_in_each_door)
print(prob_of_open_each_door)
print(prob_of_car_in_each_door_2)


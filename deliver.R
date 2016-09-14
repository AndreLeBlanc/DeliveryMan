source("DeliveryMan.R")
source("aStar.R")

# manhattan distance is used to find nearest package 
# and as the heurist in A*
runDeliveryMan(funcerino, 10, 2000, T, 0.1, 5, T, T)

# manhattan distance is used to find nearest package 
# and euclidean distance as the heurist in A*
runDeliveryMan(funcerino, 10, 2000, T, 0.1, 5, T, F)

# euclidean distance is used to find the nearest package
# and manhattan distance as the heuristic in A*
runDeliveryMan(funcerino, 10, 2000, T, 0.1, 5, F, T)

# euclidean distance is used to find nearest package 
# and as the heurist in A*
runDeliveryMan(funcerino, 10, 2000, T, 0.1, 5, F, F)
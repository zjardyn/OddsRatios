log_odds <- function() {
  size <- sample(300:400, 1)
  y <- sample(seq(0,1,by=0.01) ,size, replace = T)
  disease <- sum(y < 0.08)
  z <- sample(seq(0,1,by=0.01) ,size, replace = T)
  mutated <- sum(z < 0.39)
  
  non_mutated <- size - mutated
  non_disease <- size - disease
  
  mutated_disease <- round((disease /size) * (mutated / size) * size,0) 
  mutated_nodisease <- mutated - mutated_disease
  
  normal_disease <- disease - mutated_disease
  normal_nodisease <- non_mutated - normal_disease
  
  pre_mat <- c(mutated_disease, mutated_nodisease, normal_disease, normal_nodisease)
  
  mat <- matrix(pre_mat,nrow = 2, byrow = T)
  log_odds_ratio <- log((mat[1,1] / mat[1,2]) / (mat[2,1] / mat[2,2]))
  log_odds_ratio
}

vec <- vector(length = 10000)

for (i in seq(vec)){
  vec[i] <- log_odds()
}

hist(vec)

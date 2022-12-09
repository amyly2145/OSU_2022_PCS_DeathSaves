#calculate the probability of survival

survival_prob <- function(saves, B, rule, status, reason, roll_time){
  
  samp <- rerun(B, roll(saves, rule, status, reason, roll_time))
  prob <- mean(unlist(samp))
  sd <- sd(unlist(samp))
  return(list(prob = prob, sd = sd))
  
}


# changes the way a roll is performed based on the custom rule chosen.

mod_roll <- function(rule = c("original", "advantage", "disadvantage"), roll_time) {
  rule <- match.arg(rule)
  
  if (rule == "advantage") {
    roll <- max(sample(1:20, size=roll_time, replace=TRUE)) #advantage
  }
  else if (rule == "disadvantage") {
    roll <- min(sample(1:20, size=roll_time, replace=TRUE)) #disadvantage
  }
  else {
    roll <- sample(1:20, size=1, replace=TRUE) #original
  }
  return(roll)
  
}
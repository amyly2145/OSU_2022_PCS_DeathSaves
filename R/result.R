#function assess whether the current roll is a success or fail based on the following thresholds
result <- function(roll) {
  result <- c()
  
  if (roll<10 && roll >1) {
    result <-c("fail")
  } else if (roll >= 10 && roll <20){
    result <-c("success")
  } else if (roll==1){
    result <- c("fail", "fail")
  } else if (roll ==20) {
    result <- c("success", "success", "success")
  }
  
  return(result)
}


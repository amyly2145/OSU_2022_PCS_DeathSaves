#status is defined as how many successes/failures is required before providing advantage/disadvantage

roll <- function(saves, rule, status, reason = c("success", "fail"), roll_time) { 
  reason <- match.arg(reason)
  success <- 0 #success counter
  fail <- 0 #failure counter
  status <- status
  if (status == 0 & rule %in% c("advantage", "disadvantage")){
    roll <- mod_roll(rule, roll_time)
  } else {
    roll <- sample(1:20, size=1, replace=TRUE)
  }
  
  while (success <= saves && fail <= saves) {
    #perform death save throw based on desired rules
    success <- success + sum(result(roll)=="success")
    fail <- fail + sum(result(roll) =="fail")
    
    if (reason == "success"){
      if (success >= status){
        roll <- mod_roll(rule, roll_time)
      } else { 
        roll <- sample(1:20, size=1, replace=TRUE)
      }
    }
    
    if (reason == "fail"){
      if (fail >= status){
        roll <- mod_roll(rule, roll_time)
      } else { 
        roll <- sample(1:20, size=1, replace=TRUE)
      }
    }
    
    if (success >= saves || fail >= saves) {
      break
    }
  }
  
  survive <- ifelse(success >=saves, 1, 0)
  return(survive)
}
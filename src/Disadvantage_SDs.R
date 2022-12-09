library(miceadds)
source.all("./R", ".R")
library(tidyverse)

roll_time <- 2
B <- 1000

saves_list <- seq(from = 1, to = 16, by = 1)

status_list <- seq(from = 0, to = 5, by = 1)

simulation_params <- list(
  saves = saves_list, status = status_list
)

df <- cross_df(simulation_params)

df_fail <- df %>% 
  mutate(
    p_hat = unlist(map2(.x = saves, .y = status, 
                        ~ survival_prob(saves = .x, B, rule = "disadvantage", status = .y, reason = "fail", roll_time)$prob)), 
    sd = unlist(map2(.x = saves, .y = status, 
                     ~ survival_prob(saves = .x, B, rule = "disadvantage", status = .y, reason = "fail", roll_time)$sd)), 
    status = as.factor(status)
  ) 

#summarize the sd and mean of survival probability
df_stats <- df_fail %>% 
  group_by(status, saves) %>% 
  summarize(max_sd = max(sd),
            prob = mean(p_hat),
            min_sd = min(sd)) %>% 
  print(n=96)

write.table(df_stats, file = "disad_stats.txt", sep = ",", quote = FALSE, row.names = F)
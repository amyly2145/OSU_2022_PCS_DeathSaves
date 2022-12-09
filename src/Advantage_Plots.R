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

df_success <- df %>% 
  mutate(
    p_hat = unlist(map2(.x = saves, .y = status, 
                  ~ survival_prob(saves = .x, B, rule = "advantage", status = .y, reason = "success", roll_time)$prob)), 
    sd = unlist(map2(.x = saves, .y = status, 
              ~ survival_prob(saves = .x, B, rule = "advantage", status = .y, reason = "success", roll_time)$sd)), 
    status = as.factor(status)
    ) 

ggplot(df_success, aes(x=saves, y=p_hat, color = status))+
  geom_point(size=0.5) +
  geom_line() + 
  geom_pointrange(aes(
    ymin = p_hat - 1.96*sd/sqrt(B),
    ymax = p_hat + 1.96*sd/sqrt(B)))+
  scale_x_continuous(breaks=seq(0, 16, 2))+
  ggtitle("Probability Based on Number of Saves Required", 
          subtitle =  "Scenario: Give Advantage Based on Successes")+
  labs(y = "Probability of survival", x = "Number of Saves Required")+
  guides(color = guide_legend(title="Success Threshold"))+
  theme(legend.title = element_text(size=10))


df_fail <- df %>% 
  mutate(
    p_hat = unlist(map2(.x = saves, .y = status, 
                        ~ survival_prob(saves = .x, B, rule = "advantage", status = .y, reason = "fail", roll_time)$prob)), 
    sd = unlist(map2(.x = saves, .y = status, 
                     ~ survival_prob(saves = .x, B, rule = "advantage", status = .y, reason = "fail", roll_time)$sd)), 
    status = as.factor(status)
  ) 

ggplot(df_fail, aes(x=saves, y=p_hat, color = status))+
  geom_point(size=0.5) +
  geom_line() + 
  geom_pointrange(aes(
    ymin = p_hat - 1.96*sd/sqrt(B),
    ymax = p_hat + 1.96*sd/sqrt(B)))+
  scale_x_continuous(breaks=seq(0, 16, 2))+
  ggtitle("Probability Based on Number of Saves Required", 
          subtitle =  "Scenario: Give Advantage Based on Fails")+
  labs(y = "Probability of survival", x = "Number of Saves Required")+
  guides(color = guide_legend(title="Fail Threshold"))+
  theme(legend.title = element_text(size=10))

library(miceadds)
source.all("./R", ".R")
library(tidyverse)

roll_time <- 2
B <- 1000

saves_list <- seq(from = 1, to = 16, by = 1)

rule_list <- c("original", "advantage", "disadvantage")

simulation_params <- list(
  saves = saves_list, rule = rule_list
)

df <- cross_df(simulation_params)

df_rules <- df %>% 
  mutate(
    p_hat = unlist(map2(.x = saves, .y = rule, 
                        ~ survival_prob(saves = .x, B, rule = .y, status = 0, reason = "success", roll_time)$prob)), 
    sd = unlist(map2(.x = saves, .y = rule, 
                     ~ survival_prob(saves = .x, B, rule = .y, status = 0, reason = "success", roll_time)$sd)), 
    rule = as.factor(rule)
  ) 


ggplot(df_rules, aes(x=saves, y=p_hat, color = rule))+
  geom_point(size=0.5) +
  geom_line() + 
  geom_pointrange(aes(
    ymin = p_hat - 1.96*sd/sqrt(B),
    ymax = p_hat + 1.96*sd/sqrt(B)))+
  scale_x_continuous(breaks=seq(0, 16, 2))+
  ggtitle("Probability Based on Rules", 
          subtitle = "Full Advantage, Full Disadvantage, Original")+
  labs(y = "Probability of survival", x = "Number of Saves Required")+
  guides(color = guide_legend(title="Rule"))+
  theme(legend.title = element_text(size=10))


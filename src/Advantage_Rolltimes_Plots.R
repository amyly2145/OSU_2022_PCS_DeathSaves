library(miceadds)
source.all("./R", ".R")
library(tidyverse)

B <- 1000

reroll.labs <- c("Reroll 2x", "Reroll 3x", "Reroll 4x")
names(reroll.labs) <- c("2", "3", "4")

saves_list <- seq(from = 1, to = 10, by = 1)

status_list <- seq(from = 0, to = 3, by = 1)

rolltime_list <- seq(from = 2, to =4, by = 1)

simulation_params <- list(
  saves = saves_list, 
  status = status_list, 
  roll_time = rolltime_list
)

df <- cross_df(simulation_params)

df_success <- df %>% 
  mutate(
    p_hat = unlist(pmap(list(..1 = saves, ..2 = status, ..3 =roll_time), 
                        ~ survival_prob(saves = ..1, B, rule = "advantage", status =..2, reason = "success", roll_time = ..3)$prob)), 
    sd = unlist(pmap(list(..1 = saves, ..2 = status, ..3 =roll_time), 
                     ~ survival_prob(saves = ..1, B, rule = "advantage", status =..2, reason = "success", roll_time = ..3)$sd)), 
    status = as.factor(status), 
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
  facet_wrap(~roll_time, ncol =3, labeller = labeller(roll_time = reroll.labs)) +
  guides(color = guide_legend(title="Success Threshold"))+
  theme(legend.title = element_text(size=10), legend.position="bottom",
        legend.box="horizontal")


df_fail <- df %>% 
  mutate(
    p_hat = unlist(pmap(list(..1 = saves, ..2 = status, ..3 =roll_time), 
                        ~ survival_prob(saves = ..1, B, rule = "advantage", status =..2, reason = "success", roll_time = ..3)$prob)), 
    sd = unlist(pmap(list(..1 = saves, ..2 = status, ..3 =roll_time), 
                     ~ survival_prob(saves = ..1, B, rule = "advantage", status =..2, reason = "success", roll_time = ..3)$sd)), 
    status = as.factor(status), 
  ) 


ggplot(df_fail, aes(x=saves, y=p_hat, color = status))+
  geom_point(size=0.5) +
  geom_line() + 
  geom_pointrange(aes(
    ymin = p_hat - 1.96*sd/sqrt(B),
    ymax = p_hat + 1.96*sd/sqrt(B)))+
  scale_x_continuous(breaks=seq(0, 16, 2))+
  ggtitle("Probability Based on Number of Saves Required", 
          subtitle =  "Scenario: Give Advantage Based on Fail")+
  labs(y = "Probability of survival", x = "Number of Saves Required")+
  facet_wrap(~roll_time, ncol =3, labeller = labeller(roll_time = reroll.labs)) +
  guides(color = guide_legend(title="Fail Threshold"))+
  theme(legend.title = element_text(size=10), legend.position="bottom",
        legend.box="horizontal")
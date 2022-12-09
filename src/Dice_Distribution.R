library(tidyverse)

roll_og <- unlist(rerun(10000, sample(1:20, size=1, replace=TRUE)))
roll_ad <- unlist(rerun(10000, max(sample(1:20, size=2, replace=TRUE))))
roll_dis <- unlist(rerun(10000, min(sample(1:20, size=2, replace=TRUE))))

df <- data.frame(roll_og, roll_ad, roll_dis)

df <- df %>%
  pivot_longer(everything(), names_to = "Rule", values_to = "Number") %>% 
  mutate(Rule=recode(Rule, 
                    'roll_og'="Original",
                    'roll_ad'="Advantage", 
                    'roll_dis' = "Disadvantage"))

ggplot(df, aes(x=Number, fill = Rule))+
  geom_histogram()+
  ggtitle("Dice Distribution Based on Rules", 
          subtitle = "Full Advantage, Full Disadvantage, Original")+
  labs(y = "Frequency", x = "Dice Number")+
  facet_wrap(vars(Rule))+
  guides(color = guide_legend(title="Rule"))+
  theme(legend.title = element_text(size=10), legend.position="bottom",
        legend.box="horizontal")


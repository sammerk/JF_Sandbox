################################################################################
# Roback Legler: Chapter 1                                                   ###
################################################################################

library(tidyverse)
library(easystats)
library(GGally)
library(sjPlot)


derby.df <- read_csv("https://raw.githubusercontent.com/proback/BeyondMLR/refs/heads/master/data/derbyplus.csv")

derby.df <- derby.df %>%
  mutate(fast = ifelse(condition=="fast",1,0), 
         good = ifelse(condition=="good",1,0),
         yearnew = year - 1896,
         fastfactor = ifelse(fast == 0, "not fast", "fast"))

ggpairs(data = derby.df, 
        columns = c("condition", "year", "starters", "speed"))

model1 <- lm(speed ~ year, data = derby.df)
check_model(model1)

model2 <- lm(speed ~ yearnew, data = derby.df)
plot(model2)
derby.df <- mutate(derby.df, yearnew2 = yearnew^2)
model2q <- lm(speed ~ yearnew + yearnew2, data = derby.df)

plot_model(model2q, type = "pred")

compare_performance(model1, model2, model2q)
tab_model(model1, model2, model2q)


model2q <- lm(speed ~ yearnew + yearnew2, data = derby.df)
sjPlot::plot_model(model2q, type = "eff", term = "yearnew")
sjPlot::plot_model(model2q, type = "eff", term = "yearnew2")


model3 <- lm(speed ~ fast, data = derby.df)


model4a <- lm(speed ~ fast, data = derby.df)
summary(model4a)
sjPlot::plot_model(model4a, type = "eff", term = "fast")


model4 <- lm(speed ~ yearnew + fast, data = derby.df)
summary(model4)

compare_performance(model3, model4)
tab_model(model1, model2, model3, model4)

model5 <- lm(speed ~ yearnew + fast + yearnew:fast, 
             data=derby.df)

model0 <- lm(speed ~ yearnew + yearnew2 + fast + good +
                 starters, data = derby.df)


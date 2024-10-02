# plot_stratification.R

library(tidyverse)
library(patchwork)

strat <- read.table("Stratification.dat", header = TRUE, sep = "", col.names = c("Year", "SS", "S27"))


ggp1 <- ggplot(data=strat, aes(x=Year,y=SS)) +
  geom_point(color='red') +
  geom_smooth(method = 'lm', se = TRUE, color = "black") +
  ylab(expression(paste("Stratification (kg ", m^{-3}, ")")))  +
  xlab("") +
  geom_label(aes(x=2000, y=0.75, label = "Scotian Shelf"), color = "black")
# fit a linear model to the anomaly data
model_SS <- lm(strat$SS ~ strat$Year)
model_SS
# estimate the 95% confidence interval
confidence_95_SS <- confint(model_SS, level = 0.95)


ggp2 <- ggplot(data=strat, aes(x=Year,y=S27)) +
  geom_point(color='red') +
  geom_smooth(method = 'lm', se = TRUE, color = "black") +
  ylab(expression(paste("Stratification (kg ", m^{-3}, ")")))  +
  xlab("Year") +
  geom_label(aes(x=2000, y=1, label = "S27 - Newfoundland Shelf"), color = "black")
# fit a linear model to the anomaly data
model_S27 <- lm(strat$S27 ~ strat$Year)
model_S27
# estimate the 95% confidence interval
confidence_95_S27 <- confint(model_S27, level = 0.95)


ggp1/ggp2

# ggsave("stratification.png", width = 6, height = 3, units = "in", scale = 1.5, dpi = 1200)

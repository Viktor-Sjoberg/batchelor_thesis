# -------------------------------------
# Script: 

# Author: Claes Croneborg

# Purpose: 

# Notes:

# License: MIT

# Encoding: UTF-8-SV
Sys.setlocale("LC_ALL", "sv_SE.UTF-8")

# Packages:
library(tidyverse)
library(readr)
library(readxl)
library(ggthemes)
library(readr)

# -------------------------------------

model <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")
tab_model(model)


launch_shinystan(model)

bayes_R2(model, digits = 3) #     Estimate   Est.Error      Q2.5     Q97.5
                            #  R2 0.6311105 0.003496928 0.6241765 0.6378645


rhat_koef <- mcmc_rhat(rhat(model)[1:9], size = 5) + yaxis_text(hjust = 1) + 
  labs(title ="Rhat koefficienter") + 
  theme_bw() + 
  theme(title = element_text(size = 22, family = "serif"))
ggsave("img/rhat_koef.png", plot = rhat_koef, height = 9, width = 9)

rhat_alla <- mcmc_rhat(rhat(model), size = 2) + 
  labs(title ="Rhat alla nivåer") + 
  theme_bw() + 
  theme(title = element_text(size = 22, family = "serif"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(color = "white"))
ggsave("img/rhat_alla.png", plot = rhat_alla, height = 9, width = 9)

t <- data.frame(x = 3:24, ranef(model)$antal)
ggplot(data = t, aes(x = x, y = Estimate.Intercept)) + geom_point() + geom_smooth(method = "lm") + theme_bw()


# Histogram av residualer för respektive effekt
pp_check(model, type = "error_hist") + yaxis_text(on = TRUE) + labs(title = "Histogram residualer av koefficienter")
ggsave("img/histogram_residualer_koefficienter.png", plot = last_plot(), height = 9, width = 9)



# Residual QQ-plot
model_data_df <- data.frame(Residuals = resid(model)[,1], Fitted = fitted(model)[,1])

ggplot(model_data_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(color = "red", lwd = 1) + 
  labs(title = "QQ-plot Residualer") + 
  theme_bw() + 
  theme(text = element_text(size = 22, family = "serif"))
ggsave("img/qq_plot_residualer.png", plot = last_plot(), height = 9, width = 9)


# Histogram av residualer
ggplot(model_data_df, aes(x = Residuals, 
                          mean = mean(Residuals), 
                          sd = sd(Residuals), 
                          binwidth = 0.01, 
                          n = length(Residuals))) +
  geom_histogram(binwidth = 0.01, 
                 colour = "white", fill = "steelblue", size = 0.1) +
  stat_function(fun = function(x) dnorm(x, mean =  mean(model_data_df$Residuals), 
                                        sd =  sd(model_data_df$Residuals)) * length(model_data_df$Residuals) * 0.01,
                color = "red", size = 1) +
  labs(title = "Histogram Residualer", x = "Residualer", y = "Antal") + 
  theme_bw() +
  theme(text = element_text(size = 22, family = "serif"))
ggsave("img/histogram_residualer.png", plot = last_plot(), height = 9, width = 9)


posterior_table(model)








### på betakoefficient
ranef_antal <- data.frame(x = 3:24, ranef(model)$antal) %>% ggplot(aes(x = x, y = Estimate.Intercept)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = Q2.5.Intercept, ymax = Q97.5.Intercept), color = "steelblue", width=.2, linewidth = 1) +
  labs(title = "Skattning av Beta_N givet N's storlek", x = "N", y = "Estimat") + 
  theme_bw() +
  theme(text = element_text(size = 22, family = "serif")) +
  coord_flip()
ggsave("img/skattning_beta_givet_N.png", plot = ranef_antal, width = 9, height = 9)




##### Plottning aposteriori koefficienter
model <- read_rds("fit/fit_vader_bantyp_antal_random_uppdaterad.rds")
mod.sum <- summary(model)

map2(names(mod.sum$random), mod.sum$random, ~mutate(.y, Koefficienter = .x)) %>% 
  bind_rows() %>% 
  mutate(namn = str_extract(row.names(.), "(?<=\\().*?(?=\\))"),
         Koefficient = ifelse(namn == "Intercept", Koefficienter, namn),
         Typ = if_else(str_detect(row.names(.), "cor"), "Korrelation", "SD"),
         Koefficient = gsub("Intercept,", "", Koefficient)) %>% 
  select(Estimate,Est.Error, 'l-95% CI',  'u-95% CI', Koefficient, Typ) %>% 
  
  ggplot(aes(x = reorder(Koefficient, Estimate) , y = Estimate, group = Typ, color = Typ)) +  
  geom_point(size = 3, position = position_dodge(0.3)) + 
  geom_errorbar(aes(ymin=`l-95% CI`, ymax = `u-95% CI`), width=.2, linewidth = 1, 
                position=position_dodge(0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  scale_color_manual(values = c("red", "steelblue")) +
  labs(title = "Aposteriorifördelning koefficienter", x = "Koefficient", y = "Estimat", color = "Typ") +
  theme_bw() +
  theme(text = element_text("serif", size = 22),
        legend.position = "bottom") +
  coord_flip()

ggsave("img/aposteriori_koefficienter.png", plot = last_plot(), width = 10, height = 6)


map2(names(mod.sum$random), mod.sum$random, ~mutate(.y, Koefficienter = .x)) %>% 
  bind_rows() %>% 
  mutate(namn = str_extract(row.names(.), "(?<=\\().*?(?=\\))"),
         Koefficient = ifelse(namn == "Intercept", Koefficienter, namn),
         Typ = if_else(str_detect(row.names(.), "cor"), "Korrelation", "SD"),
         Koefficient = gsub("Intercept,", "", Koefficient)) %>% 
  select(Estimate,Est.Error, 'l-95% CI',  'u-95% CI', Koefficient, Typ, Rhat) %>% 
  
  ggplot(aes(x = reorder(Koefficient, Rhat) , y = Rhat, group = Typ, color = Typ)) +  
  geom_point(size = 4, position = position_dodge(0.3)) + 
  scale_color_manual(values = c("red", "steelblue")) + 
  ylim(c(1, 1.05)) + 
  coord_flip() + 
  theme_bw() + 
  theme(text = element_text(family = "serif", size = 22),
        legend.position = "bottom") + 
  labs(title = "Rhat koefficienter", x = "Koefficient", y = "Rhat")

ggsave("img/rhat_koefficienter.png", plot = last_plot(), width = 10, height = 6)









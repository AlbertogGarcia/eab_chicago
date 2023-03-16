library(raster)
library(terra)
library(readxl)
library(tidyverse)
library(exactextractr)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(here)

results_dir <- here("paper/results/")
file_dir <- "C:/Users/garci/Dropbox/eab_chicago_data/"

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(did)
set.seed(0930)

eab_panel <- readRDS(paste0(file_dir, "output/eab_panel_grid5km.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### tree loss

loss_attgt <- att_gt(yname = "loss",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     xformla = ~ pct_trees_in_area + med_household_income + poverty_pct ,
                     data = eab_panel
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se)
es_results <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tree cover gain

gain_attgt <- att_gt(yname = "gain",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     xformla = ~ pct_trees_in_area + med_household_income + poverty_pct ,
                     data = eab_panel
)
gain_ovr <- aggte(gain_attgt, type = "simple")
summary(gain_ovr)
gain_es <- aggte(gain_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt)%>%
  rbind(es_results)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Canopy cover

canopy_attgt <- att_gt(yname = "canopy",
                       tname = "year",
                       idname = "grid",
                       gname = "first_detected",
                       control_group = "notyettreated",
                       xformla = ~ pct_trees_in_area + med_household_income + poverty_pct ,
                       ,
                       data = eab_panel
)
canopy_ovr <- aggte(canopy_attgt, type = "simple")
summary(canopy_ovr)
canopy_es <- aggte(canopy_attgt, type = "dynamic")

ovr_results <- data.frame("outcome" = "canopy", "ATT" = canopy_ovr$overall.att, "se" = canopy_ovr$overall.se)%>%
  rbind(ovr_results)
es_results <- data.frame("outcome" = "canopy", "ATT" = canopy_es$att.egt, "e" = canopy_es$egt, "se" = canopy_es$se.egt)%>%
  rbind(es_results)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Event study plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loss_plot <- ggplot(es_results %>% filter(outcome == "loss" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
loss_plot
ggsave(loss_plot, path = results_dir, filename = paste0("loss_es_grid5km.png"), width = 7, height = 5)


gain_plot <- ggplot(es_results %>% filter(outcome == "gain" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
gain_plot
ggsave(gain_plot, path = results_dir, filename = paste0("gain_es_grid5km.png"), width = 7, height = 5)


canopy_plot <- ggplot(es_results %>% filter(outcome == "canopy" & between(e, -7, 8)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin=ATT - 1.96*se,ymax=ATT + 1.96*se),alpha=0.2)+
  #geom_ribbon(aes(ymin=ATT - canopy_es$crit.val.egt*se,ymax=ATT + canopy_es$crit.val.egt*se),alpha=0.2)+
  geom_vline(xintercept = -0.25, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
canopy_plot
ggsave(canopy_plot, path = results_dir, filename = paste0("canopy_es_grid5km.png"), width = 7, height = 5)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### TWFE results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



library(fixest)

for(i in c(3, 5)){

  grid_area <- as.character(i^2)
  
  eab_panel <- readRDS(paste0(file_dir, "output/eab_panel_grid", i, "km.rds"))
# Main results

twfe_canopy <- feols(canopy ~ treated | year + grid, data = eab_panel)
summary(twfe_canopy)

twfe_loss <- feols(loss ~ treated | year + grid, data = eab_panel)
summary(twfe_loss)

twfe_gain <- feols(gain ~ treated | year + grid, data = eab_panel)
summary(twfe_gain)

models = list("Canopy cover" = twfe_canopy,
              "Loss" = twfe_loss,
              "Gain" = twfe_gain
)

treat_canopy_2005 <- round(mean(subset(eab_panel, year == 2005 & first_detected > 0)$canopy, na.rm = T)  , digits = 3)
treat_loss <- round(mean(subset(eab_panel, year < first_detected & first_detected > 0)$loss, na.rm = T)  , digits = 3)
treat_gain <- round(mean(subset(eab_panel, year < first_detected & first_detected > 0)$gain, na.rm = T)  , digits = 3)


rows <- tribble(~term, ~`Canopy cover`, ~Loss, ~Gain, 
                'pre-treatment mean', as.character(treat_canopy_2005), as.character(treat_loss), as.character(treat_gain)
                , 'grid area (square km)', grid_area, grid_area, grid_area
)%>%
  as.data.frame()

f1 <- function(x) format(round(x, 4), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,
             output="latex",
             title = 'TWFE estimates of canopy cover impacts of ash borer infestation',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~grid,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = c("treated" = "Infestation"),
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             , add_rows = rows
             , notes = "Standard errors are clustered at the grid level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/twfe_main_", i, "km.tex"))


# Heterogeneity in canopy cover impacts

twfe_canopy_ash <- feols(canopy ~ treated * log(trees_per_acre) | year + grid, data = eab_panel)
summary(twfe_canopy_ash)

twfe_canopy_baseline <- feols(canopy ~ treated * log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_baseline)

twfe_canopy_income <- feols(canopy ~ treated + treated:log(med_household_income) | year + grid, data = eab_panel)
summary(twfe_canopy_income)

twfe_canopy_it <- feols(canopy ~ treated + treated:log(med_household_income) + treated:log(trees_per_acre) +  treated:log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_it)

models = list("(1)" = twfe_canopy_ash,
              "(2)" = twfe_canopy_baseline,
              "(3)" = twfe_canopy_income,
              "(4)"  = twfe_canopy_it
)

names_coef <- c("treated" = "Infestation",
                "treated:log(trees_per_acre)" = "Infestation x Ash per acre",
                "treated:log(med_household_income)" = "Infestation x Med income",
                "treated:log(canopy_baseline)" = "Infestation x Canopy baseline"
                )

modelsummary(models,
             output="latex",
             title = 'Heterogeneous canopy cover impacts of ash borer infestation',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~grid,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = names_coef,
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             #    , add_rows = rows
             , notes = "Standard errors are clustered at the grid level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/twfe_het_", i, "km.tex"))


}

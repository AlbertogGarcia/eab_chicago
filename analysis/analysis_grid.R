library(tidyverse)
library(ggplot2)
library(kableExtra)
library(modelsummary)
library(here)
library(fixest)
library(did)
library(janitor)
library(ggpubr)

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

results_dir <- here::here("analysis", "results")

clean_data_dir <- here::here("cleaned")

fig_dir <- here::here("figs")

eab_panel <- readRDS(paste0(clean_data_dir, "/eab_panel_grid5km.rds"))%>%
  mutate_at(vars(place_first_detected), as.numeric)%>%
  mutate(gain = gain * 0.2223948433, # converting 900m^2 pixels into acres
         loss = loss * 0.2223948433)

set.seed(0930)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Callaway did
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ovr_results <- data.frame()

loss_attgt <- att_gt(yname = "loss",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)

loss_ovr <- aggte(loss_attgt, type = "simple")
loss_ovr

ovr_results <- data.frame("outcome" = "loss", "ATT" = loss_ovr$overall.att, "se" = loss_ovr$overall.se, 
                          "pre-treat" = mean((eab_panel %>% filter( first_detected > 0 & year < first_detected))$loss, na.rm = T), "Ngrids" = length(unique(eab_panel$grid))
)%>%
  rbind(ovr_results)

loss_es <- aggte(loss_attgt, type = "dynamic", min_e = -7, max_e = 6)

es_plot_df <- data.frame("outcome" = "loss", "ATT" = loss_es$att.egt, "e" = loss_es$egt, "se" = loss_es$se.egt, "crit" = loss_es$crit.val.egt)

loss_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("Tree loss (Acres/year)")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
loss_plot

loss_plot + ggtitle("Tree cover loss impacts of ash borer infestation by event time")
ggsave(paste0(fig_dir, "/eventstudy_loss.png"), width = 8, height = 5)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Tree cover gain

gain_attgt <- att_gt(yname = "gain",
                     tname = "year",
                     idname = "grid",
                     gname = "first_detected",
                     control_group = "notyettreated",
                     data = eab_panel
)
gain_ovr <- aggte(gain_attgt, type = "simple")
gain_ovr

ovr_results <- data.frame("outcome" = "gain", "ATT" = gain_ovr$overall.att, "se" = gain_ovr$overall.se, 
                          "pre-treat" = mean((eab_panel %>% filter( first_detected > 0 & year < first_detected))$gain, na.rm = T), "Ngrids" = length(unique(eab_panel$grid))
)%>%
  rbind(ovr_results)

gain_es <- aggte(gain_attgt, type = "dynamic", min_e = -7, max_e = 6)

es_plot_df <- data.frame("outcome" = "gain", "ATT" = gain_es$att.egt, "e" = gain_es$egt, "se" = gain_es$se.egt, "crit" = loss_es$crit.val.egt)

gain_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("Tree gain (Acres/year)")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
gain_plot

gain_plot + ggtitle("Tree cover gain impacts of ash borer infestation by event time")
ggsave(paste0(fig_dir, "/eventstudy_gain.png"), width = 8, height = 5)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Canopy cover

canopy_attgt <- att_gt(yname = "canopy",
                       tname = "year",
                       idname = "grid",
                       gname = "first_detected",
                       control_group = "notyettreated",
                       data = eab_panel
)
canopy_ovr <- aggte(canopy_attgt, type = "simple")
canopy_ovr

ovr_results <- data.frame("outcome" = "canopy", "ATT" = canopy_ovr$overall.att, "se" = canopy_ovr$overall.se, 
                          "pre-treat" = mean((eab_panel %>% filter( first_detected > 0 & year < first_detected))$canopy, na.rm = T), "Ngrids" = length(unique(eab_panel$grid))
)%>%
  rbind(ovr_results)

canopy_es <- aggte(canopy_attgt, type = "dynamic", min_e = -7, max_e = 6)

es_plot_df <- data.frame("outcome" = "canopy", "ATT" = canopy_es$att.egt, "e" = canopy_es$egt, "se" = canopy_es$se.egt, "crit" = loss_es$crit.val.egt)

canopy_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("Mean canopy cover probability")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
canopy_plot

canopy_plot + ggtitle("Canopy cover impacts of ash borer infestation by event time")
ggsave(paste0(fig_dir, "/eventstudy_canopy.png"), width = 8, height = 5)


ggarrange(canopy_plot, loss_plot, gain_plot, ncol = 1, nrow = 3,
          labels = c("A", "B", "C"))
ggsave(paste0(fig_dir, "/eventstudy_grid_trio.png"), width = 5, height = 10)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Grid level tree cover impacts table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

paper_results <- ovr_results %>%
  mutate(
    stars = ifelse(between(abs(ATT/se), 1.645, 1.96), "*",
                   ifelse(between(abs(ATT/se), 1.96, 2.58), "**",
                          ifelse(abs(ATT/se) >= 2.58, "***", "")
                   )
    )
  )%>%
  mutate_at(vars(ATT, se, pre.treat), ~ round(., digits = 3))%>%
  mutate(se = paste0("(", se, ")"),
         ATT = paste0(ATT, stars))%>%
  select(-stars)%>%
  t() %>%
  row_to_names(row_number = 1) %>%
  as.data.frame()
row.names(paper_results) <- c("ATT", " ", "Pre-treat mean", "N grid cells")

kbl(paper_results %>% dplyr::select(canopy, loss, gain),
    format = "latex",
    booktabs = T,
    caption = "Difference-in-differences estimates of the impact of ash borer infestation on tree cover outcomes across the Chicago metropolitan region. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated grid cells in the control group.",
    col.names = c("Canopy", "Loss (Acres/year)", "Gain (Acres/year)"),
    align = c("l", "c", "c", "c"),
    label = "grid-tree-table"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 3))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at grid level")%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/grid_results_tree_main.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Canopy cover place treatment assignment

place_canopy_attgt <- att_gt(yname = "canopy",
                       tname = "year",
                       idname = "grid",
                       gname = "place_first_detected",
                       control_group = "notyettreated",
                       data = eab_panel
)
canopy_ovr <- aggte(place_canopy_attgt, type = "simple")
canopy_ovr

canopy_es <- aggte(place_canopy_attgt, type = "dynamic", min_e = -7, max_e = 6)

es_plot_df <- data.frame("outcome" = "canopy", "ATT" = canopy_es$att.egt, "e" = canopy_es$egt, "se" = canopy_es$se.egt, "crit" = loss_es$crit.val.egt)

place_canopy_plot <- ggplot(es_plot_df, aes(x = e, y = ATT)) + 
  ylab("ATT")+ xlab("Years since infestation detection")+
  ggtitle("Canopy cover impacts of ash borer infestation by event time")+
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
place_canopy_plot

ggsave(plot = place_canopy_plot, paste0(fig_dir, "/eventstudy_canopy_place.png"), width = 8, height = 5)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Heterogeneity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
twfe_canopy_trees <- feols(canopy ~ treated + treated:log(trees_per_acre) +  treated:log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_trees, vcov = ~grid)

twfe_canopy_income <- feols(canopy ~ treated + treated:log(med_household_income) +  treated:log(canopy_baseline)| year + grid, data = eab_panel)
summary(twfe_canopy_income, vcov = ~grid)

twfe_canopy_it <- feols(canopy ~ treated + treated:log(med_household_income) + treated:log(trees_per_acre) +  treated:log(canopy_baseline) | year + grid, data = eab_panel)
summary(twfe_canopy_it, vcov = ~grid)

models = list("(1)" = twfe_canopy_trees,
              "(2)" = twfe_canopy_income,
              "(3)"  = twfe_canopy_it
)

names_coef <- c("treated" = "Infestation",
                "treated:log(trees_per_acre)" = "Infestation x ln(Ash per acre)",
                "treated:log(med_household_income)" = "Infestation x ln(Med. income)",
                "treated:log(canopy_baseline)" = "Infestation x ln(Canopy baseline)"
)

f1 <- function(x) format(round(x, 4), big.mark=",")
options("modelsummary_format_numeric_latex" = "plain")
modelsummary(models,
             label = "twfe-grid-het",
             output="latex",
             title = 'Heterogeneous canopy cover impacts of ash borer infestation.',
             fmt = f1, # 4 digits and trailing zero
             vcov = ~grid,
             stars = c('*' = .1, '**' = .05, '***' = .01),
             coef_rename = names_coef,
             gof_omit = 'DF|Deviance|Adj|Within|Pseudo|AIC|BIC|Log|Year|FE|Std|RMSE'
             #    , add_rows = rows
             , notes = "Standard errors are clustered at the grid level."
)%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/twfe_het_grid_main.tex"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##### Varying grid size
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_files <- c(
  seq(from = 1, to = 10)
                )
canopy_results <- data.frame()

for(i in grid_files){
  
  eab_panel <- readRDS(paste0(clean_data_dir, "/eab_panel_grid", i, "km.rds"))%>%
    mutate_at(vars(place_first_detected), as.numeric)%>%
    mutate(gain = gain * 0.2223948433, # converting 900m^2 pixels into acres
           loss = loss * 0.2223948433)
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### Canopy cover
  
  canopy_attgt <- att_gt(yname = "canopy",
                         tname = "year",
                         idname = "grid",
                         gname = "first_detected",
                         control_group = "notyettreated",
                         data = eab_panel
  )
  canopy_ovr <- aggte(canopy_attgt, type = "simple")
  
  canopy_results <- data.frame("grid_size" = i, "ATT" = canopy_ovr$overall.att, "se" = canopy_ovr$overall.se) %>% rbind(canopy_results)
  
  print(i)
}

ggplot(canopy_results, aes(x = grid_size, y = ATT))+
  geom_errorbar(aes(ymin = ATT - 1.96*se, ymax = ATT + 1.96*se), width = 0.5)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(size = 4, shape = 21, fill = palette$green, color = palette$dark)+
  theme_minimal()+
  ylim(-0.5, 0.2) +
  xlab("Grid cell size (km)") + ggtitle("Canopy cover impacts of infestations across grid cell sizes")+
  scale_x_continuous(breaks = grid_files)
ggsave(paste0(fig_dir, "/grid_sizes_schart.png"), width = 8, height = 5)





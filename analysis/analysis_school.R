library(fixest)
library(tidyverse)
library(here)
library(did)
#library(Hmisc)
library(fixest)
library(janitor)
library(kableExtra)
library(png)
library(grid)
library(ggpubr)

select <- dplyr::select
source(here::here('analysis', 'schart.R'))

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

results_dir <- here::here('analysis', "results")

clean_data_dir <- here::here("cleaned")

fig_dir <- here::here("figs")

eab_panel_school <- readRDS(paste0(clean_data_dir, "/eab_panel_school1km.rds"))%>%
  mutate(gain = gain * 0.09, # converting 900m^2 pixels into hectares
         loss = loss * 0.09)

panel <- eab_panel_school %>%
  mutate_at(vars(year, first_exposed), as.numeric) %>%
  drop_na(ISAT_composite)%>%
  mutate(
    e_time = ifelse(first_exposed > 0, year - first_exposed, 0)
  )
    


pct_used <- 0.25

ggplot(panel %>% filter(first_exposed > 0), aes(x = e_time))+
  geom_histogram(binwidth = 1, color = palette$dark, alpha = 0.25)+
  theme_classic()+xlab("Event time")+
  geom_hline(yintercept = max(table((panel %>% filter(first_exposed != 0))$e_time))*pct_used, linetype = "dashed")



# ggplot(panel %>% filter(first_exposed > 0) %>% mutate(used_dynamic = ifelse(between(e_time, min_e, max_e), "Event time used in event study", "Event time not used in event study"))
#        , aes(x = e_time, fill = used_dynamic))+
#   geom_histogram(binwidth = 1, color = palette$dark, alpha = 0.75)+
#   scale_fill_manual(values = c(palette$red, palette$light_grey))+
#   theme_classic()+ theme(legend.title = element_blank())+xlab("Event time")+
#   geom_hline(yintercept = max(table((panel %>% filter(first_exposed != 0))$e_time))*pct_used, linetype = "dashed")
# ggsave(path = fig_dir, filename = "event_time_histogram.png", width = 7, height = 5)


treat_isat <- round(mean(subset(panel, year == first_exposed - 1 & first_exposed > 0)$ISAT_composite, na.rm = T)  , digits = 3)
treat_attend <- round(mean(subset(panel, year == first_exposed - 1 & first_exposed > 0)$all_attend, na.rm = T)  , digits = 3)
treat_lowinc_attend <- round(mean(subset(panel, year == first_exposed - 1 & first_exposed > 0)$low_income_attend, na.rm = T)  , digits = 3)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Logit results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cov_names <- c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct", "cov_enrollment"
               , "cov_truants_pct"
               , "cov_all_attend"
               , "pct_trees_in_area"
               , "canopy_05"
               , "canopy_0600"
               , "canopy_0604"
)

logit_df <- panel %>%
  mutate(ever_treated = ifelse(first_exposed > 0, 1, 0))%>%
  group_by(school_ID)%>%
  slice_head()

formula <- as.formula(paste("ever_treated ~ ", paste(cov_names, collapse = " + ")))

test_logit <- glm(formula, data = logit_df, family = "binomial")
summary(test_logit)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Callaway and Sant'anna estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

school_outcomes <- c("low_income_attend", "all_attend", "ISAT_composite", "enrollment")

max_e = 10
min_e = -15
ovr_results <- data.frame()
es_results_ed <- data.frame()
set.seed(1993)
for(k in school_outcomes){
  print(k)
  this_panel <- panel %>%
    mutate_at(vars(k, year), as.numeric)%>%
    rename(this_outcome = k)
  
  attgt <- att_gt(yname = "this_outcome",
                  tname = "year",
                  idname = "school_ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  base_period = "universal",
                  xformla = as.formula(paste("~ ", paste(c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct", "cov_enrollment", "cov_ISAT_composite", "trend_ISAT_composite", "`trend_low-income school pct`" , "`trend_l.e.p. school pct`", "pct_trees_in_area", "canopy_0604", "canopy_0600", "canopy_0602"), collapse = " + "))),
                  data = this_panel,
                  clustervars = "GEOID10"
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, 
                            "pre-treat" = mean((this_panel %>% filter( year == first_exposed - 1 & first_exposed > 0))$this_outcome, na.rm = T), 
                            "Nschools" = length(unique(this_panel$RCDS))
  )%>%
    rbind(ovr_results)
  
  es <- aggte(attgt, type = "dynamic", min_e = min_e, max_e = max_e, na.rm = T)
  
  es_results_ed <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "crit" = es$crit.val.egt)%>%
    mutate(se = replace_na(se, 0))%>%
    rbind(es_results_ed)
  
}

tree_outcomes <- c("gain", "loss", "canopy")

for(k in tree_outcomes){
  print(k)
  this_panel <- panel %>%
    mutate_at(vars(k, year), as.numeric)%>%
    rename(this_outcome = k) %>%
    filter(year <= 2014)

  attgt <- att_gt(yname = "this_outcome",
                  tname = "year",
                  idname = "school_ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  base_period = "universal",
                  xformla = as.formula(paste("~ ", paste(c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct", "cov_enrollment", "cov_ISAT_composite", "trend_ISAT_composite", "`trend_low-income school pct`" , "`trend_l.e.p. school pct`", "pct_trees_in_area", "canopy_0600" , "canopy_0602"), collapse = " + "))),
                  data = this_panel,
                  clustervars = "GEOID10"
  )

  ovr <- aggte(attgt, type = "simple", na.rm = T)

  ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se,
                            "pre-treat" = mean((this_panel %>% filter( first_exposed > 0 & year == first_exposed - 1))$this_outcome, na.rm = T),
                            "Nschools" = length(unique(this_panel$RCDS))
  )%>%
    rbind(ovr_results)

  es <- aggte(attgt, type = "dynamic", min_e = min_e, max_e = max_e, na.rm = T)

  es_results_ed <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "crit" = es$crit.val.egt)%>%
    mutate(se = replace_na(se, 0))%>%
    rbind(es_results_ed)

}

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
row.names(paper_results) <- c("ATT", "(se)", "Pre-treat mean", "N schools")

kbl(paper_results %>% dplyr::select(canopy, loss, gain),
  #  format = "latex",
    booktabs = T,
    caption = "Difference-in-differences estimates of the impact of ash borer infestation on tree cover outcomes within 3.22km (2 miles) of the school. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated schools in the control group.",
    col.names = c("Canopy (mean probability)", "Loss (Hectares/year)", "Gain (Hectares/year)"),
    align = c("l", "c", "c", "c"),
    label = "school-tree-table"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 3))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at census tract")%>%
 # kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/school_results_tree_2mi.html"))

kbl(paper_results %>% dplyr::select(ISAT_composite, all_attend, low_income_attend, enrollment),
    booktabs = T,
  #  format = "latex",
    caption = "Difference-in-differences estimates of the impact of ash borer infestation on school-level education outcomes. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated schools in the control group.",
    col.names = c("ISAT composite", "Attendance rate", "Low-income attend.", "Enrollment"),
    align = c("l", "c", "c", "c", "c"),
    label = "school-educ-table"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 4))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at census tract")%>%
 # kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/school_results_educ_2mi.html"))

kbl(paper_results %>% dplyr::select(ISAT_composite, all_attend, low_income_attend, enrollment, canopy, loss, gain),
    booktabs = T,
    #  format = "latex",
    caption = "Difference-in-differences estimates of the impact of ash borer infestation on school-level outcomes. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated schools in the control group.",
    col.names = c("ISAT composite", "Attendance rate", "Low-income attend.", "Enrollment", "Canopy (mean probability)", "Loss (Hectares/year)", "Gain (Hectares/year)"),
    align = c("l", "c", "c", "c", "c", "c", "c", "c"),
    label = "school-table"
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 7))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at census tract")%>%
  # kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/school_results_2mi.html"))


isat_plot <- ggplot(es_results_ed %>% filter(outcome == "ISAT_composite"),
                    aes(x = e, y = ATT)) +
  ylab("% students meet or exceed\nISAT cutoff (composite)")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -1, linetype = "dashed", color = palette$red)+
  geom_hline(yintercept = 0)+
  theme_classic()
isat_plot

ggsave(path = fig_dir, filename = "es_school_isat_2mi.png", width = 8, height = 4)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By subject, grade, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test <- c("ISAT_")
groups <- c(
  "low income_", "non-low income_")
grades <- c("gr8", "gr7", "gr6", "gr5", "gr4", "gr3")
categ <- c(" read school meets", " math school meets", 
           " read school academic warning", " math school academic warning",
           " read school below", " math school below", 
           " read school exceeds", " math school exceeds"
)
tests <- as.vector(outer(as.vector(outer(test, groups, paste0)), as.vector(outer(grades, categ, paste0)), paste0))



ovr_results_isat <- data.frame()

plot_list_meets <- vector('list', 24)
plot_list_below <- vector('list', 24)
plot_list_warning <- vector('list', 24)
plot_list_exceeds <- vector('list', 24)

set.seed(1993)
for(k in tests){
  print(k)
  # k = tests[31]
  grade = gsub('gr','', strsplit(strsplit(k, split = "_")[[1]][3], " ")[[1]][1])
  group = strsplit(k, split = "_")[[1]][2]
  subject = ifelse(grepl("math", k, fixed = TRUE), "math", "reading")
  benchmark = ifelse(grepl("meets", k, fixed = TRUE), "meets", 
                     ifelse(grepl("warning", k, fixed = TRUE), "academic warning",
                            ifelse(grepl("exceeds", k, fixed = TRUE), "exceeds", "below")
                     )
  )
  
  this_panel <- panel %>%
    mutate_at(vars(k), as.numeric)
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "school_ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  base_period = "universal",
                  xformla = as.formula(paste("~ ", paste(c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct", "cov_enrollment", "cov_ISAT_composite", "trend_ISAT_composite", "`trend_low-income school pct`" , "`trend_l.e.p. school pct`", "pct_trees_in_area", "canopy_0604", "canopy_0600", "canopy_0602"), collapse = " + "))),
                  data = this_panel,
                  clustervars = "GEOID10"
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results_isat <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se
                                 , "group" = group, "grade" = grade, "subject" = subject, "benchmark" = benchmark
  )%>%
    rbind(ovr_results_isat)
  
  stars = ifelse(between(abs(ovr$overall.att/ovr$overall.se), 1.645, 1.96), "*",
                 ifelse(between(abs(ovr$overall.att/ovr$overall.se), 1.96, 2.58), "**",
                        ifelse(abs(ovr$overall.att/ovr$overall.se) >= 2.58, "***", "")
                 )
  )
  
  
  es <- aggte(attgt, type = "dynamic", min_e = -6, na.rm = T)
  
  es_plot_df <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "crit" = es$crit.val.egt)%>%
    mutate(se = replace_na(se, 0),
           plot_title = paste(subject, "- grade", grade, stars))
  
  
  
  if(grepl("non", k, fixed = TRUE)){
    back_color = "grey85"
  } else{
    back_color = "lightblue"
  }
  
  
  
  
  this_plot <- ggplot(es_plot_df,
                      aes(x = e, y = ATT)) +
    #ylab("ISAT achievement")+ xlab("Years since infestation detection")+
    geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
    geom_line() +
    geom_point(shape = 21, fill = palette$dark)+
    geom_vline(xintercept = -1, linetype = "dashed", color = palette$red)+
    geom_hline(yintercept = 0)+
    theme_classic()+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  this_plot <- this_plot + facet_grid(. ~ plot_title) +
    theme(strip.background = element_rect(fill=back_color),
          strip.text = element_text(size=10, color="black"))
  
  if(grepl("meets", k, fixed = TRUE)){
    meets_cats <- tests[grepl("meets", tests)]
    pos <- match(k,meets_cats)
    plot_list_meets[[pos]] <- this_plot
    
  } else if(grepl("exceeds", k, fixed = TRUE)){
    exceeds_cats <- tests[grepl("exceeds", tests)]
    pos <- match(k,exceeds_cats)
    plot_list_exceeds[[pos]] <- this_plot
    
  } else if(grepl("below", k, fixed = TRUE)){
    below_cats <- tests[grepl("below", tests)]
    pos <- match(k,below_cats)
    plot_list_below[[pos]] <- this_plot
    
  } else{
    warning_cats <- tests[grepl("warning", tests)]
    pos <- match(k,warning_cats)
    plot_list_warning[[pos]] <- this_plot
    
  }
  
  
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Event study plots
library(ggpubr)
library(grid)
figure <- ggarrange(plotlist=plot_list_meets, 
                    ncol = 4, nrow = 6,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("ATT (pct. students at benchmark)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Years since infestation", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/subplots_meets.png")
       , width = 9, height = 12)

figure <- ggarrange(plotlist=plot_list_exceeds, 
                    ncol = 4, nrow = 6,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("ATT (pct. students at benchmark)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Years since infestation", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/subplots_exceeds.png")
       , width = 9, height = 12)

figure <- ggarrange(plotlist=plot_list_below, 
                    ncol = 4, nrow = 6,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("ATT (pct. students at benchmark)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Years since infestation", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/subplots_below.png")
       , width = 9, height = 12)

figure <- ggarrange(plotlist=plot_list_warning, 
                    ncol = 4, nrow = 6,
                    labels = NULL
                    # , common.legend = TRUE,
                    #  , legend = "top"
)
annotate_figure(figure, left = textGrob("ATT (pct. students at benchmark)", rot = 90, vjust = 1, gp = gpar(cex = 1.1)),
                bottom = textGrob("Years since infestation", gp = gpar(cex = 1.1)))
ggsave(filename = paste0(fig_dir, "/subplots_warning.png")
       , width = 9, height = 12)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Specification charts

spec_results_lowinc <- ovr_results_isat %>%
  filter(group == "low income") %>%
  pivot_wider(names_from = benchmark, values_from = benchmark, values_fn = list(benchmark = ~TRUE), values_fill = list(benchmark = 0))%>%
  arrange(exceeds, meets, below, `academic warning`)%>%
  #pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, group, `academic warning`, below, meets, exceeds))%>%
  as.data.frame()

lowinc_sig <- which(abs(spec_results_lowinc$ATT) >= abs(1.96*spec_results_lowinc$se))

spec_results_nonlowinc <- ovr_results_isat %>%
  filter(group != "low income") %>%
  pivot_wider(names_from = benchmark, values_from = benchmark, values_fn = list(benchmark = ~TRUE), values_fill = list(benchmark = 0))%>%
  arrange(exceeds, meets, below, `academic warning`)%>%
  #pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, group, `academic warning`, below, meets, exceeds))%>%
  as.data.frame()

nonlowinc_sig <- which(abs(spec_results_nonlowinc$ATT) >= abs(1.96*spec_results_nonlowinc$se))

labels <- list(
  "Subject" = c("Math", "Reading"),
  "Grade" = c("3rd", "4th", "5th","6th", "7th", "8th")
)

duo_heights = c(5,4)

png(paste0(fig_dir,"/schart_isat_lowinc_2mi.png"), width = 9, height = 6, units = "in", res = 500)
par(oma=c(1,0,1,1))
schart(spec_results_lowinc, ci=c(0.9, 0.95), ylab="", labels = labels,
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c(palette$dark,"white","white", palette$blue),
       col.est=c(palette$dark, palette$blue),
       n = 12, heights=duo_heights#, highlight = lowinc_sig
) 
abline(v=13)
abline(v=26)
abline(v=39)
text(x=6, y=7.2, "Academic Warning", col=palette$dark, font=2, cex = 0.8)
text(x=19.5, y=7.2, "Below", col=palette$dark, font=2, cex = 0.8)
text(x=32.5, y=7.2, "Meets", col=palette$dark, font=2, cex = 0.8)
text(x=46, y=7.2, "Exceeds", col=palette$dark, font=2, cex = 0.8)
legend(x=1, y=-6, col = palette$dark, legend = "95% CI", seg.len=0.65, inset = 0.005,  box.lty=0, cex=0.9, lty = 1, lwd = 4, bg="transparent")
legend(x=1, y=-7, col = "grey", legend = "90% CI", seg.len=0.65, inset = 0.005,  box.lty=0, cex=0.9, lty = 1, lwd = 4, bg="transparent")

dev.off()


png(paste0(fig_dir,"/schart_isat_nonlowinc_2mi.png"), width = 9, height = 6, units = "in", res = 500)
par(oma=c(1,0,1,1))
schart(spec_results_nonlowinc, ci=c(0.9, 0.95), ylab="", labels = labels,
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c(palette$dark,"white","white", palette$blue),
       col.est=c(palette$dark, palette$blue),
       n=12, heights=duo_heights#, highlight = nonlowinc_sig
) 
abline(v=13)
abline(v=26)
abline(v=39)
text(x=6, y=3.25, "Academic Warning", col=palette$dark, font=2, cex = 0.8)
text(x=19.5, y=3.25, "Below", col=palette$dark, font=2, cex = 0.8)
text(x=32.5, y=3.25, "Meets", col=palette$dark, font=2, cex = 0.8)
text(x=46, y=3.25, "Exceeds", col=palette$dark, font=2, cex = 0.8)
legend(x=1, y=-3, col = palette$dark, legend = "95% CI", seg.len=0.65, inset = 0.005,  box.lty=0, cex=0.9, lty = 1, lwd = 4, bg="transparent")
legend(x=1, y=-3.5, col = "grey", legend = "90% CI", seg.len=0.65, inset = 0.005,  box.lty=0, cex=0.9, lty = 1, lwd = 4, bg="transparent")

dev.off()

plot1 <- readPNG(paste0(fig_dir,'/schart_isat_nonlowinc_2mi.png'))
plot2 <- readPNG(paste0(fig_dir,'/schart_isat_lowinc_2mi.png'))


schart_duo <- ggarrange(rasterGrob(plot1),rasterGrob(plot2), 
          nrow= 2, ncol = 1,
          labels = c("A", "B"))

annotate_figure(schart_duo, 
                top = text_grob("Non-low-income student impacts", 
                                            size = 7, color = "black"),
                bottom = text_grob("Low-income student impacts", 
                                size = 7, color = "black", vjust = -33.5)
                )

annotate_figure(schart_duo, top = text_grob("Non-low-income vs. low-income student impacts",
                                      color = "black", size = 7.5))


ggsave(paste0(fig_dir,'/schart_duo_2mi.png')
       , width = 5, height = 5
)


library(fixest)
library(tidyverse)
library(here)
library(did)
library(Hmisc)
library(fixest)
library(janitor)
library(kableExtra)
library(ggpubr)

select <- dplyr::select
source(here::here('chapter3', 'schart.R'))

palette <- list("white" = "#FAFAFA",
                "light_grey" = "#d9d9d9",
                "dark" = "#0c2230",
                "red" = "#ed195a",
                "blue" = "#1c86ee",
                "green" = "#7CAE7A",
                "dark_green" = "#496F5D",
                "gold" = "#DAA520")

file_dir <- "C:/Users/garci/Dropbox/eab_chicago_data"

cleaned_dir <- here::here("cleaned")

results_dir <- here::here("results")

fig_dir <- here::here("figs")

set.seed(0930)

eab_panel_school <- readRDS(paste0(cleaned_dir, "/eab_panel_school2mi.rds"))%>%
  mutate(gain = gain * 0.2223948433, # converting 900m^2 pixels into acres
         loss = loss * 0.2223948433)

panel <- eab_panel_school %>%
  mutate_at(vars(year, first_exposed), as.numeric) %>%
  mutate(e_time = ifelse(first_exposed > 0, year - first_exposed, 0))

pct_used <- 0.1

ggplot(panel %>% filter(first_exposed > 0), aes(x = e_time))+
  geom_histogram(binwidth = 1, color = palette$dark, alpha = 0.25)+
  theme_classic()+xlab("Event time")+
  geom_hline(yintercept = max(table((panel %>% filter(first_exposed != 0))$e_time))*pct_used, linetype = "dashed")

max_e = 6
min_e = -8

ggplot(panel %>% filter(first_exposed > 0) %>% mutate(used_dynamic = ifelse(between(e_time, min_e, max_e), "Event time used in event study", "Event time not used in event study"))
       , aes(x = e_time, fill = used_dynamic))+
  geom_histogram(binwidth = 1, color = palette$dark, alpha = 0.75)+
  scale_fill_manual(values = c(palette$red, palette$light_grey))+
  theme_classic()+ theme(legend.title = element_blank())+xlab("Event time")+
  geom_hline(yintercept = max(table((panel %>% filter(first_exposed != 0))$e_time))*pct_used, linetype = "dashed")
ggsave(path = fig_dir, filename = "event_time_histogram.png", width = 7, height = 5)


treat_tests <- round(mean(subset(panel, year == 2005 & first_exposed > 0)$all_tests, na.rm = T)  , digits = 3)
treat_isat <- round(mean(subset(panel, year < first_exposed & first_exposed > 0)$ISAT_composite, na.rm = T)  , digits = 3)
treat_attend <- round(mean(subset(panel, year < first_exposed & first_exposed > 0)$all_attend, na.rm = T)  , digits = 3)
treat_lowinc_attend <- round(mean(subset(panel, year < first_exposed & first_exposed > 0)$low_income_attend, na.rm = T)  , digits = 3)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Callaway and Sant'anna estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outcomevars <- c("low_income_attend", "all_attend", "all_tests", "ISAT_composite",
                 "gain", "loss", "canopy", 
                 "enrollment" 
)

cov_names <- c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct"
               , "cov_enrollment")

xformula <- as.formula(paste("~ ", paste(cov_names, collapse = " + ")))

ovr_results <- data.frame()
es_results_ed <- data.frame()
for(k in outcomevars){
  print(k)
  this_panel <- panel %>%
    mutate_at(vars(k, year), as.numeric)%>%
    rename(this_outcome = k)
  
  attgt <- att_gt(yname = "this_outcome",
                  tname = "year",
                  idname = "school_ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  xformla = xformula,
                  data = this_panel
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, 
                            "pre-treat" = mean((this_panel %>% filter( first_exposed > 0 & year < first_exposed))$this_outcome, na.rm = T), "Nschools" = length(unique(panel$RCDS))
  )%>%
    rbind(ovr_results)
  
  es <- aggte(attgt, type = "dynamic", min_e = min_e, max_e = max_e, na.rm = T)
  
  es_results_ed <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "crit" = es$crit.val.egt)%>%
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
  row.names(paper_results) <- c("ATT", " ", "Pre-treat mean", "N schools")

kbl(paper_results %>% dplyr::select(canopy, loss, gain),
    format = "latex",
    booktabs = T,
    caption = "This table shows difference-in-differences estimates of the impact of ash borer infestation on tree cover outcomes within 3.22km (2 miles) of the school. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated schools in the control group.",
    col.names = c("Canopy", "Loss (Acres/year)", "Gain (Acres/year)"),
    align = c("l", "c", "c", "c")
    )%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 3))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at school level")%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/school_results_tree_2mi.tex"))

kbl(paper_results %>% dplyr::select(ISAT_composite, all_tests, all_attend, low_income_attend, enrollment),
    booktabs = T,
    format = "latex",
    caption = "This table shows difference-in-differences estimates of the impact of ash borer infestation on school-level education outcomes. All estimates are based on the Callway and Sant'anna (2020) estimator and use both not-yet-treated and never-treated schools in the control group.",
    col.names = c("ISAT composite", "All tests", "Attendance rate", "Low-income attend.", "Enrollment"),
    align = c("l", "c", "c", "c", "c", "c")
)%>%
  kableExtra::row_spec(2, hline_after = TRUE)%>%
  add_header_above(c(" " = 1, "Outcome" = 5))%>%
  footnote(general = "* p<0.1, ** p<0.05, *** p<0.01; standard errors clustered at school level")%>%
  kable_styling(latex_options = c("hold_position"))%>% 
  kableExtra::save_kable(paste0(results_dir, "/school_results_educ_2mi.tex"))


isat_plot <- ggplot(es_results_ed %>% filter(outcome == "ISAT_composite"),
                    aes(x = e, y = ATT)) +
  ylab("ISAT achievement")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
isat_plot

isat_plot + ggtitle("ISAT impacts of ash borer infestation by event time")
ggsave(path = fig_dir, filename = "es_school_isat_2mi.png", width = 7, height = 5)

all_tests_plot <- ggplot(es_results_ed %>% filter(outcome == "all_tests"),
                    aes(x = e, y = ATT)) +
  ylab("All reported tests achievement")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
all_tests_plot

all_tests_plot + ggtitle("All test impacts of ash borer infestation by event time")
ggsave(path = fig_dir, filename = "es_school_all_tests_2mi.png", width = 7, height = 5)


canopy_plot <- ggplot(es_results_ed %>% filter(outcome == "canopy"),
                         aes(x = e, y = ATT)) +
  ylab("Mean canopy cover probability")+ xlab("Years since infestation detection")+
  geom_ribbon(aes(ymin= ATT - crit*se, ymax=ATT + crit*se), fill = palette$light_grey, color = palette$light_grey, alpha=1)+
  geom_line() +
  geom_point(shape = 21, fill = palette$dark)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_classic()
canopy_plot

canopy_plot + ggtitle("School vicinity canopy cover impacts of ash borer infestation by event time")
ggsave(path = fig_dir, filename = "es_school_canopy_2mi.png", width = 7, height = 5)


ggarrange(isat_plot, all_tests_plot, canopy_plot, ncol = 1, nrow = 3,
          labels = c("A", "B", "C"))
ggsave(path = fig_dir, filename = "es_school_trio_2mi.png", width = 5, height = 10)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### TWFE results for heterogeneity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(fixest)

### Education outcomes

twfe_tests <- feols(all_tests ~ treated * lowinc_pct
                    + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                    | school_ID + year, 
                    data = panel)
summary(twfe_tests)


twfe_isat <- feols(ISAT_composite ~ treated * lowinc_pct
                   + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                   | school_ID + year, 
                   data = panel)
summary(twfe_isat)

twfe_attend <- feols(all_attend ~ treated * lowinc_pct
                     + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                     | school_ID + year, 
                     data = panel)
summary(twfe_attend)

### Tree cover outcomes
twfe_canopy <- feols(canopy ~ treated * lowinc_pct
                     + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                     | school_ID + year, 
                     data = panel)
summary(twfe_canopy)

### ash intensity impacts
twfe_ash <- feols(canopy ~ treated * pct_trees_in_area
                     + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                     | school_ID + year, 
                     data = panel)
summary(twfe_ash)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By subject, grade, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test <- c("ISAT_")
groups <- c(
  "low income_", "non-low income_")
grades <- c("gr3", "gr4", "gr5", "gr6", "gr7", "gr8")
categ <- c(" read school meets", " math school meets", 
" read school academic warning", " math school academic warning",
" read school below", " math school below", 
" read school exceeds", " math school exceeds"
)
tests <- as.vector(outer(as.vector(outer(test, groups, paste0)), as.vector(outer(grades, categ, paste0)), paste0))

ovr_results_isat <- data.frame()
for(k in tests){
  print(k)
  
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
                  xformla = xformula,
                  data = this_panel
  )

  ovr <- aggte(attgt, type = "simple", na.rm = T)

  ovr_results_isat <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se
                                 , "group" = group, "grade" = grade, "subject" = subject, "benchmark" = benchmark
                                 )%>%
    rbind(ovr_results_isat)
  
}

spec_results_warning <- ovr_results_isat %>%
  filter(grepl("warning", benchmark, fixed = TRUE))%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, benchmark)) %>%
  as.data.frame()

spec_results_below <- ovr_results_isat %>%
  filter(grepl("below", benchmark, fixed = TRUE))%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, benchmark)) %>%
  as.data.frame()

spec_results_meets <- ovr_results_isat %>%
  filter(grepl("meets", benchmark, fixed = TRUE))%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, benchmark)) %>%
  as.data.frame()

spec_results_exceeds <- ovr_results_isat %>%
  filter(grepl("exceeds", benchmark, fixed = TRUE))%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome, benchmark)) %>%
  as.data.frame()




labels <- list("Group" = c("Non low-income", "Low-income"),
               "Subject" = c("Math", "Reading"),
               "Grade" = c("8th", "7th","6th", "5th", "4th", "3rd")
)


#Create the plot

png(paste0(fig_dir,"/schart_isat_warning_2mi.png"), width = 8, height = 5, units = "in", res = 300)

par(oma=c(1,0,1,1))
schart(spec_results_warning, ci=c(0.95), ylab="ATT (percentage of students)", labels = labels,
       highlight = seq(from = 2, to = 24, by = 2),
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c("white","grey","white", palette$blue),
       col.est=c(palette$dark, palette$blue)
) 
# text(x=7 , y=5, "Below", col=palette$black, font=1)
dev.off()

png(paste0(fig_dir,"/schart_isat_below_2mi.png"), width = 8, height = 5, units = "in", res = 300)

schart(spec_results_below, ci=c(0.95), ylab="ATT (percentage of students)", labels = labels,
       highlight = seq(from = 2, to = 24, by = 2),
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c("white","grey","white", palette$blue),
       col.est=c(palette$dark, palette$blue)
) 
dev.off()

png(paste0(fig_dir,"/schart_isat_meets_2mi.png"), width = 8, height = 5, units = "in", res = 300)
schart(spec_results_meets, ci=c(0.95), ylab="ATT (percentage of students)", labels = labels,
       highlight = seq(from = 2, to = 24, by = 2),
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c("white","grey","white", palette$blue),
       col.est=c(palette$dark, palette$blue)
) 
dev.off()

png(paste0(fig_dir,"/schart_isat_exceeds_2mi.png"), width = 8, height = 5, units = "in", res = 300)
schart(spec_results_exceeds, ci=c(0.95), ylab="ATT (percentage of students)", labels = labels,
       highlight = seq(from = 2, to = 24, by = 2),
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c("white","grey","white", palette$blue),
       col.est=c(palette$dark, palette$blue)
) 
dev.off()

library(png)
library(grid)
library(gridExtra)

plot1 <- readPNG(paste0(fig_dir,'/schart_isat_warning_2mi.png'))
plot2 <- readPNG(paste0(fig_dir,'/schart_isat_below_2mi.png'))
plot3 <- readPNG(paste0(fig_dir,'/schart_isat_meets_2mi.png'))
plot4 <- readPNG(paste0(fig_dir,'/schart_isat_exceeds_2mi.png'))


ggarrange(rasterGrob(plot1),rasterGrob(plot2), rasterGrob(plot3), rasterGrob(plot4), 
          nrow= 2, ncol = 2,
          labels = c("A", "B", "C", "D"))
ggsave(paste0(fig_dir,'/schart_quad_2mi.png'), width = 8, height = 5.25)



spec_results_all <- ovr_results_isat %>%
  pivot_wider(names_from = benchmark, values_from = benchmark, values_fn = list(benchmark = ~TRUE), values_fill = list(benchmark = 0))%>%
  arrange(`academic warning`, below, meets, exceeds)%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
  pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  mutate(math = ifelse(reading == TRUE, FALSE, TRUE))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))%>%
  select(-c(outcome)) %>%
  as.data.frame()

labels <- list(
               "Benchmark" = c("exceeds", "below", "academic warning", "meets"),
               "Group" = c("Non low-income", "Low-income"),
               "Subject" = c("Math", "Reading"),
               "Grade" = c("8th", "7th","6th", "5th", "4th", "3rd")
)

schart(spec_results_all, ci=c(0.95), ylab="ATT (percentage of students)", labels = labels,
       highlight = seq(from = 2, to = 96, by = 2),
       col.dot=c(palette$dark,"grey","white", palette$blue),
       bg.dot=c("white","grey","white", palette$blue),
       col.est=c(palette$dark, palette$blue)
) 

library(fixest)
library(tidyverse)
library(here)
library(did)
library(Hmisc)

select <- dplyr::select
source(here::here('analysis', 'schart.R'))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Read in tree and illinois spatial data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_dir <- "C:/Users/garci/Dropbox/eab_chicago_data"
results_dir <- here()
eab_panel_school <- readRDS(paste0(file_dir, "/output/eab_panel_school3km.rds"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### DID Tree cover analysis
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
panel <- eab_panel_school %>%
  rename(white_pct = "school - white pct",
         black_pct = "school - black pct",
         hispanic_pct = "school - hispanic pct",
         asian_pct = "school - asian pct",
         lowinc_pct = "low-income school pct",
         cov_white_pct = "cov_school - white pct",
         cov_black_pct = "cov_school - black pct",
         cov_hispanic_pct = "cov_school - hispanic pct",
         cov_asian_pct = "cov_school - asian pct",
         cov_lowinc_pct = "cov_low-income school pct",
         cov_lep_pct = "cov_l.e.p. school pct",
         cov_truants_pct = "cov_chronic truants rate school pct",
         cov_all_attend = "cov_all_attendance rate school pct",
         cov_enrollment = "cov_school total enrollment"
  )%>%
  mutate(low_income_attend = `low income_attendance rate school pct`,
         all_attend = `all_attendance rate school pct`,
         enrollment = `school total enrollment`,
         net_gain = gain - loss,
         treated = ifelse(first_exposed > 0 & year >= first_exposed, 1, 0))%>%
  mutate_at(vars(RCDS, year, first_exposed,
                 all_tests,
                 ISAT_composite,
                 low_income_attend,
                 all_attend,
                 white_pct, black_pct, hispanic_pct, asian_pct, lowinc_pct, enrollment
                 ),
            as.numeric)%>%
  group_by(RCDS, year)%>%
  slice_head()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### TWFE results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(fixest)

### Tree cover outcomes
twfe_canopy <- feols(canopy ~ treated 
                     + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                     | RCDS + year, 
                    data = panel)
summary(twfe_canopy)

twfe_loss <- feols(loss ~ treated 
                   + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                   | RCDS + year, 
                   data = panel)
summary(twfe_loss)

twfe_gain <- feols(gain ~ treated 
                   + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                   | RCDS + year, 
                   data = panel)
summary(twfe_gain)

twfe_net <- feols(net_gain ~ treated 
                  + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                  | RCDS + year, 
                  data = panel)
summary(twfe_net)

### Education outcomes

twfe_tests <- feols(all_tests ~ treated 
                    + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                    | RCDS + year, 
                    data = panel)
summary(twfe_tests)


twfe_isat <- feols(ISAT_composite ~ treated 
                   + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                   | RCDS + year, 
                   data = panel)
summary(twfe_isat)

twfe_attend <- feols(all_attend ~ treated 
                     + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                     | RCDS + year, 
                     data = panel)
summary(twfe_attend)

twfe_lowinc_attend <- feols(low_income_attend ~ treated 
                            + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct+ enrollment
                            | RCDS + year, 
                            data = panel)
summary(twfe_lowinc_attend)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Heterogeneity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Tree cover outcomes
twfe_canopy <- feols(canopy ~ treated * log(lowinc_pct)+ 
                       + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                     | RCDS + year, 
                     data = panel)
summary(twfe_canopy)

twfe_net <- feols(net_gain ~ treated * log(lowinc_pct) + 
                    + white_pct + black_pct + hispanic_pct + asian_pct + lowinc_pct + enrollment
                  | RCDS + year, 
                  data = panel)
summary(twfe_net)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Callaway and Sant'anna estimates
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outcomevars <- c("canopy", "loss", "gain", "net_gain",
                 "all_tests", "ISAT_composite", "all_attend",
                 "white_attendance rate school pct",  "black_attendance rate school pct", "hispanic_attendance rate school pct",
                 "low income_attendance rate school pct")

cov_names <- c("cov_white_pct", "cov_black_pct", "cov_hispanic_pct", "cov_asian_pct", "cov_lowinc_pct", "cov_lep_pct"
, "cov_enrollment")

xformula <- as.formula(paste("~ ", paste(cov_names, collapse = " + ")))

ovr_results_ed <- data.frame()
es_results_ed <- data.frame()
for(k in outcomevars){
  print(k)
  panel <- panel %>%
    mutate_at(vars(k, year), as.numeric)%>%
    filter(between(year, 2000, 2014))
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "RCDS",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  xformla = xformula,
                  data = panel
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se)%>%
    rbind(ovr_results_ed)
  
  es <- aggte(attgt, type = "dynamic", na.rm = T)
  
  es_results_ed <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "simul_crit" = es$crit.val.egt)%>%
    rbind(es_results_ed)
  
}

ovr_results <- ovr_results %>%
  mutate(crit.val = ATT/se)


all_tests_plot <- ggplot(es_results_ed %>% filter(outcome == "all_tests",
                                                  between(e, -5, 6)),
                         aes(x = e, y = ATT)) +
  geom_line() +
  geom_ribbon(aes(ymin= ATT - simul_crit*se, ymax=ATT + simul_crit*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
all_tests_plot

isat_plot <- ggplot(es_results_ed %>% filter(outcome == "ISAT_composite",
                                                  between(e, -5, 6)),
                         aes(x = e, y = ATT)) +
  geom_line() +
  geom_ribbon(aes(ymin= ATT - simul_crit*se, ymax=ATT + simul_crit*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
isat_plot

lowinc_attend_plot <- ggplot(es_results_ed %>% filter(outcome == "low income_attendance rate school pct",
                                                      between(e, -5, 6)),
                         aes(x = e, y = ATT)) +
  geom_line() +
  geom_ribbon(aes(ymin= ATT - simul_crit*se, ymax=ATT + simul_crit*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
lowinc_attend_plot

all_attend_plot <- ggplot(es_results_ed %>% filter(outcome == "all_attend",
                                                      between(e, -5, 6)),
                             aes(x = e, y = ATT)) +
  geom_line() +
  geom_ribbon(aes(ymin= ATT - simul_crit*se, ymax=ATT + simul_crit*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
all_attend_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Instrumental variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


outcomevars <- c("all_attend", "all_tests", "ISAT_composite", "low_income_attend")
iv_results <- data.frame()

for(k in outcomevars){
  
  
  x <- as.formula(paste(k,  '~ 1 | RCDS + year | net_gain ~ treated'))
  
  print(x)
  
  iv_est <- feols(x, panel)
  coeff <- iv_est$coefficients
  se <- iv_est$se
  fstat <- fitstat(iv_est, "ivf")[[1]]$stat
  
  
  iv_results <- data.frame("outcome" = k,
                           "coeff" = coeff,
                           "se" = se,
                           "Fstat" = fstat)%>%
    rbind(iv_results)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### By subject, grade, etc.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test <- c("ISAT_")
groups <- c("all_", 
  "low income_", "non-low income_")
grades <- c("gr3", "gr4", "gr5", "gr6", "gr7", "gr8")
categ <- c(#" read school meets", " math school meets", 
" read school academic warning", " math school academic warning")
tests <- as.vector(outer(as.vector(outer(test, groups, paste0)), as.vector(outer(grades, categ, paste0)), paste0))

ovr_results_isat <- data.frame()
for(k in tests){
  print(k)
  
  grade = gsub('gr','', strsplit(strsplit(k, split = "_")[[1]][3], " ")[[1]][1])
  group = strsplit(k, split = "_")[[1]][2]
  subject = ifelse(grepl("math", k, fixed = TRUE), "math", "reading")
  benchmark = ifelse(grepl("meets", k, fixed = TRUE), "meets", "academic warning")
  
  this_panel <- panel %>%
    mutate_at(vars(k), as.numeric)
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "RCDS",
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
  
  # ovr_results_isat <- data.frame("ATT" = twfe$coefficients[1], "se" = twfe$se[1],
  #                                "group" = group, "grade" = grade, "subject" = subject, "benchmark" = benchmark)%>%
  #   rbind(ovr_results_isat)
}


spec_results_warning <- ovr_results_isat %>%
 # filter(subject == "math")%>%
 # pivot_wider(names_from = benchmark, values_from = benchmark, values_fn = list(benchmark = ~TRUE), values_fill = list(benchmark = 0))%>%
  pivot_wider(names_from = group, values_from = group, values_fn = list(group = ~TRUE), values_fill = list(group = 0))%>%
 # pivot_wider(names_from = subject, values_from = subject, values_fn = list(subject = ~TRUE), values_fill = list(subject = 0))%>%
  pivot_wider(names_from = grade, values_from = grade, values_fn = list(grade = ~TRUE), values_fill = list(grade = 0))


labels <- list(#"Benchmark" = c("Meets", "Academic warning"),
               "Group" = c("All", "Low-income", "Non low-income"),
               "Subject" = c("Math", "Reading"),
               "Grade" = c("8th", "7th","6th", "5th", "4th", "3rd")
)

my_palette  <- list("black" = "#000000", 
                    "green"  = "#009E73", 
                    "blue" =  "#0072B2", 
                    "red" = "#D55E00")
#Create the plot
par(oma=c(1,0,1,1))

spec_results <- spec_results %>% arrange(meets, desc(ATT))%>%
  as.data.frame()


schart(spec_results, ci=c(.9,.95), ylab="ATT",
       col.dot=c(my_palette$black,"grey","white", my_palette$red),
       bg.dot=c("white","grey","white", my_palette$red),
       col.est=c(my_palette$black, my_palette$red)
) 


ovr_results_isat_cs <- data.frame()
es_results_isat <- data.frame()
for(k in tests){
  print(k)
  
  grade = gsub('gr','', strsplit(strsplit(k, split = "_")[[1]][3], " ")[[1]][1])
  group = strsplit(k, split = "_")[[1]][2]
  subject = ifelse(grepl("math", k, fixed = TRUE), "math", "reading")
  benchmark = ifelse(grepl("meets", k, fixed = TRUE), "meets", "academic warning")
  
  panel <- panel %>%
    mutate_at(vars(k), as.numeric)
  
  attgt <- att_gt(yname = k,
                  tname = "year",
                  idname = "ID",
                  gname = "first_exposed",
                  control_group = "notyettreated",
                  xformla = xformula,
                  data = panel
  )
  
  ovr <- aggte(attgt, type = "simple", na.rm = T)
  
  ovr_results_isat_cs <- data.frame("ATT" = ovr$overall.att, "se" = ovr$overall.se,
                                    "group" = group, "grade" = grade, "subject" = subject, "benchmark" = benchmark)%>%
    rbind(ovr_results_isat_cs)
  
  es <- aggte(attgt, type = "dynamic", na.rm = T)
  
  es_results_isat <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "simul_crit" = es$crit.val.egt)%>%
    rbind(es_results_isat)
  
}



library(fixest)
library(tidyverse)
library(here)
library(did)
library(Hmisc)
library(lfe)

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

file_dir <- "C:/Users/garci/Dropbox/eab_chicago_data"

cleaned_dir <- here::here("cleaned")


results_dir <- here::here("analysis", "results")

fig_dir <- here::here("analysis", "figs")

eab_panel_school <- readRDS(paste0(cleaned_dir, "/eab_panel_school2mi.rds"))

panel <- eab_panel_school %>%
  mutate_at(vars(year, first_exposed), as.numeric) %>%
  mutate(e_time = ifelse(first_exposed > 0, year - first_exposed, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Instrumental variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


outcomevars <- c("all_attend", "all_tests", "ISAT_composite", "low_income_attend")



covars_list <- c("enrollment", 
                 "white_pct", "black_pct", "hispanic_pct", "asian_pct", "lowinc_pct")
covars <- paste(covars_list, collapse = " + ")

iv_results <- data.frame()
fitvar <- "canopy"

for(k in outcomevars){
  
  covars = 1
  stage1 <- as.formula(paste(fitvar,  '~ treated +', covars, ' | school_ID + year'))
  stage1_reg <- feols(stage1, panel)
  summary(stage1_reg)
  # stage1_reg$coefficients['treated']
  
  # x <- as.formula(paste(k,  '~ treated +', covars,  '| school_ID + year |', fitvar , '~ treated'))
  # print(x)
  # 
  # iv_est <- feols(x, panel)
  # coeff <- iv_est$coefficients['treated']
  # iv_est$se
  # se <- iv_est$se[paste0("fit_", fitvar)]
  # fstat <- fitstat(iv_est, "ivf")[[1]]$stat
  
  ivreg_form <- as.formula(paste(k,  '~', covars, '+ as.factor(school_ID) + as.factor(year) | ', fitvar , ' | treated'))
  ivreg <- ivreg(ivreg_form,
                 data = panel)
  summary(ivreg)
  
  
  lfe_form <- as.formula(paste(k,  ' ~ ', covars,  '| school_ID + year |', fitvar , '~ treated'))
  iv_lfe <- felm(lfe_form,
                 data = panel)
  summary(iv_lfe)
  # Kleibergen-Paap
  fstat <- waldtest(fe_est_felm$stage1, ~x3)
  
  iv_results <- data.frame("outcome" = k,
                           "coeff" = coeff,
                           "coef_firststage" = stage1_reg$coefficients['treated'],
                           "se" = se,
                           "Fstat" = fstat)%>%
    rbind(iv_results)
}

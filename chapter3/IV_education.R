library(fixest)
library(tidyverse)
library(here)
library(did)
library(Hmisc)
library(fixest)

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

cleaned_dir <- here::here("chapter3", "cleaned")


results_dir <- here::here("chapter3", "results")

fig_dir <- here::here("chapter3", "figs")

eab_panel_school <- readRDS(paste0(cleaned_dir, "/eab_panel_school2mi.rds"))

panel <- eab_panel_school %>%
  mutate_at(vars(year, first_exposed), as.numeric) %>%
  mutate(e_time = ifelse(first_exposed > 0, year - first_exposed, 0))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Instrumental variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


outcomevars <- c("all_attend", "all_tests", "ISAT_composite", "low_income_attend")
iv_results <- data.frame()

for(k in outcomevars){
  
  
  x <- as.formula(paste(k,  '~ 1 | school_ID + year | canopy ~ treated'))
  
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

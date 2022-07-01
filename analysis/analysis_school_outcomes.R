library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(here)
library(did)

select <- dplyr::select
source(here::here('analysis', 'schart.R'))

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Py.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))

tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)

eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(crs(tree_loss))%>%
  st_crop(tree_loss)

control_vars <- c("school - white pct", "school - black pct", "school - hispanic pct", "low-income school pct", "l.e.p. school pct")

reportcard_data <- readRDS("schools/reportcard/cleaned/reportcard_data.rds")

maxyear = 2005
for(i in 1:length(control_vars)){
  print(i)
  x <- control_vars[i]
  print(x)
  #reportcard_data <- cov_fcn(reportcard_data, i, 2005)
  df_temp <- reportcard_data %>%
    filter(year <= maxyear)%>%
    group_by(RCDS)%>%
    mutate_at(vars(x), ~ mean(as.numeric(.), na.rm = T))%>%
    select(RCDS, x) %>%
    slice_head()
  
  names(df_temp) <- c(names(df_temp)[1], paste0("cov_", i))
  
  reportcard_data <- inner_join(reportcard_data, df_temp, by = "RCDS")
  
}

cov_names <- paste0("cov_", seq(from = 1, to = length(control_vars), by = 1))


reportcard_loc <- read_sf("schools/reportcard/cleaned/reportcard_loc.shp")%>%
  st_transform(crs(tree_loss))%>%
  st_crop(tree_loss)

var_names <- c("all_attendance rate school pct",    
                    # "white_attendance rate school pct",  "black_attendance rate school pct", 
               "low income_attendance rate school pct",         
                    "dropout rate  school pct", "all_tests",                  
                    "ISAT_composite" ) 

# test <- c("ISAT_")
# groups <- c("all_")#, "low income_", "non-low income_")#, "white_", "black_", "hispanic_")
# grades <- c("gr3", "gr4", "gr5", "gr6", "gr7", "gr8")
# categ <- c(" read school below", " math school below", " read school academic warning", " math school academic warning")
# tests <- as.vector(outer(as.vector(outer(test, groups, paste0)), as.vector(outer(grades, categ, paste0)), paste0))

outcome_names <- var_names
# outcome_names <- c(var_names, tests)

buffer_list <- c(1.609344, 2.5, 5)
ovr_results <- data.frame()
es_results <- data.frame()

for(i in buffer_list){
  
  infestation_buffer <- st_buffer(eab_infestations, i*1000)
  
  school_exposure <- reportcard_loc %>%
    st_join(infestation_buffer)%>%
    group_by(RCDS)%>%
    mutate_at(vars(Year), as.numeric)%>%
    mutate(first_exposed = min(Year, na.rm = T))%>%
    slice_head()%>%
    mutate_at(vars(first_exposed), ~replace(., is.na(.), 0))
  
  school_exposure$geometry <- NULL
  
  panel_reportcard <- reportcard_data %>%
    inner_join(school_exposure, by = "RCDS")%>%
    group_by(RCDS, year)%>%
    slice_head()
  
  print(i)
  
  for(k in outcome_names){
    print(k)
    panel <- panel_reportcard %>%
      group_by(RCDS)%>%
      mutate(ID = cur_group_id())%>%
      mutate_at(vars(ID, k), as.numeric)
    
    attgt <- att_gt(yname = k,
                    tname = "year",
                    idname = "ID",
                    gname = "first_exposed",
                    control_group = "notyettreated",
                    xformla = as.formula(paste("~ ", paste(cov_names, collapse = " + "))),
                    data = panel
    )
    
    ovr <- aggte(attgt, type = "simple", na.rm = T)
    
    ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, "buffer" = i)%>%
      rbind(ovr_results)
    
    es <- aggte(attgt, type = "dynamic", na.rm = T)
    
    es_results <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "buffer" = i)%>%
      rbind(es_results)
    
  }
  
}

ovr_results <- ovr_results %>%
  mutate(crit.val = ATT/se)


all_tests_plot <- ggplot(es_results %>% filter(outcome == "all_tests", buffer == 2.5, 
                                        between(e, -8, 7)),
                  aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
all_tests_plot

ISAT_plot <- ggplot(es_results %>% filter(outcome == "ISAT_composite", buffer == 2.5, 
                                          between(e, -8, 7)),
                    aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
ISAT_plot

lowinc_attendance_plot <- ggplot(es_results %>% filter(outcome == "low income_attendance rate school pct", buffer == 2.5, 
                                          between(e, -8, 7)),
                    aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
lowinc_attendance_plot


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Specification chart to visualize results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buffer_size <- 2.5

school_buffer <- reportcard_loc %>%
  st_transform(crs(tree_loss))%>%
  st_crop(tree_loss) %>%
  st_buffer(buffer_size*1000)%>%
  select(RCDS, FacilityNa)


library(exactextractr)
min_year = 1995
max_year = 2015
year_list <- seq(from = min_year, to = max_year, by = 1)

for(i in year_list){
  
  this_tree_gain <- tree_gain
  this_tree_gain[this_tree_gain[] != i ] = 0
  this_tree_gain[this_tree_gain[] == i ] = 1
  
  new <- exact_extract(this_tree_gain, school_buffer, 'sum')
  school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  colnames(school_buffer)[ncol(school_buffer)] <- paste0("gain_", i)
  
  this_tree_loss <- tree_loss
  this_tree_loss[this_tree_loss[] != i ] = 0
  this_tree_loss[this_tree_loss[] == i ] = 1
  
  new <- exact_extract(this_tree_loss, school_buffer, 'sum')
  school_buffer[ , ncol(school_buffer) + 1] <- new      # Append new column
  colnames(school_buffer)[ncol(school_buffer)] <- paste0("loss_", i)
  
}

school_buffer$geometry = NULL







# select_results <- ovr_results %>%
#   separate(outcome, into = c("subject", "grade"), sep = "_")%>%
#   filter(buffer == 5000)%>%
#   arrange(as.numeric(grade), subject)%>%
#   mutate("TRUE" = TRUE,
#          "TRUE2" = TRUE
#   )%>%
#   pivot_wider(names_from = subject, values_from = "TRUE", values_fill = FALSE)%>%
#   pivot_wider(names_from = grade, values_from = "TRUE2", values_fill = FALSE)%>%
#   select(-c(buffer, "NA"))%>%
#   as.data.frame()
# 
# 
# labels <- list("Subject" = c("Math", "Reading", "Composite"),
#                "Grade" = c("3rd", "5th", "8th", "11th")
#                #, "Buffer" = c("3km", "5km")
# )
# 
# my_palette  <- list("black" = "#000000", 
#                     "green"  = "#009E73", 
#                     "blue" =  "#0072B2", 
#                     "red" = "#D55E00")
# 
# index.ci <- match(c("lowerci","upperci"), names(select_results))
# 
# ylim <- c(-4.25, 2)
# #Create the plot
# par(oma=c(1,0,1,1))
# 
# schart(select_results,labels, ylim = ylim, ci=c(.9,.95), ylab="ATT",
#        highlight=c(1, 3, 5, 7),
#        col.dot=c(my_palette$black,"grey","white", my_palette$red),
#        bg.dot=c("white","grey","white", my_palette$red),
#        col.est=c(my_palette$black, my_palette$red)
# ) 
# 
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ### All results
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 
# select_results <- ovr_results %>%
#   separate(outcome, into = c("subject", "grade"), sep = "_")%>%
#   arrange(desc(buffer), as.numeric(grade), subject)%>%
#   mutate("TRUE" = TRUE,
#          "TRUE2" = TRUE
#   )%>%
#   pivot_wider(names_from = subject, values_from = "TRUE", values_fill = FALSE)%>%
#   pivot_wider(names_from = grade, values_from = "TRUE2", values_fill = FALSE)%>%
#   select(-c(buffer, "NA"))%>%
#   as.data.frame()
# 
# 
# labels <- list("Subject" = c("Math", "Reading", "Composite"),
#                "Grade" = c("3rd", "5th", "8th", "11th")
# )
# 
# my_palette  <- list("black" = "#000000", 
#                     "green"  = "#009E73", 
#                     "blue" =  "#0072B2", 
#                     "red" = "#D55E00")
# 
# index.ci <- match(c("lowerci","upperci"), names(select_results))
# 
# ylim <- c(-4.25, 2)
# #Create the plot
# par(oma=c(1,0,1,1))
# 
# schart(select_results,labels, ylim = ylim, ci=c(.9,.95), ylab="ATT",
#        n = 9,
#        highlight=c(1, 3, 5, 7, 10, 12, 14, 16),
#        col.dot=c(my_palette$black,"grey","white", my_palette$red),
#        bg.dot=c("white","grey","white", my_palette$red),
#        col.est=c(my_palette$black, my_palette$red)
# ) 
# abline(v=10, lty="dashed")
# text(x=2
#      , y=1.75, "5km buffer", col="black", font=2, cex = 1)
# text(x=12
#      , y=1.75, "3km buffer", col="black", font=2, cex = 1)
# 
# 
# 
# 

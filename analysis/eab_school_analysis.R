library(sf)
library(raster)
library(readxl)
library(tidyverse)
library(here)

setwd("C:/Users/garci/Documents/eab_chicago")
source(here::here('analysis', 'schart.R'))

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

illinois.shp <- read_sf("administrative/IL_State/IL_BNDY_State_Ln.shp")%>%
  st_transform(crs(raster("tree_data/tree_loss_year.tif")))
tree_loss <- raster("tree_data/tree_loss_year.tif")%>%
  crop(illinois.shp)
tree_gain <- raster("tree_data/tree_gain_year.tif")%>%
  crop(illinois.shp)
impervious_increase <- raster("tree_data/IS_change_year.tif")%>%
  crop(illinois.shp)


eab_infestations <- read_sf("eeb_infestations/eeb_address_geocode.shp")%>%
  dplyr::select(Date, City, Match_addr, City_1, Address)%>%
  separate(Date, c("Year", NA, NA))%>%
  st_transform(crs(tree_loss))


school_test_scores <- readRDS("schools/school_test_scores.rds")


buffer_size <- 5000

infestation_buffer <- st_buffer(eab_infestations, buffer_size)

test_scores_infestation <- school_test_scores %>%
  st_join(infestation_buffer)%>%
  group_by(my_id, year)%>%
  mutate_at(vars(Year, year, my_id), as.numeric)%>%
  mutate(first_exposed = min(Year, na.rm = T),
         treated = ifelse(year > first_exposed & year > 0, 1, 0))%>%
  slice_head()%>%
  mutate_at(vars(first_exposed), ~replace(., is.na(.), 0))

test_scores_infestation$geometry <- NULL


library(did)
set.seed(0930)
loss_attgt <- att_gt(yname = "Math_11",
                     tname = "year",
                     idname = "my_id",
                     gname = "first_exposed",
                     control_group = "notyettreated",
                     data = test_scores_infestation
)
loss_ovr <- aggte(loss_attgt, type = "simple")
summary(loss_ovr)
loss_es <- aggte(loss_attgt, type = "dynamic")
ggdid(loss_es)

library(fixest)
twfe_loss <- feols(Math_3 ~ treated | year + my_id, data = test_scores_infestation)
summary(twfe_loss)


variable_names <- c("Read_3", "Read_5", "Read_8", "Read_11", "Math_3", "Math_5", "Math_8", "Math_11", "ISAT")
buffer_list <- c(3000, 5000)
ovr_results <- data.frame()
es_results <- data.frame()


for(i in buffer_list){
  
  infestation_buffer <- st_buffer(eab_infestations, i)
  
  test_scores_infestation <- school_test_scores %>%
    st_join(infestation_buffer)%>%
    group_by(my_id, year)%>%
    mutate_at(vars(Year, year, my_id), as.numeric)%>%
    mutate(first_exposed = min(Year, na.rm = T),
           treated = ifelse(year >= first_exposed & year > 0, 1, 0))%>%
    slice_head()%>%
    mutate_at(vars(first_exposed), ~replace(., is.na(.), 0))
    
  test_scores_infestation$geometry <- NULL
  
  for(k in variable_names){
    
    attgt <- att_gt(yname = k,
                     tname = "year",
                     idname = "my_id",
                     gname = "first_exposed",
                     control_group = "notyettreated",
                     data = test_scores_infestation
                    )
    
    ovr <- aggte(attgt, type = "simple")
    
    ovr_results <- data.frame("outcome" = k, "ATT" = ovr$overall.att, "se" = ovr$overall.se, "buffer" = i)%>%
      rbind(ovr_results)
    
    es <- aggte(attgt, type = "dynamic")
    
    es_results <- data.frame("outcome" = k, "ATT" = es$att.egt, "e" = es$egt, "se" = es$se.egt, "buffer" = i)%>%
      rbind(es_results)
    
    }

}


ex_plot <- ggplot(es_results %>% filter(outcome == "Math_11", buffer == 5000, between(e, -9, 7)), aes(x = e, y = ATT)) + 
  geom_line() + 
  geom_ribbon(aes(ymin= ATT - 1.96*se, ymax=ATT + 1.96*se), alpha=0.2)+
  geom_vline(xintercept = -0.5, linetype = "dashed")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()
ex_plot

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Specification chart to visualize results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_results <- ovr_results %>%
  separate(outcome, into = c("subject", "grade"), sep = "_")%>%
  filter(buffer == 5000)%>%
  arrange(as.numeric(grade), subject)%>%
  mutate("TRUE" = TRUE,
         "TRUE2" = TRUE
  )%>%
  pivot_wider(names_from = subject, values_from = "TRUE", values_fill = FALSE)%>%
  pivot_wider(names_from = grade, values_from = "TRUE2", values_fill = FALSE)%>%
  select(-c(buffer, "NA"))%>%
  as.data.frame()


labels <- list("Subject" = c("Math", "Reading", "Composite"),
               "Grade" = c("3rd", "5th", "8th", "11th")
               #, "Buffer" = c("3km", "5km")
)

my_palette  <- list("black" = "#000000", 
                    "green"  = "#009E73", 
                    "blue" =  "#0072B2", 
                    "red" = "#D55E00")

index.ci <- match(c("lowerci","upperci"), names(select_results))

ylim <- c(-4.25, 2)
#Create the plot
par(oma=c(1,0,1,1))

schart(select_results,labels, ylim = ylim, ci=c(.9,.95), ylab="ATT",
       highlight=c(1, 3, 5, 7),
       col.dot=c(my_palette$black,"grey","white", my_palette$red),
       bg.dot=c("white","grey","white", my_palette$red),
       col.est=c(my_palette$black, my_palette$red)
) 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### All results
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_results <- ovr_results %>%
  separate(outcome, into = c("subject", "grade"), sep = "_")%>%
  arrange(desc(buffer), as.numeric(grade), subject)%>%
  mutate("TRUE" = TRUE,
         "TRUE2" = TRUE
  )%>%
  pivot_wider(names_from = subject, values_from = "TRUE", values_fill = FALSE)%>%
  pivot_wider(names_from = grade, values_from = "TRUE2", values_fill = FALSE)%>%
  select(-c(buffer, "NA"))%>%
  as.data.frame()


labels <- list("Subject" = c("Math", "Reading", "Composite"),
               "Grade" = c("3rd", "5th", "8th", "11th")
)

my_palette  <- list("black" = "#000000", 
                    "green"  = "#009E73", 
                    "blue" =  "#0072B2", 
                    "red" = "#D55E00")

index.ci <- match(c("lowerci","upperci"), names(select_results))

ylim <- c(-4.25, 2)
#Create the plot
par(oma=c(1,0,1,1))

schart(select_results,labels, ylim = ylim, ci=c(.9,.95), ylab="ATT",
       n = 9,
       highlight=c(1, 3, 5, 7, 10, 12, 14, 16),
       col.dot=c(my_palette$black,"grey","white", my_palette$red),
       bg.dot=c("white","grey","white", my_palette$red),
       col.est=c(my_palette$black, my_palette$red)
) 
abline(v=10, lty="dashed")
text(x=2
     , y=1.75, "5km buffer", col="black", font=2, cex = 1)
text(x=12
     , y=1.75, "3km buffer", col="black", font=2, cex = 1)





library(data.table)
library(tidyverse)
library(readxl)

setwd("C:/Users/garci/Dropbox/eab_chicago_data")

var_clean <- function(x){
  
  x <- tolower(gsub("%", "pct", x))
  x <- gsub("#", "num", x)
  
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

substrLeft <- function(x, n){
  substr(x, 0, n)
}


select <- dplyr::select

rc_nums <- c("03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14")
return_list = list()
return_data = data.frame()
for(i in rc_nums){
this_folder <- paste0("schools/reportcard/rc", i, sep = "")
this_data <- fread(list.files(path = this_folder, recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE), colClasses = c("character"), data.table = FALSE)

colnames <- read_excel(list.files(path = this_folder, recursive = TRUE,
                                  pattern = "\\.xls", 
                                  full.names = TRUE), col_names = F)%>%
  rename(col_number = 1,
         test = 2,
         group = 3,
         var = 6)%>%
  mutate(var = var_clean(var),
         group = tolower(group))%>%
  drop_na(var)%>%
  unite("new_var", test, group, var, na.rm = TRUE)%>%
  select(col_number, new_var)

sequence <- colnames$col_number

full_seq <- 1:ncol(this_data)
drop <- full_seq[!full_seq %in% sequence]

this_data <- this_data %>%
  select(-c(`drop`))

colnames <- colnames %>%
arrange(as.numeric(col_number))

colnames(this_data) <- colnames$new_var

year = as.numeric(paste0("20", i))

this_data <- this_data %>%
  mutate(year = year)

composite_string = paste0("composite_", year," school")

if (any(grepl("r-c-d-s", colnames(this_data)))) {
  this_data <- this_data %>%
    rename(RCDS = 1)
    
} else {
  
  this_data <- this_data %>%
    rename(RCDTS = 1)%>%
    mutate(RCDS = paste0(substrLeft(RCDTS, 9), substrRight(RCDTS, 4)))
}



return_data <- this_data %>%
select(RCDS, `school name`, city, county, year,
        `school - white pct`, `school - black pct`,
       `school - hispanic pct`, `school - asian pct`, `school total enrollment`, `low-income school pct`, `l.e.p. school pct`,
      `all_attendance rate school pct`, `male_attendance rate school pct`, `female_attendance rate school pct`,
       `white_attendance rate school pct`, `black_attendance rate school pct`, `hispanic_attendance rate school pct`, `asian_attendance rate school pct`,
      `low income_attendance rate school pct`, `chronic truants rate school pct`, `dropout rate  school pct`,
      `act comp school`, contains(composite_string), 
      contains("math school below"), contains("read school below"), 
      contains("math school academic"), contains("read school academic"), 
      -c(contains("IMAGE"), contains("IAA"), contains("GRADE"))  
      )%>%
  rename_at(vars(contains(paste0("ISAT_", year))), ~"ISAT_composite")%>%
  rename_at(vars(contains(paste0("PSAE_", year))), ~"PSAE_composite")%>%
  rename_at(vars(contains("ALL TESTS")), ~"all_tests")%>%
  bind_rows(return_data)


}

reportcard_data <- return_data
library(rio)
export(reportcard_data, "schools/reportcard/cleaned/reportcard_data.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############# Linking schools with geocoded locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(sf)
  
school_loc <- read_sf("schools/geocoded_schools/school_locations.shp")%>%
  select(CountyName:NCES_ID)%>%
  rename(RCD = 3) %>%
  mutate(County = tolower(CountyName),
         RCDS = paste0(RCD, School))

reportcard_loc <- reportcard_data %>%
  group_by(RCDS)%>%
  slice_head()%>%
  select(RCDS)%>%
  inner_join(school_loc, by = "RCDS")

st_write(reportcard_loc, "schools/reportcard/cleaned/reportcard_loc.shp")

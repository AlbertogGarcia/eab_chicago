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
    select(RCDS, `district name`, county, year,
           `district - white pct`, `district - black pct`,
           `district - hispanic pct`, `district - asian pct`, `district total enrollment`, `low-income district pct`, `l.e.p. district pct`,
           `all_attendance rate district pct`, `male_attendance rate district pct`, `female_attendance rate district pct`,
           `white_attendance rate district pct`, `black_attendance rate district pct`, `hispanic_attendance rate district pct`, `asian_attendance rate district pct`,
           `low income_attendance rate district pct`, `chronic truants rate district pct`, `dropout rate district pct`,
           `act comp district`, contains(composite_string), 
           contains("math district below"), contains("read district below"), 
           contains("math district academic"), contains("read district academic"), 
           -c(contains("IMAGE"), contains("IAA"), contains("GRADE"))  
    )%>%
    rename_at(vars(contains(paste0("ISAT_", year))), ~"ISAT_composite_district")%>%
    rename_at(vars(contains(paste0("PSAE_", year))), ~"PSAE_composite_district")%>%
    rename_at(vars(contains("ALL TESTS")), ~"all_tests_district")%>%
    bind_rows(return_data)%>%
    mutate(district_num = substrRight(substrLeft(RCDS, 9), 4)
           )%>%
    group_by(district_num)%>%
    slice_head()%>%
    select(district_num, everything())
  
  
}

test <- return_data[, 1:20]
reportcard_data <- return_data
library(rio)
export(reportcard_data, "schools/reportcard/cleaned/reportcard_district_data.rds")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############# Linking schools with geocoded locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(sf)

district_loc <- read_sf("schools/SCHOOLDISTRICT_SY1314_TL15/schooldistrict_sy1314_tl15.shp")%>%
  filter(STATEFP == 17)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
############# Linking schools with geocoded locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library(sf)

district_office_loc <- read_sf("schools/geocoded_schools/school_locations.shp")%>%
  select(CountyName:NCES_ID)%>%
  rename(RCD = 3) %>%
  mutate(district_num = substrRight(RCD, 4),
         County = tolower(CountyName),
         RCDS = paste0(RCD, School))%>%
  filter(RecType == "Dist")

reportcard_loc <- reportcard_data %>%
  group_by(district_num)%>%
  slice_head()%>%
  select(district_num)%>%
  inner_join(district_office_loc, by = "RCDS")

st_write(reportcard_loc, "schools/reportcard/cleaned/reportcard_district_loc.shp")

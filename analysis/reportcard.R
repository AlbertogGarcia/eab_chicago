library(data.table)
library(tidyverse)
library(readxl)

setwd("C:/Users/agarcia/Dropbox/eab_chicago_data")

var_clean <- function(x){
  
  x <- tolower(gsub("%", "pct", x))
  x <- gsub("#", "num", x)
  
}

rc_nums <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11","12", "13", "14", "15")

for(i in rc_nums){
this_folder <- paste0("schools/reportcard/rc", i, sep = "")
this_data <- fread(list.files(path = this_folder, recursive = TRUE,
                            pattern = "\\.txt$", 
                            full.names = TRUE))#fread(paste0(this_folder, "/rc", i, ".txt", sep = ""))

colnames <- read_excel(list.files(path = this_folder, recursive = TRUE,
                                  pattern = "\\.xls$", 
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

this_data <- this_data %>%
  mutate(year = as.numeric(paste0("20", i)))

assign(paste0("reportcard_", i), this_data)

}

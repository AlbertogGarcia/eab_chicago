library(data.table)
library(tidyverse)
library(readxl)

setwd("C:/Users/agarcia/Dropbox/eab_chicago_data")

var_clean <- function(x){
  
  x <- tolower(gsub("%", "pct", x))
  x <- gsub("#", "num", x)
  
}

test <- fread("schools/reportcard/rc10/rc10.txt")
colnames <- read_excel("schools/reportcard/rc10/RC10_layout.xls", col_names = F)%>%
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

full_seq <- 1:ncol(test)
drop <- full_seq[!full_seq %in% sequence]

test <- test %>%
  select(-c(`drop`))

colnames <- colnames %>%
arrange(as.numeric(col_number))

colnames(test) <- colnames$new_var

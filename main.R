source("2.libraries.r")

findi <- function(){}
ffam <- function(){}

fnest <- function()

# Предварительный отбор и очистка
df1 <- readLines("1.Data/geny.26.12.2022.ged") %>% 
  as.data.table() %>% 
  setnames(old = ".", new = "value") %>% 
  .[, value := str_remove (value, "_")] %>% 
  .[, num   := str_extract(value, "^\\d\\s")] %>% 
  .[, value := str_remove (value, "^\\d\\s")] %>% 
  .[, code  := str_extract(value, "^[:upper:]{3,5}")] %>%
  .[, value := str_remove (value, "^[:upper:]{3,5}\\s")] %>% 
  .[str_detect(value, " INDI"), `:=`(pers =  str_remove(value, " INDI"), code = "INDI")] %>%
  .[str_detect(value, " FAM") , `:=`(pers =  str_remove(value, " FAM" ), code = "FAM")] %>%
  .[str_detect(value, " SUBM"), `:=`(pers =  str_remove(value, " FAM" ), code = "SUBM")] %>%
  .[, name := shift(value, type = "lead")] %>% 
  .[is.na(pers) | code == "FAM", name := NA] %>% 
  fill (pers, .direction = "down") %>% 
  .[!is.na(pers)] %>% 
  print()

# Справочник персон df_indi ####
df_indi <- df1 %>% 
  .[!is.na(name)] %>% 
  .[, .(pers, name, code)] 

# Справочник фамилий df_fam ####
  
df_fam <- df1 %>% 
  .[str_detect(pers, "F")] %>% 
  .[, .(fam = pers, code, value)] %>% 
  merge(df_indi %>% .[, .(pers, name)], by.x = "value", by.y = "pers", all.x = TRUE) %>% print()
  print()
  
df_indi <- df1 %>% 
  .[str_detect(pers, "F")] %>% 
  

nest_by(pers) %>% 
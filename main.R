source("2.libraries.r")

findi <- function(){}
ffam <- function(){}

fnest <- function()

# Предварительный отбор и очистка    df1           ####
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

# Справочник персон                  df_indi       ####
df_indi <- df1 %>% 
  .[!is.na(name)] %>%
  .[, .(pers, name, code)] %>% 
  .[!str_detect(pers, "SUBM")] %>% 
  print()

# Справочник семей                   df_fam        ####
df_fam <- df1 %>% 
  .[str_detect(pers, "F")] %>% 
  .[, .(fam = pers, code, value)] %>% 
  merge(df_indi %>% .[, .(pers, name)], by.x = "value", by.y = "pers", all.x = TRUE) %>%
  .[, surname := str_extract(name, "(?<=/).*(?=/)")] %>% 
  print()

# Связи муж-жена ####

df_marriage <- copy(df_fam) %>% 
  .[code %in% c("WIFE", "HUSB") ] %>% 
  .[, from := value[which(code == "HUSB")], by = fam ] %>% 
  .[, to   := value[which(code == "WIFE")], by = fam ] %>% 
  .[, .(from, to)] %>% 
  print()
  

# Узлы графа с уникальными семьями   df_fam_nodes  ####
df_fam_nodes <- copy(df_fam) %>% 
  dcast(... ~ code, value.var = "surname") %>% 
  .[, fam_name := paste0(HUSB[which(!is.na(HUSB))], " - ", WIFE[which(!is.na(WIFE))]), by = fam] %>% 
  .[, .(id = fam, label = fam_name)] %>% 
  distinct(id, label) %>% 
  .[, shape := c("triangle")] %>% 
  .[, size := 50] %>% 
  print()

# Узлы графа с уникальными персонами df_indi_nodes ####
df_indi_nodes <- copy(df_indi) %>% 
  .[, .(id = pers, label = name)] %>% 
  distinct(id, label) %>% 
  .[, shape := c("hexagon")] %>% 
  .[, size := 200] %>% 
  print()

# Соберем узлы вместе                nodes         ####

nodes <- rbind(df_fam_nodes, df_indi_nodes) %>% 
  .[, label := str_wrap(label, 10)] %>% 
  .[, color := "#e69c18"] %>% 
  .[id == "@I136067@", color := "#67b5cc"] %>% 
  print()

# Проставим связи                    edges         ####
edges <- copy(df_fam) %>% 
  .[code %in% c("WIFE", "HUSB"), `:=` (from = value, to = fam)] %>% 
  .[code %in% c("CHIL"),         `:=` (from = fam  , to = value)] %>%
  .[, .(from, to)] %>% 
  .[!is.na(from)] %>% 
  .[!str_detect(from, "F"), color := "#6fb3a7"] %>% 
  .[ str_detect(from, "F"), color := "#e5e4e4"] %>% 
  rbind(df_marriage %>% .[, color := '#8eac6d']) %>% 
  print()

#### Рисуем граф ####

visNetwork(nodes = nodes, edges = edges) %>% 
  visEdges(arrows = "middle", smooth = list(type = "discrete"),
           width = 30,
           font = list(face = "Panton", size = 30)) %>%
  visNodes(font = list(face = "Panton", size = 100) 
           # size = 200
           ) %>%
  visPhysics(enabled = TRUE,
             solver  = "forceAtlas2Based",
             forceAtlas2Based = list(
               gravitationalConstant = -5000,
               length = 80, 
               avoidOverlap = 0.5)) %>% 
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             # selectedBy = "group",
             collapse = list(enabled = TRUE, keepCoord = TRUE),
             nodesIdSelection = TRUE,
             autoResize = TRUE)


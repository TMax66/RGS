library(here)
library(tidyverse)
library(odbc)
library(RODBC)
library(DBI)
library(lubridate)
source(here("RepVirologia", "R", "sql.R"))

# Accesso al dbase access----

conn <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", 
                       Server = "dbprod02.izsler.it",
                       Database = "IZSLER", 
                       Port = 1433)

conferimenti <- conn %>% tbl(sql(conf)) %>% as_tibble()
proveIN <- conn %>% tbl(sql(proveIN)) %>% as_tibble()
proveOUT <- conn %>% tbl(sql(proveOUT)) %>% as_tibble()


#Abr <- conn %>% tbl(sql(AbR)) %>% as_tibble()

saveRDS(conferimenti, file = here("RepVirologia", "data","conferimenti.RDS"))
saveRDS(proveIN, file = here("RepVirologia", "data", "proveIN.RDS"))
saveRDS(proveOUT, file = here( "RepVirologia", "data", "proveOUT.RDS"))


#saveRDS(Abr, file = here("data", "processed", "Abr.RDS"))
# conferimenti <- readRDS(here("data", "processed", "conferimenti.RDS"))
# proveIN <- readRDS(here("data", "processed", "proveIN.RDS"))
# proveOUT <- readRDS(here("data", "processed", "proveOUT.RDS"))
# Abr <- readRDS(here("data", "processed", "Abr.RDS"))

proveIN %>%  
  mutate(proveINOUT = rep("in_sede",  nrow(.))) %>% 
  bind_rows(proveOUT %>% mutate(proveINOUT = rep("fuori_sede", nrow(.)) ))%>%  
  mutate(Nconf = paste0(anno,nconf), 
         tecnica = sapply(tecnica, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         testodett = sapply(testodett, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         modexpr2 = sapply(modexpr2, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         prova = sapply(prova, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         esiti = sapply(esiti, iconv, from = "latin1", to = "UTF-8", sub = ""),
         str_analisi = sapply(str_analisi, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         lab_analisi = sapply(lab_analisi, iconv, from = "latin1", to = "UTF-8", sub = "")) %>% 
  saveRDS( file = here("RepVirologia", "data", "prove.RDS"))

conferimenti <- conferimenti %>% 
  mutate(comune = sapply(comune, iconv, from = "latin1", to = "UTF-8", sub = ""),
        # finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = ""),
         motivo_prel = sapply(motivo_prel, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         materiale = sapply(materiale, iconv, from = "latin1", to = "UTF-8", sub = ""))

conferimenti %>% distinct() %>% 
  
  mutate(anno = year(dtconf),
         annoprel = year(dtprel),
         annoreg = year(dtreg),
         codaz = casefold(codaz, upper = TRUE),
         Nconf = paste0(anno,nconf)) %>%  
  distinct(Nconf, .keep_all = TRUE) %>% 
  select(-finalita) %>%  
  
  left_join(
    conferimenti %>% distinct() %>% 
      mutate(anno = year(dtconf),
        Nconf = paste0(anno,nconf)) %>% 
      select(Nconf, finalita) %>% 
      distinct() %>% 
      pivot_wider(names_from = "finalita", values_from = "finalita") %>%   
      unite("finalita", 2:48, na.rm = TRUE, remove = FALSE) %>% 
      select(1,2),  by="Nconf") %>% 
  saveRDS( file = here("RepVirologia", "data", "conf.RDS"))
  

#prove <- readRDS(here("data", "processed", "prove.RDS"))
#conf <- readRDS(here("data", "processed", "conf.RDS"))



#conf_prove <- conf %>% distinct() %>% 
  #left_join(prove, by = "Nconf")






 


#mappa provincia di modena###

#coord <- bvcoord <- read_excel("dati/bvcoord.xlsx", 
                              # col_types = c("text", "numeric", "numeric"))

# 
# ita <- getData("GADM", country = "ITA", level = 0)
# reg <- getData("GADM", country = "ITA", level = 1)
# prov <- getData("GADM", country = "ITA", level = 2)
# com <- getData("GADM", country = "ITA", level = 3)



# MO <- prov[prov$NAME_2 == "Modena", ]
# ER <- reg[reg$NAME_1 == "Emilia-Romagna",]
# commo <- com[com$NAME_2 == "Modena",]
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(data = MO, stroke = TRUE, fill =  FALSE) %>% 
#   addPolygons(data = commo, stroke = TRUE) %>% 
#   addCircleMarkers(lng=coord$lng, lat = coord$lat, radius = 0.5 , label = coord$codaz)


# ##accesso al dbase access
# 
# con <- odbcConnect("modena")
# 
# 
# con <- dbConnect(drv = odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
#                                                        DBQ=C:/Users/vito.tranquillo/Desktop/Git Projects /Reporting-Sanitario/dati/Database AUSL attivit?? 2022.accdb;"))
# 

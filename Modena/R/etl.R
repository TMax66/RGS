library(here)
library(tidyverse)
library(odbc)
library(RODBC)
library(DBI)
library(lubridate)
source(here("Modena", "R", "sql.R"))

# Accesso al dbase access----

conn <- DBI::dbConnect(odbc::odbc(), Driver = "SQL Server", 
                       Server = "dbprod02.izsler.it",
                       Database = "IZSLER", 
                       Port = 1433)

conferimenti <- conn %>% tbl(sql(conf)) %>% as_tibble()
proveIN <- conn %>% tbl(sql(proveIN)) %>% as_tibble()
proveOUT <- conn %>% tbl(sql(proveOUT)) %>% as_tibble()

#Abr <- conn %>% tbl(sql(AbR)) %>% as_tibble()

#coordbv <- read_excel(here("Modena", "data","coordbv.xlsx"))

saveRDS(conferimenti, file = here("Modena", "data", "processed", "conferimenti.RDS"))
saveRDS(proveIN, file = here("Modena", "data", "processed", "proveIN.RDS"))
saveRDS(proveOUT, file = here("Modena", "data", "processed", "proveOUT.RDS"))

#saveRDS(Abr, file = here("Modena", "data", "processed", "Abr.RDS"))
conferimenti <- readRDS(here("Modena", "data", "processed", "conferimenti.RDS"))
proveIN <- readRDS(here("Modena", "data", "processed", "proveIN.RDS"))
proveOUT <- readRDS(here("Modena", "data", "processed", "proveOUT.RDS"))
#Abr <- readRDS(here("Modena", "data", "processed", "Abr.RDS"))

proveIN %>%  
  mutate(proveINOUT = rep("in_sede", nrow(.))) %>% 
  bind_rows(proveOUT %>% mutate(proveINOUT = rep("fuori_sede", nrow(.)))) %>%  
  mutate(Nconf = paste0(anno, nconf), 
         # tecnica = sapply(tecnica, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         # testodett = sapply(testodett, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         # modexpr2 = sapply(modexpr2, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         # prova = sapply(prova, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         # esiti = sapply(esiti, iconv, from = "latin1", to = "UTF-8", sub = ""),
         # str_analisi = sapply(str_analisi, iconv, from = "latin1", to = "UTF-8", sub = ""), 
         # lab_analisi = sapply(lab_analisi, iconv, from = "latin1", to = "UTF-8", sub = "")
         ) %>% 
  saveRDS(file = here("Modena", "data", "processed", "prove.RDS"))

# conferimenti <- conferimenti %>% 
#   mutate(
#     comune = sapply(comune, iconv, from = "latin1", to = "UTF-8", sub = ""),
#     finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = ""),
#     motivo_prel = sapply(motivo_prel, iconv, from = "latin1", to = "UTF-8", sub = ""),
#     materiale = sapply(materiale, iconv, from = "latin1", to = "UTF-8", sub = "")
#     )

conferimenti %>%
  distinct() %>%
  mutate(anno = year(dtconf),
         annoprel = year(dtprel),
         annoreg = year(dtreg),
         codaz = casefold(codaz, upper = TRUE),
         Nconf = paste0(annoreg, nconf)) %>%
  distinct(Nconf, .keep_all = TRUE) %>% 
  dplyr::select(-finalita) %>% 
  left_join(
    conferimenti %>%
      distinct() %>% 
      mutate(anno = year(dtconf),
             annoreg = year(dtreg),
             Nconf = paste0(annoreg, nconf)) %>% 
      dplyr::select(Nconf, finalita) %>% 
      distinct() %>% 
      pivot_wider(names_from = "finalita", values_from = "finalita") %>%  
      unite("finalita", 2:73, na.rm = TRUE, remove = FALSE) %>% 
      dplyr::select(1,2), by = "Nconf") %>% 
  saveRDS(file = here("Modena", "data", "processed", "conf.RDS"))

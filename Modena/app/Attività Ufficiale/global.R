library(shiny)
library(here)
library(tidyverse)
library(openxlsx)
library(lubridate)
#library(rgeos)
#library(rmapshaper)
library(raster)
library(rgdal)
#library(sp)
library(sf)
library(leaflet)
library(maps)
library(readxl)
library(openxlsx)
library(DT)
library(gt)
library(tmap)
#library(GADMTools)
library(tmaptools)
library(stringr)
library(plotly)
library(zoo)
library(janitor)
library(tidyverse)
library(htmltools)
library(showtext)
library(shinyjs)
font_families()
font_add_google("Montserrat", "Montserrat")
showtext_auto()


library(showtext)
font_families()
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# Carico i dati ----
conf <- readRDS(here("Modena", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Modena", "data", "processed", "prove.RDS"))

coordbv <- read_excel(here("Modena", "data","coordbv.xlsx"))

coordbv <- coordbv %>% 
  mutate("long"=as.numeric(lon), "lat"=as.numeric(lat))

## codice per app sul server----
# conf <- readRDS(here("Modena", "data", "processed", "conf.RDS"))
# prove <- readRDS(here("Modena", "data", "processed", "prove.RDS"))

# Preparazione dati----
conf <- conf %>% 
 # mutate(finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = "")) %>%
  rename(ncamp_accettati = NrCampioni) %>% 
  filter(tipo_prelievo == "Ufficiale", 
         str_detect(ASL, "MODENA"))
 

nesami <- prove %>%
  group_by(Nconf) %>% 
  count()

#mappa home----
com <- readRDS(here("Modena", "data", "processed", "ITA_adm3.sf.rds"))
prov <- readRDS(here("Modena", "data", "processed", "ITA_adm2.sf.rds"))
reg <- readRDS(here("Modena", "data", "processed", "ITA_adm1.sf.rds"))
sf::st_crs(com) = 4326
sf::st_crs(prov) = 4326
sf::st_crs(reg) = 4326

MO <- prov[prov$NAME_2 == "Modena", ]
commo <- com[com$NAME_2 == "Modena",]
ER <- reg[reg$NAME_1 == "Emilia-Romagna", ]
PRer <- prov[prov$NAME_1 == "Emilia-Romagna", ]

distretti <- commo %>% 
  st_as_sf() %>% 
  mutate(DISTRETTO = case_when(
    NAME_3 %in% c("Campogalliano", "Carpi", "Novi Di Modena", "Soliera") ~ paste0("DISTRETTO DI CARPI"),
    NAME_3 %in% c("Camposanto", "Cavezzo", "Concordia Sulla Secchia", "Finale Emilia", "Medolla",
                  "Mirandola", "San Felice Sul Panaro", "San Possidonio", "San Prospero") ~ paste0("DISTRETTO DI MIRANDOLA"),
    NAME_3 %in% c("Modena") ~ paste0("DISTRETTO DI MODENA"),
    NAME_3 %in% c("Fiorano Modenese", "Formigine", "Frassinoro", "Maranello", "Montefiorino", "Palagano",
                  "Prignano Sulla Secchia", "Sassuolo") ~ paste0("DISTRETTO DI SASSUOLO"),
    NAME_3 %in% c("Fanano", "Fiumalbo", "Lama Mocogno", "Montecreto", "Pavullo Nel Frignano", "Pievepelago",
                  "Polinago", "Riolunato", "Serramazzoni", "Sestola") ~ paste0("DISTRETTO DI PAVULLO"),
    NAME_3 %in% c("Castelnuovo Rangone", "Castelvetro Di Modena", "Guiglia", "Marano Sul Panaro", "Montese",
                  "Savignano Sul Panaro", "Spilamberto", "Vignola", "Zocca") ~ paste0("DISTRETTO DI VIGNOLA"),
    NAME_3 %in% c("Bastiglia", "Bomporto", "Castelfranco Emilia", "Nonantola", "Ravarino",
                  "San Cesario Sul Panaro") ~ paste0("DISTRETTO DI CASTELFRANCO EMILIA"))) %>%  
    group_by(DISTRETTO) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>% 
  mutate(DISTRETTO2 = case_when(
    DISTRETTO == "DISTRETTO DI CARPI" ~ paste0("CARPI"),
    DISTRETTO == "DISTRETTO DI MIRANDOLA"  ~ paste0("MIRANDOLA"),
    DISTRETTO == "DISTRETTO DI MODENA"  ~ paste0("MODENA"),
    DISTRETTO == "DISTRETTO DI SASSUOLO"  ~ paste0("SASSUOLO"),
    DISTRETTO == "DISTRETTO DI PAVULLO"  ~ paste0("PAVULLO"),
    DISTRETTO == "DISTRETTO DI VIGNOLA"  ~ paste0("VIGNOLA"),
    DISTRETTO == "DISTRETTO DI CASTELFRANCO EMILIA"  ~ paste0("CASTELFRANCO E."))) %>%  
  mutate(allev = c(35,45,10,20,10,20,80)) %>% 
  mutate(esam = c(500,280,320,650,870,920,710))



# pal <- 
#    colorFactor(palette = c("#ff9a66", "#9acc99", "#ffc44c", "#9999cd", "#8fd9f6", "#0284d8","#ffcc9a"), 
#                levels = c("DISTRETTO DI CARPI","DISTRETTO DI CASTELFRANCO EMILIA","DISTRETTO DI MIRANDOLA",
#                           "DISTRETTO DI MODENA","DISTRETTO DI PAVULLO","DISTRETTO DI SASSUOLO","DISTRETTO DI VIGNOLA"))

labels <- paste("<strong>",
                distretti$DISTRETTO,
                "</strong><br>",
                "n° di allevamenti:",
                distretti$allev,
                "<br>",
                "n° esami eseguiti",
                distretti$esam) %>%
  lapply(htmltools::HTML)

labels2 <- paste("distretto di",
                "<br>",
                "<strong>",
                distretti$DISTRETTO2,
                "</strong>") %>%
  lapply(htmltools::HTML)

factpal <- colorFactor(c("#ff9a66", "#9acc99", "#ffc44c", "#9999cd", "#8fd9f6", "#0284d8","#ffcc9a"), 
               c("DISTRETTO DI CARPI","DISTRETTO DI CASTELFRANCO EMILIA","DISTRETTO DI MIRANDOLA",
                          "DISTRETTO DI MODENA","DISTRETTO DI PAVULLO","DISTRETTO DI SASSUOLO","DISTRETTO DI VIGNOLA"))

#8fd9f6




# dataset conferimenti per Settore----

confSA <- conf %>% filter(settore == "Sanità Animale",
                          !str_detect(finalita, "Diagnos"), 
                          !str_detect(finalita, "Progetto"))  
confSicA <- conf %>% filter(settore == "Alimenti Uomo")
confAZ <- conf %>% filter(settore == "Alimenti Zootecnici")

# dataset prove per Settore----

# proveSA <- 
#   confSA %>% dplyr::select(nconf,anno, ASL, finalita, motivo_prel, specie, materiale) 
 


proveSA <- 
  confSA %>% dplyr::select(Nconf,anno, ASL, finalita, motivo_prel, specie, materiale) %>%
  #filter(!str_detect(finalita, "Diagnos")) %>%    
  left_join(prove, by="Nconf") 

proveSicA <- 
  confSicA %>% dplyr::select(Nconf,anno, ASL, finalita,motivo_prel,  matrice) %>% 
  left_join(prove, by="Nconf") 


proveAZ <- 
  confAZ %>% dplyr::select(Nconf,anno, ASL, finalita,motivo_prel,  matrice) %>% 
  left_join(prove, by="Nconf")







# Funzioni----
## Valuebox----

valueBox <- function(value, subtitle, icon, color) {
  div(class = "col-lg-3 col-md-6",
      div(class = "panel panel-primary",
          div(class = "panel-heading", style = paste0("background-color:", color),
              div(class = "row",
                  div(class = "col-xs-3",
                      icon(icon, "fa-5x")
                  ),
                  div(class = ("col-xs-9 text-right"),
                      div(style = ("font-size: 20px; font-weight: bold;"),
                          textOutput(value)
                      ),
                      div(subtitle)
                  )
              )
          ),
          div(class = "panel-footer",
              div(class = "clearfix")
          )
      )
  )
}

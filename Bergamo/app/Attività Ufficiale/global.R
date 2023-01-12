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

library(showtext)
# font_families()
# font_add_google("Montserrat", "Montserrat")
# showtext_auto()

# Carico i dati ----
conf <- readRDS(here("Bergamo", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Bergamo", "data", "processed", "prove.RDS"))

coordbv <- read_excel(here("Bergamo", "data", "allbov.xlsx"))
coordbv <- coordbv %>% 
  mutate(codaz = substr(allevix, 1, 8),
         "long" = as.numeric(lon), 
         "lat" = as.numeric(lat)) %>% 
  mutate(codaz = toupper(codaz))

distrettiBG <- read_excel(here("Bergamo", "data", "distrettiBG.xlsx"))

## codice per app sul server----
# conf <- readRDS(here("Bergamo", "data", "processed", "conf.RDS"))
# prove <- readRDS(here("Bergamo", "data", "processed", "prove.RDS"))

# Preparazione dati----
conf <- conf %>% 
  mutate(finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = "")) %>%
  rename(ncamp_accettati = NrCampioni) %>%  
  filter(tipo_prelievo == "Ufficiale", 
         str_detect(ASL, "BERGAMO"))

nesami <- prove %>%
  group_by(Nconf) %>% 
  count()

#mappa home----

com <- readRDS(here("Bergamo", "data", "processed", "ITA_adm3.sf.rds"))
prov <- readRDS(here("Bergamo", "data", "processed", "ITA_adm2.sf.rds"))
reg <- readRDS(here("Bergamo", "data", "processed", "ITA_adm1.sf.rds"))
sf::st_crs(com) = 4326
sf::st_crs(prov) = 4326
sf::st_crs(reg) = 4326
BG <- prov[prov$NAME_2 == "Bergamo", ]
combg <- com[com$NAME_2 == "Bergamo", ]
LOM <- reg[reg$NAME_1 == "Lombardia", ]
PRlom <- prov[prov$NAME_1 == "Lombardia", ]

combg <- combg %>%
  mutate(NAME_3 = case_when(
    NAME_3 == "Albano Sant' Alessandro" ~ "Albano Sant'Alessandro",
    NAME_3 == "Arzago d' Adda" ~ "Arzago d'Adda",
    NAME_3 == "Brembilla" ~ "Val Brembilla",
    NAME_3 == "Brignano Gera d' Adda" ~ "Brignano Gera d'Adda",
    NAME_3 == "Calusco d' Adda" ~ "Calusco d'Adda",
    NAME_3 == "Canonica d' Adda" ~ "Canonica d'Adda",
    NAME_3 == "Casirate d' Adda" ~ "Casirate d'Adda",
    NAME_3 == "Cazzano Sant' Andrea" ~ "Cazzano Sant'Andrea",
    NAME_3 == "Chignolo d' Isola" ~ "Chignolo d'Isola",
    NAME_3 == "Cortenova" ~ "Cortenuova",
    NAME_3 == "Fara Gera d' Adda" ~ "Fara Gera d'Adda",
    NAME_3 == "Misano di Gera d' Adda" ~ "Misano di Gera d'Adda",
    NAME_3 == "Moio de' Calvi" ~ "Moio dè Calvi",
    NAME_3 == "Rota d' Imagna" ~ "Rota d'Imagna",
    NAME_3 == "San Paolo d' Argon" ~ "San Paolo d'Argon",
    NAME_3 == "Sant' Omobono Imagna" ~ "Sant'Omobono Terme",
    NAME_3 == "Sotto Il Monte Giovanni Xxiii" ~ "Sotto Il M.Giovanni XXIII",
    NAME_3 == "Terno d' Isola" ~ "Terno d'Isola",
    NAME_3 == "Torre de' Roveri" ~ "Torre Dè Roveri",
    NAME_3 == "Villa d' Adda" ~ "Villa d'Adda",
    NAME_3 == "Villa d' Almè" ~ "Villa d'Almè",
    NAME_3 == "Villa d' Ogna" ~ "Villa d'Ogna",
    NAME_3 == "Gerosa" ~ "Val Brembilla", #ex-comune
    NAME_3 == "Valsecca" ~ "Sant'Omobono Terme", #ex-comune
    TRUE ~ NAME_3)) %>% 
  mutate(NAME_3 = str_replace_all(
    NAME_3, c(
      " Di " = " di ",
      " Del " = " del ",
      " Degli " = " degli ",
      " Della " = " della ",
      " Con " = " con ",
      " Al " = " al ",
      "Oltre Il Colle" = "Oltre il Colle"))) %>% 
  group_by(NAME_3) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup()

combg <- combg %>%
  left_join(distrettiBG, by = c("NAME_3" = "comune"))

distretti <- combg %>% 
  st_as_sf() %>% 
  group_by(distretto) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%  
  mutate(distretto2 = case_when(
    distretto == "A.T.S. BERGAMO - DISTR. A" ~ paste0("DISTRETTO A"),
    distretto == "A.T.S. BERGAMO - DISTR. B"  ~ paste0("DISTRETTO B"))) %>% 
  mutate(confer = c(14016,11192)) %>% 
  mutate(campion = c(68868,110073))

# pal <-
#   colorFactor(palette = c( "#99b9cd",  "#0284d8"),
#               levels = c("A.T.S. BERGAMO - DISTR. A","A.T.S. BERGAMO - DISTR. B"))

labels <- paste("<strong>",
                distretti$distretto,
                "</strong><br>",
                "n° conferimenti:",
                distretti$confer,
                "<br>",
                "n° campioni",
                distretti$campion) %>%
  lapply(htmltools::HTML)

labels2 <- paste("<strong>",
                 distretti$distretto2,
                 "</strong>") %>%
  lapply(htmltools::HTML)

factpal <- colorFactor(c("#99b9cd",  "#0284d8"),
                       c("A.T.S. BERGAMO - DISTR. A","A.T.S. BERGAMO - DISTR. B"))




# dataset conferimenti per Settore----

confSA <- conf %>% filter(settore == "Sanità Animale",
                          !str_detect(finalita, "Diagnos"), 
                          !str_detect(finalita, "Progetto")) %>% 
  mutate(codaz = toupper(codaz))

confSicA <- conf %>% filter(settore == "Alimenti Uomo") %>% 
  mutate(codaz = toupper(codaz))

confAZ <- conf %>% filter(settore == "Alimenti Zootecnici") %>% 
  mutate(codaz = toupper(codaz))

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

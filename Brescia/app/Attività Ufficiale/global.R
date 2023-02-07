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
library(shinyjs)

library(showtext)
font_families()
font_add_google("Montserrat", "Montserrat")
showtext_auto()

# Carico i dati ----
conf <- readRDS(here("Brescia", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Brescia", "data", "processed", "prove.RDS"))

# coordbv <- read_excel(here("Brescia", "data", "coordbv.xlsx"))
# coordbv <- coordbv %>% 
#   mutate("long" = as.numeric(lon), "lat" = as.numeric(lat))

coord <- read_sf(here("Brescia", "data", "allevamenti", "allevamenti_bovidi.shp"))

coord <- coord %>%
  st_set_crs(3003) %>% 
  st_transform(4326) %>%
  mutate(long = sf::st_coordinates(.)[,1],
         lat = sf::st_coordinates(.)[,2]) %>% 
  rename(COMUNE = DS_COMUNE)

coord <- coord %>% 
  slice_head(n = 3847)

jo <- conf %>% 
  filter(settore == "Sanità Animale",
        !str_detect(finalita, "Diagnos"), 
        !str_detect(finalita, "Progetto"),
        !is.na(codaz)) %>% 
  distinct(codaz) 

coordbv <- bind_cols(jo, coord)


# coordbv <- coordbv %>% 
#   mutate(codaz = substr(allevix, 1, 8),
#          "long" = as.numeric(lon), 
#          "lat" = as.numeric(lat)) %>% 
#   mutate(codaz = toupper(codaz))

distrettiBS <- read_excel(here("Brescia", "data", "distrettiBS.xlsx"))

## codice per app sul server----
# conf <- readRDS(here("Brescia", "data", "processed", "conf.RDS"))
# prove <- readRDS(here("Brescia", "data", "processed", "prove.RDS"))

# Preparazione dati----
conf <- conf %>% 
 # mutate(finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = "")) %>%
  rename(ncamp_accettati = NrCampioni) %>% 
  filter(tipo_prelievo == "Ufficiale", 
         str_detect(ASL, paste(c("BRESCIA", "MONTAGNA"), collapse = '|'))) %>%  
  mutate(ASL = case_when(
    ASL == "A.T.S. BRESCIA - DISTR. BRESCIA - GARDONE V.T." ~ paste0("DISTRETTO 1 DI BRESCIA"),
    ASL == "A.T.S. BRESCIA - DISTR. ROVATO"  ~ paste0("DISTRETTO 4 DI ROVATO"),
    ASL == "A.T.S. BRESCIA - DISTR. LONATO" ~ paste0("DISTRETTO 2 DI LONATO"),
    ASL == "A.T.S. BRESCIA - DISTR. LENO"  ~ paste0("DISTRETTO 3 DI LENO"),
    ASL == "A.T.S. MONTAGNA - DISTR. VALCAMONICA SEBINO"  ~ paste0("DISTRETTO VALCAMONICA - SEBINO"),
    TRUE ~ ASL))

nesami <- prove %>%
  group_by(Nconf) %>% 
  count()

#mappa home----
com <- readRDS(here("Brescia", "data", "processed", "ITA_adm3.sf.rds"))
prov <- readRDS(here("Brescia", "data", "processed", "ITA_adm2.sf.rds"))
reg <- readRDS(here("Brescia", "data", "processed", "ITA_adm1.sf.rds"))
sf::st_crs(com) = 4326
sf::st_crs(prov) = 4326
sf::st_crs(reg) = 4326

BS <- prov[prov$NAME_2 == "Brescia", ]
comBS <- com[com$NAME_2 == "Brescia", ]
LOM <- reg[reg$NAME_1 == "Lombardia", ]
PRlom <- prov[prov$NAME_1 == "Lombardia", ]

comBS <- comBS %>%
  mutate(NAME_3 = case_when(
    NAME_3 == "Lonato" ~ "Lonato del Garda",
    NAME_3 == "Palazzolo sull' Oglio" ~ "Palazzolo sull'Oglio",
    NAME_3 == "Provaglio d' Iseo" ~ "Provaglio d'Iseo",
    NAME_3 == "Puegnago Sul Garda" ~ "Puegnago del Garda",
    NAME_3 == "Quinzano d' Oglio" ~ "Quinzano d'Oglio",
    NAME_3 == "Rodengo-Saiano" ~ "Rodengo Saiano",
    NAME_3 == "Toscolano-Maderno" ~ "Toscolano Maderno",
    NAME_3 == "Urago d' Oglio" ~ "Urago d'Oglio",
    NAME_3 == "Vezza d' Oglio" ~ "Vezza d'Oglio",
    NAME_3 == "Saviore dell' Adamello" ~ "Saviore dell'Adamello",
    NAME_3 == "Prestine" ~ "Bienno",
    TRUE ~ NAME_3)) %>% 
  mutate(NAME_3 = str_replace_all(
    NAME_3, c(
      " Di " = " di ",
      " Del " = " del ",
      " Degli " = " degli ",
      " Della " = " della ",
      " Con " = " con ",
      " Sul " = " sul "))) %>% 
  group_by(NAME_3) %>%
  mutate(geometry = sf::st_union(geometry)) %>%
  ungroup()

comBS <- comBS %>%
  left_join(distrettiBS, by = c("NAME_3" = "comune"))



distretti <- comBS %>% 
  #filter(!is.na(distretto)) %>% 
  st_as_sf() %>% 
  group_by(distretto) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  mutate(distretto2 = case_when(
    distretto == "A.T.S. BRESCIA - DISTR. BRESCIA - GARDONE V.T." ~ paste0("DISTRETTO 1 DI BRESCIA"),
    distretto == "A.T.S. BRESCIA - DISTR. ROVATO"  ~ paste0("DISTRETTO 4 DI ROVATO"),
    distretto == "A.T.S. BRESCIA - DISTR. LONATO" ~ paste0("DISTRETTO 2 DI LONATO"),
    distretto == "A.T.S. BRESCIA - DISTR. LENO"  ~ paste0("DISTRETTO 3 DI LENO"),
    distretto == "A.T.S. MONTAGNA - DISTR. VALCAMONICA SEBINO"  ~ paste0("DISTRETTO DI VALCAMONICA-SEBINO")
    )) %>%
  mutate(confer = c(14016,11192,10,20,10)) %>%
  mutate(campion = c(68868,110073,10,20,10))

ATS_BS <- distretti %>% 
  st_as_sf() %>% 
  group_by(distretto) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%  
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))


# pal <- 
#    colorFactor(palette = c("#ff9a66", "#9acc99", "#ffc44c", "#9999cd", "#8fd9f6", "#0284d8","#ffcc9a"), 
#                levels = c("DISTRETTO DI CARPI","DISTRETTO DI CASTELFRANCO EMILIA","DISTRETTO DI MIRANDOLA",
#                           "DISTRETTO DI MODENA","DISTRETTO DI PAVULLO","DISTRETTO DI SASSUOLO","DISTRETTO DI VIGNOLA"))

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

factpal <- colorFactor(c("#ff9a66", "#9acc99", "#ffc44c", "#9999cd", "#ffcc9a"), 
               c("A.T.S. BRESCIA - DISTR. BRESCIA - GARDONE V.T.",
                 "A.T.S. BRESCIA - DISTR. LENO",
                 "A.T.S. BRESCIA - DISTR. LONATO",
                 "A.T.S. BRESCIA - DISTR. ROVATO",
                 "A.T.S. MONTAGNA - DISTR. VALCAMONICA SEBINO"
                 ))

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
  confSA %>% dplyr::select(Nconf, anno, ASL, finalita, motivo_prel, specie, materiale) %>%
  #filter(!str_detect(finalita, "Diagnos")) %>%    
  left_join(prove, by="Nconf") 

proveSicA <- 
  confSicA %>% dplyr::select(Nconf, anno, ASL, finalita, motivo_prel, matrice) %>% 
  left_join(prove, by="Nconf") 

proveAZ <- 
  confAZ %>% dplyr::select(Nconf, anno, ASL, finalita, motivo_prel, matrice) %>% 
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

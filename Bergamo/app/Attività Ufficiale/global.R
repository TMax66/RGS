#NOTA: server = TRUE aggiungi downloadhandler esterno

# dt datatable server true and dowload all - Cerca con Google
# https://www.google.com/search?q=dt+datatable+server+true+and+dowload+all&rlz=1C1GCEU_itIT998IT998&oq=dt+datatable+s&aqs=chrome.0.69i59j0i19i512l3j69i57j69i60l3.6419j0j7&sourceid=chrome&ie=UTF-8
# 
# r - Enabling download all with server = TRUE datatable by using invisible download button - Stack Overflow
# https://stackoverflow.com/questions/68688737/enabling-download-all-with-server-true-datatable-by-using-invisible-download-b
# 
# javascript - Download all data of shiny datatable - Stack Overflow
# https://stackoverflow.com/questions/67182328/download-all-data-of-shiny-datatable
# 
# Scaricare tutti i dati usando l'estensione Buttons · Issue #267 · rstudio/DT
# https://github.com/rstudio/DT/issues/267


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
library(shinyWidgets)
library(grillade)

({ })

# Carico i dati ----
conf <- readRDS(here("Bergamo", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Bergamo", "data", "processed", "prove.RDS"))

coordbv <- read_excel(here("Bergamo", "data", "coordbv.xlsx"))
coordbv <- coordbv %>% 
  mutate(codaz = substr(allevix, 1, 8),
         "long" = as.numeric(lon), 
         "lat" = as.numeric(lat)) %>% 
  mutate(codaz = toupper(codaz)) %>% 
  rename(COMUNE = Comune,
         XT_DENOMINAZIONE = Denominazione)

distretti_ASL_ATS <- read_excel(here("Bergamo", "data", "distretti_ASL_ATS.xlsx"))

# Preparazione dati----
conf <- conf %>%
  mutate(annoreg = year(dtreg),
         weekreg = strftime(dtreg, format = "%U-%Y"),
         Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
         Nconf3 = gsub("^.{0,4}", "", Nconf),
         dtprel_chr = format(dtprel, "%d/%m/%Y"),
         dtconf_chr = format(dtconf, "%d/%m/%Y"),
         dtreg_chr = format(dtreg, "%d/%m/%Y"),
         NrCampioni_chr = as.character(NrCampioni),
         specie = str_to_sentence(specie),
         materiale = str_to_sentence(materiale),
         matrice = str_to_sentence(matrice)) %>%
  # mutate(finalita = sapply(finalita, iconv, from = "latin1", to = "UTF-8", sub = "")) %>%
  # rename(ncamp_accettati = NrCampioni) %>% 
  filter(tipo_prelievo == "Ufficiale", 
         str_detect(ASL, "BERGAMO")) %>%  
  mutate(ASL = case_when(
    ASL == "A.T.S. BERGAMO - DISTR. A" ~ paste0("DISTRETTO A"),
    ASL == "A.T.S. BERGAMO - DISTR. B"  ~ paste0("DISTRETTO B"),
    TRUE ~ ASL))

nesami <- prove %>%
  group_by(Nconf) %>% 
  count()

#MAPPA HOME----
com <- readRDS(here("Bergamo", "data", "processed", "ITA_adm3.sf.rds"))
prov <- readRDS(here("Bergamo", "data", "processed", "ITA_adm2.sf.rds"))
reg <- readRDS(here("Bergamo", "data", "processed", "ITA_adm1.sf.rds"))
sf::st_crs(com) = 4326
sf::st_crs(prov) = 4326
sf::st_crs(reg) = 4326

provincia <- prov[prov$NAME_2 == "Bergamo", ]
comuni <- com[com$NAME_2 == "Bergamo", ]
regione <- reg[reg$NAME_1 == "Lombardia", ]
province_regione <- prov[prov$NAME_1 == "Lombardia", ]

comuni <- comuni %>%
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


comuni <- comuni %>%
  left_join(distretti_ASL_ATS, by = c("NAME_3" = "comune"))

distretti <- comuni %>% 
  #filter(!is.na(distretto)) %>% 
  st_as_sf() %>% 
  group_by(distretto) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>%
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  mutate(distretto2 = case_when(
    distretto == "A.T.S. BERGAMO - DISTR. A" ~ paste0("DISTRETTO A"),
    distretto == "A.T.S. BERGAMO - DISTR. B"  ~ paste0("DISTRETTO B")
  )) %>%
  mutate(distretto3 = case_when(
    distretto == "A.T.S. BERGAMO - DISTR. A" ~ paste0("ATS BERGAMO<br>DISTRETTO A"),
    distretto == "A.T.S. BERGAMO - DISTR. B"  ~ paste0("ATS BERGAMO<br>DISTRETTO B")
  ))
  
  # mutate(confer = c(14016,11192,10,20,10)) %>%
  # mutate(campion = c(68868,110073,10,20,10))

ASL_ATS <- distretti %>% 
  st_as_sf() %>% 
  group_by(distretto) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%  
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))


# pal <- colorFactor(
#   palette = c("#ff9a66", "#9acc99", "#ffc44c", "#9999cd", "#8fd9f6", "#0284d8","#ffcc9a"),
#   levels = c("DISTRETTO DI CARPI","DISTRETTO DI CASTELFRANCO EMILIA","DISTRETTO DI MIRANDOLA",
#              "DISTRETTO DI MODENA","DISTRETTO DI PAVULLO","DISTRETTO DI SASSUOLO","DISTRETTO DI VIGNOLA"))


# SPOSTATO IN SERVER_CODE home.R:
# labels <- paste("<strong>",
#                 distretti$distretto,
#                 "</strong><br>",
#                 "n° conferimenti:",
#                 distretti$confer,
#                 "<br>",
#                 "n° campioni",
#                 distretti$campion) %>%
#   lapply(htmltools::HTML)

labels2 <- paste("distretto di",
                 "<br>",
                 "<strong>",
                 distretti$distretto2,
                 "</strong>") %>%
  lapply(htmltools::HTML)

factpal <- colorFactor(c("#99b9cd",  "#0284d8"),
                       c("A.T.S. BERGAMO - DISTR. A","A.T.S. BERGAMO - DISTR. B"))

#8fd9f6




# CONF per Settore----

confSA <- conf %>% filter(settore == "Sanità Animale",
                          #!str_detect(finalita, "Diagnos"), 
                          !str_detect(finalita, "Progetto")) 

confSicA <- conf %>% filter(settore == "Alimenti Uomo",
                            #!str_detect(finalita, "Diagnos"), 
                            !str_detect(finalita, "Progetto"))

confAZ <- conf %>% filter(settore == "Alimenti Zootecnici",
                          #!str_detect(finalita, "Diagnos"), 
                          !str_detect(finalita, "Progetto"))

# PROVE per Settore----

proveSA <- 
  confSA %>% 
  # dplyr::select(Nconf, anno, ASL, finalita, motivo_prel, specie, materiale) %>%
  #filter(!str_detect(finalita, "Diagnos")) %>%    
  left_join(prove, by = "Nconf") %>% 
  mutate(annoiniz = year(dtinizio),
         numero_del_campione_chr = as.character(numero_del_campione),
         dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
         dtfine_chr = format(dtfine, "%d/%m/%Y"))

proveSicA <- 
  confSicA %>% 
  left_join(prove, by = "Nconf") %>% 
  mutate(annoiniz = year(dtinizio),
         numero_del_campione_chr = as.character(numero_del_campione),
         dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
         dtfine_chr = format(dtfine, "%d/%m/%Y"))

proveAZ <- 
  confAZ %>% 
  left_join(prove, by = "Nconf") %>% 
  mutate(annoiniz = year(dtinizio),
         numero_del_campione_chr = as.character(numero_del_campione),
         dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
         dtfine_chr = format(dtfine, "%d/%m/%Y"))



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


navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[4]][[1]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]], form)
  navbar
}


# SPINNER----
# https://www.listendata.com/2019/07/add-loader-for-shiny-r.html

# Image in global e css code
#img <- 'http://northerntechmap.com/assets/img/Loading_Spinner.svg'
# /* background-image: url('",img,"'); */
  
# Image Size in global e css code
#imgsize <- "auto 20%" #"auto 40%"
# /* background-size:", imgsize, "; */
  

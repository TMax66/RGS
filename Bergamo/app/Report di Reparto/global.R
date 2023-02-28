library(shiny)
library(here)
library(tidyverse)
library(openxlsx)
library(lubridate)
library(DT)
library(zoo)
library(plotly)
library(gt)
library(janitor)
library(rpivotTable)
library(reactable)
library(shinyjs)
library(shinyWidgets)
library(grillade)


conf <- readRDS(here("Bergamo", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Bergamo", "data", "processed", "prove.RDS"))

conf <- conf %>%
  mutate(annoconf = year(dtconf), 
         # annoprel = year(dtprel), 
         annoreg = year(dtreg),
         weekreg = strftime(dtreg, format = "%U-%Y"),
         Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
         Nconf3 = gsub("^.{0,4}", "", Nconf))

#per nascondere date mancanti plotly accettazione
# date_range_acc <- seq(min(as.Date(conf$dtreg[conf$annoreg == 2022])), max(as.Date(conf$dtreg[conf$annoreg == 2022])), by = 1) 
# date_breaks_acc <- as.character(date_range_acc[!date_range_acc %in% as.Date(conf$dtreg)])
# 
# 



# Preparazione dati----

## boxvalue homepage (n.conferimenti e n.esami per settore)
nesami <- prove %>% 
  group_by(Nconf) %>% 
  count()

## esami per pivot
esami <- prove %>%
  dplyr::select(-anno) %>%
  mutate(anno = year(dtinizio), 
         mese = month(dtinizio)) %>% 
  group_by(nconf, anno, mese, str_analisi, lab_analisi, prova, tecnica, proveINOUT) %>%
  summarise("numero di esami" = n())

esamidt <- as.data.frame(esami)


#LABORATORI----
## lab diagnostica ----

diagnostica <- conf %>% 
  # rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("Nconf")) %>% 
  filter(str_analisi == "Sede Territoriale di Bergamo",
         lab_analisi == "Laboratorio Diagnostica Generale") %>%
  mutate(annoiniz = year(dtinizio),
         weekiniz = strftime(dtinizio, format = "%U-%Y"),
         annofine = year(dtfine),
         # Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)), #METTI IN CONF
         # Nconf3 = gsub("^.{0,4}", "", Nconf), #METTI IN CONF
         settore = as.factor(settore), #METTI IN CONF
         tipo_prelievo = as.factor(tipo_prelievo))

# diagnostica_esami <- prove %>% 
#   # rename(ncamp_accettati = NrCampioni) %>%  
#   left_join(conf, by = c("Nconf")) %>% 
#   filter(str_analisi == "Sede Territoriale di Bergamo",
#          lab_analisi == "Laboratorio Diagnostica Generale") %>%
#   mutate(annoiniz = year(dtinizio),
#          weekiniz = strftime(dtinizio, format = "%U-%Y"),
#          annofine = year(dtfine),
#          settore = as.factor(settore), #METTI IN CONF
#          tipo_prelievo = as.factor(tipo_prelievo))

# #per nascondere date mancanti plotly diagnostica
# date_range_diagn <- seq(min(as.Date(diagnostica$dtconf[diagnostica$annoconf == 2022])), max(as.Date(diagnostica$dtconf[diagnostica$annoconf == 2022])), by = 1) 
# date_breaks_diagn <- as.character(date_range_diagn[!date_range_diagn %in% as.Date(diagnostica$dtconf)])


##lab sierologia ----

sierologia <- conf %>%
  left_join(prove, by = c("Nconf")) %>% 
  filter(str_analisi == "Sede Territoriale di Bergamo", 
         lab_analisi == "Laboratorio Sierologia") %>%
  mutate(annoiniz = year(dtinizio),
         weekiniz = strftime(dtinizio, format = "%U-%Y"),
         annofine = year(dtfine),
         settore = as.factor(settore),
         tipo_prelievo = as.factor(tipo_prelievo))


##lab microbiologia degli alimenti ----

microbiologia <- conf %>%
  left_join(prove, by = c("Nconf")) %>% 
  filter(str_analisi == "Sede Territoriale di Bergamo", 
         lab_analisi == "Laboratorio Microbiologia degli Alimenti") %>%
  mutate(annoiniz = year(dtinizio),
         weekiniz = strftime(dtinizio, format = "%U-%Y"),
         annofine = year(dtfine),
         settore = as.factor(settore),
         tipo_prelievo = as.factor(tipo_prelievo))



# HOME - CONF e PROVE per Settore----
confSA <- conf %>% filter(settore == "Sanità Animale") 

confSicA <- conf %>% filter(settore == "Alimenti Uomo")

confAZ <- conf %>% filter(settore == "Alimenti Zootecnici")

proveSA <- 
  confSA %>%
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


### lab biologia molecolare 
# campioni accettati da Bergamo con prove effettuate dal lab di diagnostica di Bergamo
# 
# biomol <- conf %>% 
#   rename(ncamp_accettati = NrCampioni) %>%  
#   left_join(prove, by = c("anno", "nconf")) %>% 
#   filter( anno == 2022,
#           str_analisi == "Sede Territoriale di Bergamo",
#           lab_analisi == "Laboratorio Biologia molecolare")


### lab controllo alimenti
# # campioni accettati da Bergamo con prove effettuate dal lab di microbiologia alimenti di Bergamo
# alimenti <- conf %>% 
#   rename(ncamp_accettati = NrCampioni) %>%  
#   left_join(prove, by = c("anno", "nconf")) %>% 
#   filter(anno == 2022,
#          str_analisi == "Sede Territoriale di Bergamo",
#          lab_analisi == "Laboratorio Microbiologia degli Alimenti")
# 
# 
# ##attività di ricerca
# 
# ricerca <- conf %>% 
#   rename(ncamp_accettati = NrCampioni) %>%  
#   left_join(prove, by = c("anno", "nconf")) %>%  
#   filter(anno == 2022,
#          str_analisi == "Sede Territoriale di Bergamo", 
#          str_detect(finalita, "Progett"))


#controllo qualità
  
  
#autocontrollo ( attività non ufficiale)



# FUNZIONI----

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


italianDate <- function(x) {
  locale <- Sys.getlocale("LC_TIME")
  
  # when function exits, restore original locale
  on.exit(Sys.setlocale("LC_TIME", locale))
  
  Sys.setlocale("LC_TIME", "it_IT.UTF-8")
  
  str_to_title(format(x, "%A %d %B %Y"))
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


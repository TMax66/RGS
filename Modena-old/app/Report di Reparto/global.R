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
library(shinyWidgets)
library(shinyjs)



conf <- readRDS(here("Modena", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Modena", "data", "processed", "prove.RDS"))


# Preparazione dati----


## boxvalue homepage (n.conferimenti e n.esami per settore)

nesami <- prove %>% 
  group_by(Nconf) %>% 
  count()

## esami per tabelle pivot----
esami <- prove %>% select(-anno) %>% 
  mutate(anno = year(dtinizio), 
         mese = month(dtinizio)) %>% 
  group_by(nconf, anno, mese, str_analisi,lab_analisi,prova, tecnica,proveINOUT ) %>% 
summarise(n.esami = n())






## Sezione laboratori----
###lab sierologia ----
# campioni accettati da Modena con prove effettuate dal lab di sierologia di Modena

siero <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter(#anno == input$selanno,
          proveINOUT == "in_sede", 
          lab_analisi == "Laboratorio Sierologia")
          
                # str_analisi == "Sede Territoriale di Modena",
                # )


### lab diagnostica 
# campioni accettati da Modena con prove effettuate dal lab di diagnostica di Modena

diagnostica <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter( #anno == input$selanno,
          str_analisi == "Sede Territoriale di Modena",
          lab_analisi == "Laboratorio Diagnostica Generale")


### lab biologia molecolare 
# campioni accettati da Modena con prove effettuate dal lab di diagnostica di Modena

biomol <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter( #anno == input$selanno,
          str_analisi == "Sede Territoriale di Modena",
          lab_analisi == "Laboratorio Biologia molecolare")


### lab controllo alimenti
# campioni accettati da Modena con prove effettuate dal lab di microbiologia alimenti di Modena
alimenti <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter( #anno == input$selanno,
          str_analisi == "Sede Territoriale di Modena",
          lab_analisi == "Laboratorio Microbiologia degli Alimenti")


##attività di ricerca

ricerca <-   conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>%  
  filter(# anno == input$selanno,
          str_analisi == "Sede Territoriale di Modena", 
          str_detect(finalita, "Progett"))


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


navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form", inputs)
  navbar[[4]][[1]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[4]][[1]][[1]]$children[[1]], form)
  navbar
}
  

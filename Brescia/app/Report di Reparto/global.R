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


conf <- readRDS(here("Brescia", "data", "processed", "conf.RDS"))
prove <- readRDS(here("Brescia", "data", "processed", "prove.RDS"))

conf <- conf %>%
  mutate(annoconf = year(dtconf), 
         annoprel = year(dtprel), 
         annoreg = year(dtreg),
         weekconf = strftime(dtconf, format = "%U-%Y"))

#per nascondere date mancanti plotly accettazione
date_range_acc <- seq(min(as.Date(conf$dtconf[conf$annoconf == 2022])), max(as.Date(conf$dtconf[conf$annoconf == 2022])), by = 1) 
date_breaks_acc <- as.character(date_range_acc[!date_range_acc %in% as.Date(conf$dtconf)])





# Preparazione dati----


## boxvalue homepage (n.conferimenti e n.esami per settore)

nesami <- prove %>% 
  group_by(Nconf) %>% 
  count()

## esami per tabelle pivot----
esami <- prove %>%
  select(-anno) %>%
  mutate(anno = year(dtinizio), 
         mese = month(dtinizio)) %>% 
  group_by(nconf, anno, mese, str_analisi, lab_analisi,prova, tecnica, proveINOUT) %>%
  summarise(n.esami = n())


## Sezione laboratori----
###lab sierologia ----
# campioni accettati da Modena con prove effettuate dal lab di sierologia di Modena

siero <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter(anno == 2022,
         proveINOUT == "in_sede", 
         lab_analisi == "Laboratorio Sierologia")

      # str_analisi == "Sede Territoriale di Modena",
      # )


### lab diagnostica 
# campioni accettati da Modena con prove effettuate dal lab di diagnostica di Modena



diagnostica <- conf %>% 
  #rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter(anno == 2022,
         str_analisi == "Sede Territoriale di Brescia",
         lab_analisi == "Laboratorio Diagnostica Generale") %>%
  mutate(anno_conf = sub(".*-","", weekconf),
         sett_conf = as.numeric(sub("-.*","", weekconf)),
         dtconf = as.Date(dtconf),
         sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
         intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")))


date_range_diagn <- seq(min(as.Date(diagnostica$dtconf[diagnostica$annoconf == 2022])), max(as.Date(diagnostica$dtconf[diagnostica$annoconf == 2022])), by = 1) 
date_breaks_diagn <- as.character(date_range_diagn[!date_range_diagn %in% as.Date(diagnostica$dtconf)])
### lab biologia molecolare 
# campioni accettati da Modena con prove effettuate dal lab di diagnostica di Modena
# 
# biomol <- conf %>% 
#   rename(ncamp_accettati = NrCampioni) %>%  
#   left_join(prove, by = c("anno", "nconf")) %>% 
#   filter( anno == 2022,
#           str_analisi == "Sede Territoriale di Modena",
#           lab_analisi == "Laboratorio Biologia molecolare")


### lab controllo alimenti
# campioni accettati da Modena con prove effettuate dal lab di microbiologia alimenti di Modena
alimenti <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>% 
  filter(anno == 2022,
         str_analisi == "Sede Territoriale di Brescia",
         lab_analisi == "Laboratorio Microbiologia degli Alimenti")


##attività di ricerca

ricerca <- conf %>% 
  rename(ncamp_accettati = NrCampioni) %>%  
  left_join(prove, by = c("anno", "nconf")) %>%  
  filter(anno == 2022,
         str_analisi == "Sede Territoriale di Brescia", 
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


italianDate <- function(x) {
  locale <- Sys.getlocale("LC_TIME")
  
  # when function exits, restore original locale
  on.exit(Sys.setlocale("LC_TIME", locale))
  
  Sys.setlocale("LC_TIME", "it_IT.UTF-8")
  
  str_to_title(format(x, "%A %d %B %Y"))
}

  

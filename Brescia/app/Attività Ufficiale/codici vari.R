## numero di allevamenti campionati (vanno filtrati solo i conferimenti ufficiali)

output$nall <- renderText({
  d <- conf %>% 
    filter(anno == 2022, 
           codaz != "000IX000") %>% 
    distinct(codaz) %>% 
    summarise( n = n())
  d$n
})

## numero di campioni prelevati (ncamprelsa)----

output$ncamprelsa <- renderText({
  dx <- conf %>% 
    filter(anno == 2022, 
           settore == "Sanità Animale") %>% 
    distinct(Nconf, .keep_all = TRUE) %>%  
    summarise( n = sum(NrCampioni))
  dx$n
})



## numero di campioni esaminati in controlli ufficiali in Sanità Animale (ncampSA)

output$nesamiSA  <- renderText({
  
  d2 <- conf %>% 
    filter(settore == "Sanità Animale", 
           anno == 2022) %>% 
    left_join(ncampioni, by = "Nconf") %>% ungroup() %>% 
    summarise(ncamp = sum(n, na.rm = TRUE))
  d2$ncamp
})


## numero di campionamenti in controllo alimenti ( n. di conferimenti nel Settore Alimenti nconfSicA)

output$nconfSicA<- renderText({
  d3 <- conf %>% 
    filter(anno == 2022,
           settore == "Alimenti Uomo") %>%
    distinct(nconf) %>% 
    summarise( n = n())
  d3$n
})

## numero campioni prelevati per controllo alimenti (ncampprelSicA)
output$ncampprelSicA <- renderText({
  dz <- conf %>% 
    filter(anno == 2022, 
           settore == "Alimenti Uomo") %>% 
    distinct(Nconf, .keep_all = TRUE) %>%  
    summarise( n = sum(NrCampioni))
  dz$n
})

## numero di campioni esaminati in controlli ufficiali in Alimenti Uomo (ncampSicA)

output$nesamiSicA <- renderText({
  d4 <- conf %>% 
    filter(anno == 2022,
           settore == "Alimenti Uomo") %>%
    left_join(ncampioni, by = "Nconf") %>% 
    summarise(ncamp = sum(n, na.rm = TRUE))
  
  d4$ncamp
  
  
})


## numero controlli alimZoo

output$nconfAZ<- renderText({
  az <- conf %>% 
    filter(anno == 2022,
           settore == "Alimenti Zootecnici") %>%
    distinct(nconf) %>% 
    summarise( n = n())
  az$n
})


## numero campioni alimZoo
output$ncampprelAZ <- renderText({
  az2 <- conf %>% 
    filter(anno == 2022, 
           settore == "Alimenti Zootecnici") %>% 
    distinct(Nconf, .keep_all = TRUE) %>%  
    summarise( n = sum(NrCampioni))
  az2$n
})

## numero di campioni esaminati in controlli ufficiali in Alimenti Uomo (ncampSicA)

output$nesamiAZ <- renderText({
  z3 <- conf %>% 
    filter(anno == 2022,
           settore == "Alimenti Zootecnici") %>%
    left_join(ncampioni, by = "Nconf") %>% 
    summarise(ncamp = sum(n, na.rm = TRUE))
  
  z3$ncamp
  
  
})






conf %>%
  filter(anno == 2022, settore == "Sanità Animale") %>%
  distinct(nconf, .keep_all = TRUE) %>%
  mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
  group_by(ASL) %>% 
  summarise('gg attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE),1))

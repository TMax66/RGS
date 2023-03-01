confHSA<- reactive(conf %>% 
                   rename(ncamp_accettati = NrCampioni) %>% 
                   distinct(Nconf, .keep_all = TRUE) %>%
                   filter(anno == input$selanno, 
                   settore == "Sanità Animale") )


output$thomeSA<-renderTable( 
  confHSA() %>% 
  summarise( 'N.conferimenti' = n()) %>%
    
  bind_cols(
    confHSA() %>% 
    summarise( 'Campioni conferiti'  = sum(ncamp_accettati))  
            ) %>%           
  bind_cols(
    confHSA() %>% 
    left_join(nesami, by = "Nconf") %>% ungroup() %>% 
    summarise('Esami eseguiti' = sum(n, na.rm = TRUE))
           )%>% 
          
  bind_cols(
    confHSA()  %>%
    mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
    summarise('tempo attesa mediano (gg)' = round(quantile(tempo_attesa_esito, probs = 0.5,  na.rm = TRUE))))%>% 
    gt()
                    
)

confHAU <-  reactive(conf %>%
  distinct(Nconf, .keep_all = TRUE) %>% 
  rename(ncamp_accettati = NrCampioni) %>% 
  filter(anno == input$selanno, settore == "Alimenti Uomo")
)

output$thomeAU <-renderTable( 
confHAU() %>% 
  summarise( 'N.conferimenti' = n()) %>% 
  
  bind_cols( 
    confHAU() %>%  
      summarise( 'Campioni conferiti'  = sum(ncamp_accettati))) %>% 
  
  bind_cols(
    esami <- confHAU() %>% 
      left_join(nesami, by = "Nconf") %>% ungroup() %>% 
      summarise('Esami eseguiti' = sum(n, na.rm = TRUE)))%>% 
  bind_cols(
    confHAU() %>% 
      mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
      summarise('tempo attesa mediano (gg)' = round(median(tempo_attesa_esito, na.rm = TRUE),1)))%>% 
  gt()
)
 

confHAZ <- reactive(conf %>%
                    rename(ncamp_accettati = NrCampioni) %>%
                    distinct(Nconf, .keep_all = TRUE) %>%
                    filter(anno == input$selanno, settore == "Alimenti Zootecnici"))

output$thomeAZ <- renderTable(

  confHAZ() %>%
  summarise( 'N.conferimenti' = n()) %>%

bind_cols(
  confHAZ() %>%
  summarise( 'Campioni conferiti'  = sum(ncamp_accettati))) %>%

bind_cols(
  confHAZ() %>%
   left_join(nesami, by = "Nconf") %>% ungroup() %>%
   summarise('Esami eseguiti' = sum(n, na.rm = TRUE)))%>%

bind_cols(
   confHAZ() %>%
   mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>%
   summarise('tempo attesa mediano (gg)' = round(median(tempo_attesa_esito, na.rm = TRUE),1)))%>%
   gt()
  )
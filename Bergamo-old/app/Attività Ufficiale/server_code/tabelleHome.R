# Tabelle Home Page----

## Tabella Sanità Animale----

confHSA <- reactive(
  conf %>%
    filter(anno == 2022, settore == "Sanità Animale")
  )

output$t1 <- renderTable(align = "lrrrrr",
  confHSA() %>%
    filter(codaz != "000IX000") %>%
    group_by(ASL) %>% 
    distinct(codaz) %>% 
    #mutate(ASL = gsub(".*- ","",ASL)) %>% 
    mutate(ASL = case_when(
      ASL == "A.T.S. BERGAMO - DISTR. A" ~ "DISTRETTO A",
      ASL == "A.T.S. BERGAMO - DISTR. B" ~ "DISTRETTO B"
    )) %>% 
    summarise('Allevamenti controllati' = n()) %>%
    
    bind_cols(
      confHSA() %>%
        group_by(ASL) %>%
        distinct(Nconf, .keep_all = TRUE) %>%
        summarise('Campioni conferiti'  = sum(ncamp_accettati)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      esami <- confHSA() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>%
        group_by(ASL) %>% 
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      confHSA() %>%
        group_by(ASL) %>% 
        distinct(nconf, .keep_all = TRUE) %>%
        mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
        summarise('gg conferimento' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      confHSA() %>%
      distinct(nconf, .keep_all = TRUE) %>%
      mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
      group_by(ASL) %>% 
      summarise('gg attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>% 
      dplyr::select(-ASL)) %>%
    rename(Distretto = ASL) %>%
    
    adorn_totals(name = "TOTALE ATS", col = 2:4) %>%
    
    gt()
  )


## Tabella Alimenti Uomo----

confHAU <- reactive(
  conf %>% filter(anno == 2022, settore == "Alimenti Uomo")
  )

output$t2 <- renderTable(align = "lrrrrr",
  confHAU() %>%
    group_by(ASL) %>% 
    distinct(nconf) %>% 
    #mutate(ASL = gsub(".*- ","",ASL)) %>% 
    mutate(ASL = case_when(
      ASL == "A.T.S. BERGAMO - DISTR. A" ~ "DISTRETTO A",
      ASL == "A.T.S. BERGAMO - DISTR. B" ~ "DISTRETTO B"
    )) %>% 
    summarise( 'Controlli effettuati' = n()) %>%
    
    bind_cols(
      confHAU() %>%  
      group_by(ASL) %>% 
      distinct(Nconf, .keep_all = TRUE) %>%  
      summarise('Campioni conferiti' = sum(ncamp_accettati)) %>% 
     dplyr::select(-ASL)) %>%
    
    bind_cols(
      esami <- confHAU() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>% 
        group_by(ASL) %>% 
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      confHAU() %>%
        distinct(nconf, .keep_all = TRUE) %>%
        mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
        group_by(ASL) %>% 
        summarise('gg conferimento' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
      dplyr::select(-ASL)) %>% 
    
    bind_cols(
      confHAU() %>%
        distinct(nconf, .keep_all = TRUE) %>%
        mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
        group_by(ASL) %>% 
        summarise('gg attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>% 
        dplyr::select(-ASL)) %>%
    rename(Distretto = ASL) %>%
    
    adorn_totals(name = "TOTALE ATS", col = 2:4) %>%
    
    gt()
  )

## Tabella Alimenti Zootecnici----

confHAZ <- reactive(
  conf %>% filter(anno == 2022, settore == "Alimenti Zootecnici")
  )

output$t3 <- renderTable(align = "lrrrrr",
  confHAZ() %>%
    group_by(ASL) %>%
    distinct(nconf) %>%
    #mutate(ASL = gsub(".*- ","",ASL)) %>% 
    mutate(ASL = case_when(
      ASL == "A.T.S. BERGAMO - DISTR. A" ~ "DISTRETTO A",
      ASL == "A.T.S. BERGAMO - DISTR. B" ~ "DISTRETTO B"
    )) %>% 
    summarise( 'Controlli effettuati' = n()) %>%
    
    bind_cols(
      confHAZ() %>%
        group_by(ASL) %>%
        distinct(Nconf, .keep_all = TRUE) %>%  
        summarise( 'Campioni conferiti' = sum(ncamp_accettati)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      esami <- confHAZ() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>% 
        group_by(ASL) %>% 
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      confHAZ() %>%
        distinct(nconf, .keep_all = TRUE) %>%
        mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
        group_by(ASL) %>% 
        summarise('gg conferimento' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
        dplyr::select(-ASL)) %>%
    
    bind_cols(
      confHAZ() %>%
        distinct(nconf, .keep_all = TRUE) %>%
        mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf))) %>% 
        group_by(ASL) %>% 
        summarise('gg attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>%  
        dplyr::select(-ASL))%>%
    rename(Distretto = ASL) %>%
    
    adorn_totals(name = "TOTALE ATS", col = 2:4) %>%
    
    gt()
  )



 


  
 
 

   
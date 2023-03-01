#tabelle conferimenti ----

tuttiAZ <- reactive(
  confAZ %>% 
    filter(anno == input$anno3, finalita == input$finalita3))


dtAZ <- reactive(
  confAZ %>% 
    filter(anno == input$anno3, ASL == input$distretto3, finalita == input$finalita3) %>% 
    mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel))
)

tuttiAZprove <- reactive(proveAZ %>% 
                              filter(anno.x == input$anno3, 
                                     finalita== input$finalita3))#,
                                     #esiti %in% c("Irr/Pos", "Reg/Neg/n.a.")))

dtproveAZ <- reactive(proveAZ %>% 
                          filter(anno.x == input$anno3, 
                                 finalita == input$finalita3, 
                                 ASL == input$distretto3))#,
                                # esiti %in% c("Irr/Pos", "Reg/Neg/n.a.")))




#t1SicA----
output$t1AZ<- renderTable(
  
  if(input$distretto3 == "Tutti"){ 
    req(input$finalita3)
    tuttiAZ() %>% 
      group_by(motivo_prel, matrice) %>% 
      mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      summarise( 'Controlli effettuati' = n()) %>% 
      
      bind_cols(
        tuttiAZ() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          distinct(Nconf, .keep_all = TRUE) %>%  
          summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
          dplyr::select('Campioni conferiti')
        
      ) %>% 
      
      bind_cols(
        tuttiAZ() %>% 
          left_join(nesami, by = "Nconf") %>% ungroup() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
          dplyr::select('Esami eseguiti') 
        
      ) %>% 
      
      bind_cols(
        tuttiAZprove() %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
          group_by(motivo_prel,  matrice, esiti) %>% 
          summarise(ris = n()) %>%
          pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
          rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
          mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
          dplyr::select(`% esami non conformi`)
      )
    
    
    
    
    
  }else{ 
    
    
    dtAZ() %>% 
      group_by(motivo_prel, matrice) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      summarise( 'Controlli effettuati' = n()) %>% 
      
      
      bind_cols(
        dtAZ() %>% 
          group_by(motivo_prel, matrice) %>% 
          distinct(Nconf, .keep_all = TRUE) %>%  
          summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
          dplyr::select('Campioni conferiti')
        
      ) %>% 
      
      bind_cols(
        dtAZ() %>% 
          left_join(nesami, by = "Nconf") %>% ungroup() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
          dplyr::select('Esami eseguiti') 
      ) %>% 
      
      bind_cols(
        dtproveAZ() %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
          group_by(motivo_prel,  matrice, esiti) %>% 
          summarise(ris = n()) %>%
          pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
          rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
          mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
          dplyr::select(`% esami non conformi`)
        
      )
    
    
    
  }  
  
)


#______________________________________________________________________________________________
#tabelle esami----

# t2SicA----
output$t2AZ <- renderTable(
  
  if(input$distretto3 == "Tutti"){
    
    tuttiAZprove() %>% 
      group_by(nconf, matrice,  prova, esiti) %>% 
      filter(esiti == "Irr/Pos") %>%
      summarise(esami = n()) %>% 
      pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
    
    
    
    
  } else {
    
    
    dtproveAZ() %>%
      group_by(nconf, matrice,  prova, esiti) %>% 
      filter(esiti == "Irr/Pos") %>%
      summarise(esami = n()) %>% 
      pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
    
    
  }
  
  
  
  
)
#____________________________________________________________________________________





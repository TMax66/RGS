#tabelle conferimenti ----

tuttiSicA <- reactive(
  confSicA %>% 
    filter(anno == input$anno2, finalita == input$finalita2))


dtSicA <- reactive(
  confSicA %>% 
    filter(anno == input$anno2, ASL == input$distretto2, finalita == input$finalita2) %>% 
    mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel))
)

tuttiSicAprove <- reactive(proveSicA %>% 
                              filter(anno.x == input$anno2, 
                                     finalita== input$finalita2))#,
                                     #esiti %in% c("Irr/Pos", "Reg/Neg/n.a.")))

dtproveSicA <- reactive(proveSicA %>% 
                          filter(anno.x == input$anno2, 
                                 finalita == input$finalita2, 
                                 ASL == input$distretto2))#,
                                # esiti %in% c("Irr/Pos", "Reg/Neg/n.a.")))




#t1SicA----
output$t1SicA <- renderTable(
  
  if(input$distretto2 == "Tutti"){ 
    req(input$finalita2)
    tuttiSicA() %>% 
      group_by(motivo_prel, matrice) %>% 
      mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      summarise( 'Controlli effettuati' = n()) %>% 
      
      bind_cols(
        tuttiSicA() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          distinct(Nconf, .keep_all = TRUE) %>%  
          summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
          dplyr::select('Campioni conferiti')
        
      ) %>% 
      
      bind_cols(
        tuttiSicA() %>% 
          left_join(nesami, by = "Nconf") %>% ungroup() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
          dplyr::select('Esami eseguiti') 
        
      )
    
    # %>% 
    #   
    #   bind_cols(
    #     tuttiSicAprove() %>% 
    #       mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    #       group_by(motivo_prel,  matrice, esiti) %>% 
    #       summarise(ris = n()) %>%
    #       pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
    #       rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
    #       mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
    #       dplyr::select(`% esami non conformi`)
    #   )
    
    
    
    
    
  }else{ 
    
    
    dtSicA() %>% 
      group_by(motivo_prel, matrice) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      summarise( 'Controlli effettuati' = n()) %>% 
      
      
      bind_cols(
        dtSicA() %>% 
          group_by(motivo_prel, matrice) %>% 
          distinct(Nconf, .keep_all = TRUE) %>%  
          summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
          dplyr::select('Campioni conferiti')
        
      ) %>% 
      
      bind_cols(
        dtSicA() %>% 
          left_join(nesami, by = "Nconf") %>% ungroup() %>% 
          group_by(motivo_prel, matrice) %>% 
          mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
          dplyr::select('Esami eseguiti') 
      ) 
    
    # %>% 
    #   
    #   bind_cols(
    #     dtproveSicA() %>% 
    #       mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    #       group_by(motivo_prel,  matrice, esiti) %>% 
    #       summarise(ris = n()) %>%
    #       pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
    #       rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
    #       mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
    #       dplyr::select(`% esami non conformi`)
    #     
    #   )
    
    
    
  }  
  
)


#______________________________________________________________________________________________
#tabelle esami----

# t2SicA----
output$t2SicA <- renderTable(
  
  if(input$distretto2 == "Tutti"){
    
    tuttiSicAprove() %>% 
      group_by(nconf, matrice,  prova, esiti) %>% 
      filter(esiti == "Irr/Pos") %>%
      summarise(esami = n()) %>% 
      pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
    
    
    
    
  } else {
    
    
    dtproveSicA() %>%
      group_by(nconf, matrice,  prova, esiti) %>% 
      filter(esiti == "Irr/Pos") %>%
      summarise(esami = n()) %>% 
      pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
    
    
  }
  
  
  
  
)
#____________________________________________________________________________________





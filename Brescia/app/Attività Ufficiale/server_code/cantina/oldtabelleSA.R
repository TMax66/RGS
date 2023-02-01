# #tabelle conferimenti ----
# 
# tuttiSA <- reactive(
#   confSA %>% 
#     filter(anno == input$anno, finalita == input$finalita))
# 
# 
# dtSA <- reactive(
#   confSA %>% 
#     filter(anno == input$anno, ASL == input$distretto, finalita == input$finalita) %>% 
#     mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel))
# )
# 
# 
# 
# tuttiSAprove <- reactive(
#   proveSA %>% 
#     filter(anno.x == input$anno, finalita== input$finalita)#, 
#   #esiti %in% c("Irr/Pos", "Reg/Neg/n.a."))
# )
# 
# dtproveSA <- reactive(proveSA %>% 
#                         filter(anno.x == input$anno, 
#                                finalita == input$finalita, ASL == input$distretto)#,  
#                       #esiti %in% c("Irr/Pos", "Reg/Neg/n.a."))
# )
# 
# output$t1SA <- renderTable(
#   
# if(input$distretto == "Tutti"){ 
#   
#   req(input$finalita)
#    
#   
#   
#   tuttiSA() %>% 
#     group_by(motivo_prel, specie, materiale) %>% 
#     mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
#     distinct(nconf, .keep_all = TRUE) %>% 
#     summarise( 'Controlli effettuati' = n()) %>% 
#     
#     bind_cols(
#     tuttiSA() %>% 
#         group_by(motivo_prel, specie, materiale) %>% 
#         mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
#         distinct(Nconf, .keep_all = TRUE) %>%  
#         summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
#         dplyr::select('Campioni conferiti')
#       
#     ) %>% 
# 
#        bind_cols(
#          tuttiSA() %>% 
#            left_join(nesami, by = "Nconf") %>% ungroup() %>% 
#            group_by(motivo_prel, specie, materiale) %>% 
#            mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
#            summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
#            dplyr::select('Esami eseguiti') 
#          
#       ) 
#   
#   # %>% 
#   #     
#   #    bind_cols( 
#   #      
#   #    tuttiSAprove() %>% 
#   #    mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
#   #    group_by(motivo_prel, specie, materiale, esiti) %>% 
#   #    summarise(ris = n()) %>%
#   #    pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
#   #    rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
#   #    mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
#   #             dplyr::select(`% esami non conformi`) 
#   #    ) %>% as.tibble() %>% filter(!is.na("% esami non conformi"))
#     
#   
#   
#   
#   
#   }else{ 
#   
#   
#   dtSA() %>% 
#   group_by(motivo_prel, specie, materiale) %>% 
#   distinct(nconf, .keep_all = TRUE) %>% 
#   summarise( 'Controlli effettuati' = n()) %>% 
# 
#       
# bind_cols(
#   dtSA() %>% 
#     group_by(motivo_prel, specie, materiale) %>% 
#     distinct(Nconf, .keep_all = TRUE) %>%  
#     summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
#     dplyr::select('Campioni conferiti')
#   
# ) %>% 
#       
#     bind_cols(
#       dtSA() %>% 
#       left_join(nesami, by = "Nconf") %>% ungroup() %>% 
#       group_by(motivo_prel, specie, materiale) %>% 
#       mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
#       summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
#       dplyr::select('Esami eseguiti') 
#       ) 
    
    # %>% 
    #   
    #   bind_cols(
    #     
    #     dtproveSA() %>% 
    #       mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    #       group_by(motivo_prel, specie, materiale, esiti) %>% 
    #       summarise(ris = n()) %>%
    #       pivot_wider(names_from = "esiti", values_from = "ris", values_fill = 0) %>%
    #       rename(P = `Irr/Pos`, N = `Reg/Neg/n.a.`) %>% 
    #       mutate("% esami non conformi" = round(100*(P / (P+N)),1)) %>% ungroup() %>% 
    #       dplyr::select(`% esami non conformi`)
    # 
    #   )
    
    
    
#   }  
# 
# )

#tabelle esami----

# tuttiSAprove <- reactive( proveSA %>% 
#   filter(anno.x == 2022, finalita== input$finalita))

# dtproveSA <- reactive(proveSA %>% 
#                         filter(anno.x == 2022, finalita == input$finalita, ASL == input$distretto))



# output$t2SA <- renderTable(
#   
#   if(input$distretto == "Tutti"){
#     
#     tuttiSAprove() %>% 
#       group_by(nconf, specie,  prova, esiti) %>%  
#       filter(esiti == "Irr/Pos") %>%  
#       summarise(esami = n()) %>% 
#       pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
#       
#   } else {
#     
#     
#    dtproveSA() %>%
#       group_by(nconf, specie,  prova, esiti) %>%  
#       filter(esiti == "Irr/Pos") %>%  
#       summarise(esami = n()) %>% 
#       pivot_wider(names_from = "esiti", values_from = "esami", values_fill = 0)
#     
#   }
# 
# )

# tabelle ricerca codice aziendale----
 


## tabella principale di sintesi----
summarycodaz <- reactive(confSA %>%
  filter(codaz == input$codiceall) %>% 
  dplyr::select(codaz, nconf, finalita, dtprel, 
                dtconf, dtreg, specie, materiale, 
                conferente, proprietario, 
                veterinario,  comune, ncamp_accettati)
)




output$t3SA <- DT::renderDataTable(summarycodaz(), 
                                       server = TRUE, 
                                       rownames = FALSE,
                                       options = list(
                                         dom = 'Bfrtip',pageLength = 5) )


# ## tabella di drilldown----

azcodedrill <- reactive({
  shiny::validate(
    need(length(input$t3SA_rows_selected) > 0, "Seleziona il conferimento per vedere i dettagli delle prove")
  )
   
   select_conf <- summarycodaz()[as.integer(input$t3SA_rows_selected), ]$nconf
  
  confSA %>% filter(nconf == select_conf) %>%
    left_join(proveSA, by = c("nconf")) %>% 
    dplyr::select( codaz, nconf, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
})
 
output$cadrill <- DT::renderDataTable(azcodedrill(),
                                     server = TRUE,
                                     rownames = FALSE,
                                     options = list(
                                     dom = 'Bfrtip',pageLength = 5)
)


# ## tabella principale di sintesi----
# summary_Siero <- siero %>% distinct(nconf, .keep_all = TRUE) %>%
#   dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale, conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)
# 
# 
# 
# 
# output$SumSiero <- DT::renderDataTable(summary_Siero,
#                                        server = TRUE,
#                                        rownames = FALSE,
#                                        options = list(
#                                          dom = 'Bfrtip',pageLength = 5) )
# 
# 
# ## tabella di drilldown----
# 
# Sdrill <- reactive({
#   shiny::validate(
#     need(length(input$SumSiero_rows_selected) > 0, "Select rows to drill down!")
#   )
# 
#   select_conf <- summary_Siero[as.integer(input$SumSiero_rows_selected), ]$nconf
# 
#   siero %>% filter(nconf == select_conf) %>%
#     dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
# })
# 
# output$sdrill <- DT::renderDataTable(Sdrill(),
# 
#                                      server = TRUE,
#                                      rownames = FALSE,
#                                      options = list(
#                                        dom = 'Bfrtip',pageLength = 5)
# )

# Accettazione-----------------------------------------------------------------------





# Laboratorio sierologia----------------------------------------------------------------------------
dtS<-reactive(siero %>%  
  group_by( prova, tecnica) %>% 
  count()
)



output$St1 <- renderTable(
  dtS() %>% 
    
  bind_cols(  
    
  siero %>%
  mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>% 
  group_by(prova, tecnica) %>%
  summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>% ungroup() %>% 
  dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
  
  ) %>% 
adorn_totals(col = 3) %>% 
  gt()

)
  
  


## tabella principale di sintesi----
summary_Siero <- siero %>% distinct(nconf, .keep_all = TRUE) %>% 
  dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale, conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)


   

output$SumSiero <- DT::renderDataTable(summary_Siero, 
                                       server = TRUE, 
                                       rownames = FALSE,
                                       options = list(
                                         dom = 'Bfrtip',pageLength = 5) )


## tabella di drilldown----

Sdrill <- reactive({
  shiny::validate(
    need(length(input$SumSiero_rows_selected) > 0, "Select rows to drill down!")
  )
  
  select_conf <- summary_Siero[as.integer(input$SumSiero_rows_selected), ]$nconf
  
  siero %>% filter(nconf == select_conf) %>% 
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
})

output$sdrill <- DT::renderDataTable(Sdrill(),
                                     
                                     server = TRUE, 
                                     rownames = FALSE,
                                     options = list(
                                       dom = 'Bfrtip',pageLength = 5) 
                                     )

#_________________________________________________________________________________________________
#Laboratorio Diagnostica Generale----

 
dtD<-reactive(diagnostica %>%  
                group_by( prova) %>% 
                count()
)



output$Dt1 <- renderTable(
  dtD() %>% 
    
    bind_cols(  
      
      diagnostica %>%
        mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>% 
        group_by(prova) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>% ungroup() %>% 
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
      
    ) %>% 
    top_n(30) %>% 
    arrange(desc(n)) %>% 
    adorn_totals(col = 2) %>% 
    gt()
  
)


## tabella principale di sintesi----
summary_Diagnostica <- diagnostica %>% distinct(nconf, .keep_all = TRUE) %>% 
  dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale, conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)




output$SumDiagn <- DT::renderDataTable(summary_Diagnostica, 
                                       server = TRUE, 
                                       rownames = FALSE,
                                       options = list(
                                         dom = 'Bfrtip',pageLength = 5) )


## tabella di drilldown----

Ddrill <- reactive({
  shiny::validate(
    need(length(input$SumDiagn_rows_selected) > 0, "Select rows to drill down!")
  )
  
  select_conf <- summary_Diagnostica[as.integer(input$SumDiagn_rows_selected), ]$nconf
  
  diagnostica %>% filter(nconf == select_conf) %>% 
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
})

output$ddrill <- DT::renderDataTable(Ddrill(),
                                     
                                     server = TRUE, 
                                     rownames = FALSE,
                                     options = list(
                                       dom = 'Bfrtip',pageLength = 5) 
)

#_________________________________________________________________________________________________
#Laboratorio Microbiologia Alimenti----

dtA<-reactive(alimenti %>%  
                group_by( prova ) %>% 
                count()
)



output$MAt1 <- renderTable(
  dtA() %>% 
    
    bind_cols(  
      
      alimenti %>%
        mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>% 
        group_by(prova ) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>% ungroup() %>% 
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
      
    ) %>% 
    top_n(30) %>% 
    arrange(desc(n)) %>% 
    adorn_totals(col = 2) %>% 
    gt()
  
)


## tabella principale di sintesi----
summary_alimenti <- alimenti %>% distinct(nconf, .keep_all = TRUE) %>% 
  dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale, conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)




output$SumMA <- DT::renderDataTable(summary_alimenti, 
                                       server = TRUE, 
                                       rownames = FALSE,
                                       options = list(
                                         dom = 'Bfrtip',pageLength = 5) )


## tabella di drilldown----

MAdrill <- reactive({
  shiny::validate(
    need(length(input$SumMA_rows_selected) > 0, "Select rows to drill down!")
  )
  
  select_conf <- summary_alimenti[as.integer(input$SumMA_rows_selected), ]$nconf
  
  alimenti %>% filter(nconf == select_conf) %>% 
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
})

output$madrill <- DT::renderDataTable(MAdrill(),
                                     
                                     server = TRUE, 
                                     rownames = FALSE,
                                     options = list(
                                       dom = 'Bfrtip',pageLength = 5) 
)



#_________________________________________________________________________________________________
#Laboratorio Biologia Molecolare----

# dtbiom<-reactive(biomol %>%  
#                 group_by( prova) %>% 
#                 count()
# )
# 
# 
# 
# output$bmt1 <- renderTable({   
#   dtbiom() %>% 
#     
#     bind_cols(  
#       
#       biomol %>%
#         mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>% 
#         # filter(tempo_esecuzione >= 0) %>% 
#         group_by(prova) %>%
#         summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>% ungroup() %>%  
#         dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
#       
#     ) %>% 
#     filter(`tempo medio di esecuzione (gg)` >= 0) %>% 
#     top_n(30) %>% 
#     arrange(desc(n)) %>%  
#     adorn_totals(col = 2)%>% 
#     gt()
#   
#   
# })
# 
# ## tabella principale di sintesi----
# summary_biomol <- biomol %>% distinct(nconf, .keep_all = TRUE) %>% 
#   dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale, conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)
# 
# 
# 
# 
# output$Sumbm <- DT::renderDataTable(summary_biomol, 
#                                     server = TRUE, 
#                                     rownames = FALSE,
#                                     options = list(
#                                       dom = 'Bfrtip',pageLength = 5) )
# 
# 
# ## tabella di drilldown----
# 
# biomoldrill <- reactive({
#   shiny::validate(
#     need(length(input$Sumbm_rows_selected) > 0, "Select rows to drill down!")
#   )
#   
#   select_conf <- summary_biomol[as.integer(input$Sumbm_rows_selected), ]$nconf
#   
#   biomol %>% filter(nconf == select_conf) %>% 
#     dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
# })
# 
# output$bmdrill <- DT::renderDataTable(biomoldrill(),
#                                       
#                                       server = TRUE, 
#                                       rownames = FALSE,
#                                       options = list(
#                                       dom = 'Bfrtip',pageLength = 5) 
# )
# 
# 


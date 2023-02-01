# Accettazione-----------------------------------------------------------------------




#_________________________________________________________________________________________________
# Laboratorio sierologia----
dtS <- reactive({
  siero %>%
    group_by(prova, tecnica) %>%
    count()
  })


output$St1 <- renderTable({
  dtS() %>%
    bind_cols(
      siero %>%
        mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>%
        group_by(prova, tecnica) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
        ungroup() %>%
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
      ) %>%
    adorn_totals(col = 3) %>% 
    gt()
  })
  
  

## tab principale di sintesi----
summary_Siero <- siero %>%
  distinct(nconf, .keep_all = TRUE) %>%
  dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale,
                conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)


output$SumSiero <- DT::renderDataTable(server = TRUE,{
  summary_Siero %>%
    DT::datatable(
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
      )
  })


## tab di drilldown----

Sdrill <- reactive({
  shiny::validate(
    need(length(input$SumSiero_rows_selected) > 0, "Select rows to drill down!")
    )
  
  select_conf <- summary_Siero[as.integer(input$SumSiero_rows_selected), ]$nconf
  
  siero %>%
    filter(nconf == select_conf) %>%
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
  })

output$sdrill <- DT::renderDataTable(server = TRUE,{
  Sdrill() %>% 
    DT::datatable(
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
    )
  })

#_________________________________________________________________________________________________
# #Lab Diagnostica Generale----
# 
dtD <- reactive({
  diagnostica %>%
    group_by(prova) %>%
    count()
  })


output$Dt1 <- renderDataTable({
  dtD() %>%
    bind_cols(
      diagnostica %>%
        mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>%
        group_by(prova) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
        ungroup() %>%
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
      ) %>%
    # top_n(30) %>%
    arrange(desc(n)) %>%
    # adorn_totals(col = 2) %>%
    datatable(style = 'bootstrap',
              rownames = FALSE,
              selection = 'none',
              options = list(
                class = 'compact row-border',
                dom = 'tip',
                pageLength = 10)
    )
  })
# 
# ## tab principale di sintesi----
# summary_Diagnostica <- diagnostica %>%
#   distinct(nconf, .keep_all = TRUE) %>%
#   dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale,
#                 conferente, proprietario, veterinario, codaz, comune, NrCampioni)


# output$SumDiagn <- DT::renderDataTable(server = TRUE,{
#   summary_Diagnostica %>% 
#     DT::datatable(
#       rownames = FALSE,
#       options = list(
#         dom = 'Bfrtip',
#         pageLength = 5)
#       )
#   })
# 
# 
# ## tab di drilldown----
# 
prove_diagn <- reactive({
  shiny::validate(
    need(length(input$table_diagn_rows_selected) > 0, "")
    )

  select_conf <- conf_diagn()[as.integer(input$table_diagn_rows_selected), ]$nconf

  diagnostica %>%
    filter(nconf == select_conf) %>%
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore) %>% 
    DT::datatable(
      style = 'bootstrap',
      rownames = FALSE,
      selection = 'none',
      options = list(
        class = 'compact row-border',
        dom = 'tip',
        pageLength = 5)
      ) %>% 
    formatDate(
      columns = c(5, 6), 
      method =  "toLocaleDateString", 
      params = list(
        'it-IT', 
        list(
          year = 'numeric', 
          month = 'numeric',
          day = 'numeric')
      )
    )
  })

output$drill_diagn <- DT::renderDataTable(server = FALSE,{
  prove_diagn()
  })



# #_________________________________________________________________________________________________


#Laboratorio Microbiologia Alimenti----

dtA<-reactive({
  alimenti %>%
    group_by(prova) %>%
    count()
  })


output$MAt1 <- renderTable({
  dtA() %>%
    bind_cols(
      alimenti %>%
        mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400)) %>% 
        group_by(prova) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
        ungroup() %>% 
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)
      ) %>%
    top_n(30) %>% 
    arrange(desc(n)) %>% 
    adorn_totals(col = 2) %>% 
    gt()
  })


## tab principale di sintesi----
summary_alimenti <- alimenti %>%
  distinct(nconf, .keep_all = TRUE) %>% 
  dplyr::select(nconf, finalita, dtprel, dtconf, dtreg, specie, materiale,
                conferente, proprietario, veterinario, codaz, comune, ncamp_accettati)




output$SumMA <- DT::renderDataTable(server = TRUE,{
  summary_alimenti %>%
    DT::datatable(
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
    )
  })


## tab di drilldown----

MAdrill <- reactive({
  shiny::validate(
    need(length(input$SumMA_rows_selected) > 0, "Select rows to drill down!")
    )
  
  select_conf <- summary_alimenti[as.integer(input$SumMA_rows_selected), ]$nconf
  
  alimenti %>%
    filter(nconf == select_conf) %>% 
    dplyr::select(nconf, codaz, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
  })

output$madrill <- DT::renderDataTable(server = TRUE,{
  MAdrill() %>% 
    DT::datatable(
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
    )
  })



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
## tabella principale di sintesi----
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
## tabella di drilldown----
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


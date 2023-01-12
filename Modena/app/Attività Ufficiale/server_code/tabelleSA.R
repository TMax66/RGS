totcampionamento <- reactive(
  confSA %>% 
    filter(anno == input$anno))


campionamento <- reactive(
  confSA %>% 
    filter(anno == input$anno, finalita == input$finalita))


#tabella summary ----


totsummarycamp <- reactive({
  totcampionamento() %>% 
    group_by(ASL) %>% 
    # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    distinct(nconf, .keep_all = TRUE) %>% 
    summarise( 'Controlli effettuati' = n()) %>% 
    
    bind_cols(
      totcampionamento() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        distinct(Nconf, .keep_all = TRUE) %>%  
        summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
        dplyr::select('Campioni conferiti')) %>% 
    
    bind_cols(
      totcampionamento() %>% 
        left_join(nesami, by = "Nconf") %>% ungroup() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
        dplyr::select('Esami eseguiti')) %>% 
    adorn_totals(name = "TOTALE AUSL", col = 2:4)
})


summaryCamp <- reactive({  
    campionamento() %>% 
      group_by(ASL) %>% 
      # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      summarise( 'Controlli effettuati' = n()) %>% 
      
      bind_cols(
        campionamento() %>% 
          group_by(ASL) %>% 
          # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          distinct(Nconf, .keep_all = TRUE) %>%  
          summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
          dplyr::select('Campioni conferiti')) %>% 
      
      bind_cols(
        campionamento() %>% 
          left_join(nesami, by = "Nconf") %>% ungroup() %>% 
          group_by(ASL) %>% 
          # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
          summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
          dplyr::select('Esami eseguiti')) %>%
    
    adorn_totals(name = "TOTALE AUSL", col = 2:4)
  
})



output$t1SA <- DT::renderDataTable(
  
  if(input$finalita == " ")  
    
    { totsummarycamp() %>% 
        mutate(ASL = gsub(".*- ","", ASL)) %>%
        rename(Distretto = ASL) %>% 
        datatable(rownames = FALSE, 
                  # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
                  #                                   input$finalita),
                  style = 'bootstrap',
                  selection = 'single',
                  options = list(dom = 't'))
      
      
    }else{
      
   
  summaryCamp() %>% 
    mutate(ASL = gsub(".*- ","", ASL)) %>%
    rename(Distretto = ASL) %>% 
    datatable(rownames = FALSE, 
              # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
              #                                   input$finalita),
              style = 'bootstrap',
              selection = 'single',
              options = list(dom = 't'))
    
    }
)


# tabella drill down

ASLdrill <- reactive({
  shiny::validate(
    need(length(input$t1SA_rows_selected) > 0, "Seleziona il DISTRETTO per vedere il dettaglio delle attività")
  )
  
  
  select_asl <- summaryCamp()[as.integer(input$t1SA_rows_selected), ]$ASL
  
  #tabella totale----
  
  if(select_asl == "TOTALE AUSL"){  
    confSA %>%
      filter(finalita == input$finalita, anno == input$anno) %>%
      left_join(proveSA, by = c("nconf")) %>%
      mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
             dtfine_chr = format(dtfine, "%d/%m/%Y"),
             nconf_chr = as.character(nconf),
             numero_del_campione = as.numeric(numero_del_campione),
             numero_del_campione_chr = as.character(numero_del_campione),
             ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
             specie = str_to_sentence(specie.x),
             materiale = str_to_sentence(materiale.x)) %>% 
      dplyr::select(nconf, nconf_chr, verbale, codaz, dtinizio, dtfine, dtinizio_chr,
                    dtfine_chr, specie, materiale, prova, tecnica,
                    numero_del_campione, numero_del_campione_chr, "esito" = valore, "distretto" = ASL) %>% 
      DT::datatable(
        style = 'bootstrap', 
        escape = FALSE,
        colnames = c("conferimento_0",#0
                     "conferimento",#1
                     "verbale",#2
                     "cod.azienda",###
                     "inizio_0",#3
                     "fine_0",#4
                     "inizio",#5
                     "fine",#6
                     "specie",#7
                     "materiale",#8
                     "prova",#9
                     "tecnica",#10
                     "campione_0",#11
                     "campione",#12
                     "esito",#13
                     "distretto"),#14
        #server = FALSE,
        rownames = FALSE,
        selection = 'none',
        extensions = 'Buttons',
        filter = c('top'),
        callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
        #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
        #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
        options = list(
          order = list(list(4, 'desc'), list(5, 'desc'), list(0, 'desc'), list(12, 'asc')),
          dom = 'Brltip',
          searching = TRUE,
          autowidth = FALSE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, 100, -1), 
                            c('10', '25', '50', '100', 'Tutti')),
          buttons = list(
            list(extend = "excel", text = "Scarica Tutto",
                 filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
                 title = NULL,
                 titleAttr = "Excel",
                 exportOptions = list(
                   modifier = list(page = "all"),
                   columns = c(1,2,3,6,7,8,9,10,11,13,14,15))),
            list(extend = "excel", text = "Scarica Selezione",
                 filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
                 title = NULL,
                 titleAttr = "Excel",
                 exportOptions = list(
                   modifier = list(page = "current"),
                   columns = c(1,2,3,6,7,8,9,10,11,13,14,15)))
          ),    
          columnDefs = list(
            list(orderData = 4, targets = 6),
            list(orderData = 5, targets = 7),
            list(orderData = 0, targets = 1),
            list(orderData = 12, targets = 13),
            list(visible = FALSE, targets = c(0, 4, 5, 12)),
            list(className = 'dt-body-right', targets = c(6, 7, 13)),
            list(className = 'dt-body-center', targets = c(1)),
            list(className = 'dt-head-center', targets = "_all"),
            list(width = '10px', targets =c(1, 2, 3, 13)),
            list(width = '60px', targets =c(6, 7))
            #   list(width = '80px', targets =c(5)),
            #   list(width = '80px', targets =c(6)),      
            #   list(width = '100px', targets =c(7)),
            #   list(className = 'dt-body-right', targets = c(5,6)),
            #   list(targets = c(1,2,3,4), className = 'dt-middle')
          ),
          language = list(search = "Cerca: ",
                          lengthMenu = "Mostra _MENU_ esami",
                          paginate = list(previous = "Precedente", `next` = "Successiva"),
                          info = "_START_ - _END_ di _TOTAL_ esami",
                          infoFiltered = "(su un totale di _MAX_ esami)",
                          infoEmpty = "Nessun esame disponibile",
                          zeroRecords = "---")        
        )
      )
    
    
    #tabella per distretti----
    
  } else {
    confSA %>%
      filter(ASL == select_asl, finalita == input$finalita, anno == input$anno) %>%
      left_join(proveSA, by = c("nconf")) %>%
      mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
             dtfine_chr = format(dtfine, "%d/%m/%Y"),
             nconf_chr = as.character(nconf),
             numero_del_campione = as.numeric(numero_del_campione),
             numero_del_campione_chr = as.character(numero_del_campione),
             ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
             specie = str_to_sentence(specie.x),
             materiale = str_to_sentence(materiale.x)) %>% 
      dplyr::select(nconf, nconf_chr, verbale, codaz, dtinizio, dtfine, dtinizio_chr,
                    dtfine_chr, specie, materiale, prova, tecnica,
                    numero_del_campione, numero_del_campione_chr, "esito" = valore) %>% 
      DT::datatable(
        style = 'bootstrap', 
        colnames = c("conferimento_0",#0
                     "conferimento",#1
                     "verbale",#2
                     "cod.azienda",###
                     "inizio_0",#3
                     "fine_0",#4
                     "inizio",#5
                     "fine",#6
                     "specie",#7
                     "materiale",#8
                     "prova",#9
                     "tecnica",#10
                     "campione_0",#11
                     "campione",#12
                     "esito"),#13
        #server = FALSE,
        rownames = FALSE,
        selection = 'none',
        extensions = 'Buttons',
        filter = c('top'),
        callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
        options = list(
          order = list(list(4, 'desc'), list(5, 'desc'), list(0, 'desc'), list(12, 'asc')),
          dom = 'Brltip',
          searching = TRUE,
          autowidth = FALSE,
          pageLength = 10,
          lengthMenu = list(c(10, 25, 50, 100, -1), 
                            c('10', '25', '50', '100', 'Tutti')),
          buttons = list(
            list(extend = "excel", text = "Scarica Tutto",
                 filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
                 title = NULL,
                 titleAttr = "Excel",
                 exportOptions = list(
                   modifier = list(page = "all"),
                   columns = c(1,2,3,6,7,8,9,10,11,13,14))),
            list(extend = "excel", text = "Scarica Selezione",
                 filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
                 title = NULL,
                 titleAttr = "Excel",
                 exportOptions = list(
                   modifier = list(page = "current"),
                   columns = c(1,2,3,6,7,8,9,10,11,13,14)))
          ),    
          columnDefs = list(
            list(orderData = 4, targets = 6),
            list(orderData = 5, targets = 7),
            list(orderData = 0, targets = 1),
            list(orderData = 12, targets = 13),
            list(visible = FALSE, targets = c(0, 4, 5, 12)),
            list(className = 'dt-body-right', targets = c(6, 7, 13)),
            list(className = 'dt-body-center', targets = c(1)),
            list(className = 'dt-head-center', targets = "_all"),
            list(width = '10px', targets =c(1, 2, 3, 13)),
            list(width = '60px', targets =c(6, 7))
            #   list(width = '80px', targets =c(5)),
            #   list(width = '80px', targets =c(6)),      
            #   list(width = '100px', targets =c(7)),
            #   list(className = 'dt-body-right', targets = c(5,6)),
            #   list(targets = c(1,2,3,4), className = 'dt-middle')
          ),
          language = list(search = "Cerca: ",
                          lengthMenu = "Mostra _MENU_ esami",
                          paginate = list(previous = "Precedente", `next` = "Successiva"),
                          info = "_START_ - _END_ di _TOTAL_ esami",
                          infoFiltered = "(su un totale di _MAX_ esami)",
                          infoEmpty = "Nessun esame disponibile",
                          zeroRecords = "---")        
        )
      )
  }
  
})

#output----

output$asldrill <- DT::renderDataTable(server = FALSE,{
  ASLdrill()
})




#RICERCA CODICE ALLEVAMENTO ----
## tabella principale di sintesi----

summarycodaz <- reactive({
  confSA %>%
    filter(codaz == input$codiceall) %>%
    dplyr::select(codaz, verbale, nconf, finalita, dtprel, dtconf, dtreg,
                  specie, materiale, conferente, proprietario,
                  veterinario, comune, ncamp_accettati) %>% 
    mutate(dtprel = format(dtprel, "%d/%m/%Y"),
           dtconf = format(dtconf, "%d/%m/%Y"), 
           dtreg = format(dtreg,"%d/%m/%Y" ))
})




output$t3SA <- DT::renderDataTable(
  summarycodaz() %>% 
  datatable (rownames = FALSE,
             escape = FALSE,
  selection = 'single',
  extensions = 'Buttons',
  filter = c('top'),
  callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
  options = list(
    order = list(list(4, 'desc'), list(5, 'desc'), list(0, 'desc'), list(12, 'asc')),
    dom = 'Bfrtip',
    searching = TRUE,
    autowidth = FALSE,
    pageLength = 10,
    lengthMenu = list(c(10, 25, 50, 100, -1), 
                      c('10', '25', '50', '100', 'Tutti')),
    buttons = list(
      list(extend = "excel", text = "Scarica Tutto",
          # filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
           title = NULL,
           titleAttr = "Excel",
           exportOptions = list(
             modifier = list(page = "all"))#,
             #columns = c(1,2,3,6,7,8,9,10,11,13,14)))
    ))
    )
    

)
)


## tabella di drilldown----

azcodedrill <- reactive({
  shiny::validate(
    need(length(input$t3SA_rows_selected) > 0, "Seleziona il conferimento per vedere i dettagli delle prove")
  )
  
  select_conf <- summarycodaz()[as.integer(input$t3SA_rows_selected), ]$nconf
  
  confSA %>% filter(nconf == select_conf) %>%
    left_join(proveSA, by = c("nconf")) %>% 
    dplyr::select(codaz, nconf, prova, tecnica, dtinizio, dtfine, numero_del_campione, "esito" = valore)
})

output$cadrill <- DT::renderDataTable(azcodedrill(),
                                      server = FALSE,
                                      rownames = FALSE,
                                      options = list(
                                      dom = 'Bfrtip',
                                      pageLength = 5)
)



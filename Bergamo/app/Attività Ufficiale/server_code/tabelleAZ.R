azot <- reactive({
  if(input$finalita3 == "Tutte le finalità") {
    confAZ %>% 
      filter(anno == input$anno3)
  } else {
    confAZ %>% 
      filter(anno == input$anno3, finalita == input$finalita3)
  }
})


#tabella summary ----

summaryazot <- reactive({  
  azot() %>% 
    group_by(ASL) %>% 
    # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    distinct(nconf, .keep_all = TRUE) %>% 
    summarise( 'Controlli effettuati' = n()) %>%
    
    bind_cols(
     azot() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        distinct(Nconf, .keep_all = TRUE) %>%  
        summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
        dplyr::select('Campioni conferiti')) %>% 
    
    bind_cols(
      azot() %>% 
        left_join(nesami, by = "Nconf") %>% ungroup() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
        dplyr::select('Esami eseguiti')) %>%
    
    adorn_totals(name = "TOTALE ATS", col = 2:4)
  
})



output$t1AZ <- DT::renderDataTable(
  summaryazot() %>% 
  mutate(ASL = gsub(".*- ","", ASL)) %>%
  rename(Distretto = ASL) %>% 
  datatable(rownames = FALSE,
            # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
            #                                   input$finalita3),
            style = 'bootstrap',
            selection = 'single',
            options = list(dom = 't'))
  )


# tabella drill down

ASLdrillaz <- reactive({
  shiny::validate(
    need(length(input$t1AZ_rows_selected) > 0, "Seleziona la FINALITÀ e il DISTRETTO per vedere il dettaglio delle attività")
  )
  
  
select_aslaz <- summaryazot()[as.integer(input$t1AZ_rows_selected), ]$ASL
  
#tabella totale----

if(select_aslaz == "TOTALE ATS"){  
  confAZ %>%
    filter(finalita == input$finalita3, anno == input$anno3) %>%
    left_join(proveAZ, by = c("nconf")) %>%
    mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
           dtfine_chr = format(dtfine, "%d/%m/%Y"),
           nconf_chr = as.character(nconf),
           numero_del_campione = as.numeric(numero_del_campione),
           numero_del_campione_chr = as.character(numero_del_campione),
           ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
           specie = str_to_sentence(specie)) %>% 
    dplyr::select(nconf, nconf_chr, verbale, dtinizio, dtfine, dtinizio_chr,
                  dtfine_chr, specie, materiale, prova, tecnica,
                  numero_del_campione, numero_del_campione_chr, "esito" = valore, "distretto" = ASL) %>% 
    DT::datatable(
      style = 'bootstrap', 
      escape = FALSE,
      colnames = c("conferimento_0",#0
                   "conferimento",#1
                   "verbale",#2
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
        order = list(list(3, 'desc'), list(4, 'desc'), list(0, 'desc'), list(11, 'asc')),
        dom = 'Brltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 5,
        lengthMenu = list(c(5, 25, 50, 100, -1), 
                          c('5', '25', '50', '100', 'Tutti')),
        buttons = list(
          list(extend = "excel", text = "Scarica Tutto",
               filename = paste0(input$finalita3, " - attività ufficiale ", input$anno3, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "all"),
                 columns = c(1,2,5,6,7,8,9,10,12,13,14))),
          list(extend = "excel", text = "Scarica Selezione",
               filename = paste0(input$finalita3, " - attività ufficiale ", input$anno3, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "current"),
                 columns = c(1,2,5,6,7,8,9,10,12,13,14)))
        ),    
        columnDefs = list(
          list(orderData = 3, targets = 5),
          list(orderData = 4, targets = 6),
          list(orderData = 0, targets = 1),
          list(orderData = 11, targets = 12),
          list(visible = FALSE, targets = c(0, 3, 4, 11)),
          list(className = 'dt-body-right', targets = c(5, 6, 12)),
          list(className = 'dt-body-center', targets = c(1)),
          list(className = 'dt-head-center', targets = "_all"),
          list(width = '10px', targets =c(1, 12)),
          list(width = '30px', targets =c(2)),
          list(width = '60px', targets =c(5, 6))
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
  confAZ %>%
    filter(ASL == select_aslaz, finalita == input$finalita3, anno == input$anno3) %>%
    left_join(proveAZ, by = c("nconf")) %>%
    mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
           dtfine_chr = format(dtfine, "%d/%m/%Y"),
           nconf_chr = as.character(nconf),
           numero_del_campione = as.numeric(numero_del_campione),
           numero_del_campione_chr = as.character(numero_del_campione),
           ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
           specie = str_to_sentence(specie)) %>% 
    dplyr::select(nconf, nconf_chr, verbale, dtinizio, dtfine, dtinizio_chr,
                  dtfine_chr, specie, materiale, prova, tecnica,
                  numero_del_campione, numero_del_campione_chr, "esito" = valore) %>% 
    DT::datatable(
      style = 'bootstrap', 
      colnames = c("conferimento_0",#0
                   "conferimento",#1
                   "verbale",#2
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
        order = list(list(3, 'desc'), list(4, 'desc'), list(0, 'desc'), list(11, 'asc')),
        dom = 'Brltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 5,
        lengthMenu = list(c(5, 25, 50, 100, -1), 
                          c('5', '25', '50', '100', 'Tutti')),
        buttons = list(
          list(extend = "excel", text = "Scarica Tutto",
               filename = paste0(input$finalita3, " - attività ufficiale ", input$anno3, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "all"),
                 columns = c(1,2,5,6,7,8,9,10,12,13))),
          list(extend = "excel", text = "Scarica Selezione",
               filename = paste0(input$finalita3, " - attività ufficiale ", input$anno3, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "current"),
                 columns = c(1,2,5,6,7,8,9,10,12,13)))
        ),    
        columnDefs = list(
          list(orderData = 3, targets = 5),
          list(orderData = 4, targets = 6),
          list(orderData = 0, targets = 1),
          list(orderData = 11, targets = 12),
          list(visible = FALSE, targets = c(0, 3, 4, 11)),
          list(className = 'dt-body-right', targets = c(5, 6, 12)),
          list(className = 'dt-body-center', targets = c(1)),
          list(className = 'dt-head-center', targets = "_all"),
          list(width = '10px', targets =c(1, 12)),
          list(width = '30px', targets =c(2)),
          list(width = '60px', targets =c(5, 6))
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

output$asldrillalimZot <- DT::renderDataTable(server = FALSE,{
  ASLdrillaz()
})


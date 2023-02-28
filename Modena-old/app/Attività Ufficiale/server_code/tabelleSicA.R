alimenti <- reactive({
  if(input$finalita2 == "Tutte le finalità") {
    confSicA %>% 
      filter(anno == input$anno)
  } else {
    confSicA %>% 
      filter(anno == input$anno, finalita == input$finalita2)
  }
})

#tabella summary----

summaryalim <- reactive({  
  alimenti() %>% 
    group_by(ASL) %>% 
    # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
    distinct(nconf, .keep_all = TRUE) %>% 
    summarise( 'Controlli effettuati' = n()) %>% 
    
    bind_cols(
      alimenti() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        distinct(Nconf, .keep_all = TRUE) %>%  
        summarise( 'Campioni conferiti'  = sum(ncamp_accettati)) %>% ungroup() %>% 
        dplyr::select('Campioni conferiti')) %>% 
    
    bind_cols(
      alimenti() %>% 
        left_join(nesami, by = "Nconf") %>% ungroup() %>% 
        group_by(ASL) %>% 
        # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))%>% ungroup() %>% 
        dplyr::select('Esami eseguiti')) %>%
    
    adorn_totals(name = "TOTALE AUSL", col = 2:4)
  
  })



output$t1SicA <- DT::renderDataTable({
  
  if (req(input$finalita2) == "Tutte le finalità") {
    
    summaryalim() %>%
      mutate(ASL = gsub(".*- ","", ASL)) %>%
      rename(Distretto = ASL) %>%
      datatable(rownames = FALSE,
                # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
                #                                   input$finalita),
                style = 'bootstrap',
                selection = 'none',
                options = list(dom = 't'))
    
  } else {  
    
    summaryalim() %>%
      mutate(ASL = gsub(".*- ","", ASL)) %>%
      rename(Distretto = ASL) %>%
      datatable(rownames = FALSE,
                # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
                #                                   input$finalita),
                style = 'bootstrap',
                selection = 'single',
                options = list(dom = 't'))
    
  }
  
})

# tabella drill down

ASLdrillalim <- reactive({
  shiny::validate(
    need(length(input$t1SicA_rows_selected) > 0, "Seleziona la FINALITÀ e il DISTRETTO per vedere il dettaglio delle attività")
  )
  
  
select_aslalim <- summaryalim()[as.integer(input$t1SicA_rows_selected), ]$ASL
  
  
#tabella totale----

if(select_aslalim == "TOTALE AUSL"){  
  confSicA %>%
    filter(finalita == input$finalita2, anno == input$anno) %>%
    left_join(proveSicA, by = c("nconf")) %>%
    mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
           dtfine_chr = format(dtfine, "%d/%m/%Y"),
           nconf_chr = as.character(nconf),
           numero_del_campione = as.numeric(numero_del_campione),
           numero_del_campione_chr = as.character(numero_del_campione),
           ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
           #specie = str_to_sentence(specie),
           matrice = str_to_sentence(matrice.x)) %>% 
    dplyr::select(nconf, nconf_chr, verbale, dtinizio, dtfine, dtinizio_chr,
                  dtfine_chr, matrice, prova, tecnica,
                  numero_del_campione, numero_del_campione_chr, "esito" = valore, "distretto" = ASL) %>% 
    DT::datatable(
      style = 'bootstrap', 
      colnames = c("conferimento_0",#0
                   "conferimento",#1
                   "verbale",#2
                   "inizio_0",#3
                   "fine_0",#4
                   "inizio",#5
                   "fine",#6
                   "matrice",#7
                   "prova",#8
                   "tecnica",#9
                   "campione_0",#10
                   "campione",#11
                   "esito",#12
                   "distretto"),#13
      #server = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons',
      filter = c('top'),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),
      options = list(
        order = list(list(3, 'desc'), list(4, 'desc'), list(0, 'desc'), list(10, 'asc')),
        dom = 'Brltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), 
                          c('10', '25', '50', '100', 'Tutti')),
        buttons = list(
          list(extend = "excel", text = "Scarica Tutto",
               filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")), #format(Sys.Date(),"%d-%m-%Y")
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "all"),
                 columns = c(1,2,5,6,7,8,9,11,12,13))),
          list(extend = "excel", text = "Scarica Selezione",
               filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")), #format(Sys.Date(),"%d-%m-%Y")
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "current"),
                 columns = c(1,2,5,6,7,8,9,11,12,13)))
        ),    
        columnDefs = list(
          list(orderData = 3, targets = 5),
          list(orderData = 4, targets = 6),
          list(orderData = 0, targets = 1),
          list(orderData = 10, targets = 11),
          list(visible = FALSE, targets = c(0, 3, 4, 10)),
          list(className = 'dt-body-right', targets = c(5, 6, 11)),
          list(className = 'dt-body-center', targets = c(1)),
          list(className = 'dt-head-center', targets = "_all"),
          list(width = '10px', targets =c(1, 2, 11)),
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
  confSicA %>%
    filter(ASL == select_aslalim, finalita == input$finalita2, anno == input$anno) %>%
    left_join(proveSicA, by = c("nconf")) %>%
    mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
           dtfine_chr = format(dtfine, "%d/%m/%Y"),
           nconf_chr = as.character(nconf),
           numero_del_campione = as.numeric(numero_del_campione),
           numero_del_campione_chr = as.character(numero_del_campione),
           ASL = str_to_title(word(ASL.x, -1)), #gsub(".*- ","", ASL.x)
           #specie = str_to_sentence(specie),
           matrice = str_to_sentence(matrice.x)) %>%  
    dplyr::select(nconf, nconf_chr, verbale, dtinizio, dtfine, dtinizio_chr,
                  dtfine_chr, matrice, prova, tecnica,
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
                   "matrice",#7
                   "prova",#8
                   "tecnica",#9
                   "campione_0",#10
                   "campione",#11
                   "esito"),#12
      #server = FALSE,
      rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons',
      filter = c('top'),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),
      options = list(
        order = list(list(3, 'desc'), list(4, 'desc'), list(0, 'desc'), list(10, 'asc')),
        dom = 'Brltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), 
                          c('10', '25', '50', '100', 'Tutti')),
        buttons = list(
          list(extend = "excel", text = "Scarica Tutto",
               filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")), #format(Sys.Date(),"%d-%m-%Y")
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "all"),
                 columns = c(1,2,5,6,7,8,9,11,12))),
          list(extend = "excel", text = "Scarica Selezione",
               filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),  #format(Sys.Date(),"%d-%m-%Y")
               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "current"),
                 columns = c(1,2,5,6,7,8,9,11,12)))
        ),    
        columnDefs = list(
          list(orderData = 3, targets = 5),
          list(orderData = 4, targets = 6),
          list(orderData = 0, targets = 1),
          list(orderData = 10, targets = 11),
          list(visible = FALSE, targets = c(0, 3, 4, 10)),
          list(className = 'dt-body-right', targets = c(5, 6, 11)),
          list(className = 'dt-body-center', targets = c(1)),
          list(className = 'dt-head-center', targets = "_all"),
          list(width = '10px', targets =c(1, 2, 11)),
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

output$asldrillalim <- DT::renderDataTable(server = FALSE,{
  ASLdrillalim()
})

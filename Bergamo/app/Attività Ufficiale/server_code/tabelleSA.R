campionamento <- reactive({
  if(input$finalita == "Tutte le finalità") {
    proveSA %>% 
      filter(annoiniz == input$selanno)
  } else {
    proveSA %>% 
      filter(annoiniz == input$selanno, finalita == input$finalita)
  }
})


#tabella summary ----

summaryCamp <- reactive({  
  campionamento() %>% 
    distinct(Nconf, .keep_all = TRUE) %>% 
    group_by(ASL) %>% 
    summarise('Controlli effettuati' = n()) %>%
    ungroup() %>% 
    
    bind_cols(
      # campionamento() %>% 
      #   distinct(Nconf, .keep_all = TRUE) %>%  
      #   group_by(ASL) %>% 
      #   summarise('Campioni conferiti' = sum(NrCampioni)) %>%
      #   ungroup() %>% 
      #   dplyr::select('Campioni conferiti')) %>% 
      
      campionamento() %>% 
        # mutate(Nconf_camp = paste(Nconf, numero_del_campione, sep="-")) %>%
        # distinct(Nconf_camp, .keep_all = TRUE) %>%
        distinct(Nconf, numero_del_campione, .keep_all = TRUE) %>%
        group_by(ASL) %>% 
        summarise('Campioni conferiti' = n()) %>%
        ungroup() %>% 
        dplyr::select('Campioni conferiti')) %>% 
    
    bind_cols(
      campionamento() %>% 
        group_by(ASL) %>% 
        summarise('Esami eseguiti' = n()) %>%
        ungroup() %>% 
        dplyr::select('Esami eseguiti')) %>%
    
    adorn_totals(name = "TOTALE ATS", col = 2:4)  
})

# campionamento <- reactive({
#   if(input$finalita == "Tutte le finalità") {
#     confSA %>% 
#       filter(anno == input$selanno)
#   } else {
#     confSA %>% 
#       filter(anno == input$selanno, finalita == input$finalita)
#   }
# })
# 
# 
# #tabella summary
# 
# summaryCamp <- reactive({  
#     campionamento() %>% 
#       group_by(ASL) %>% 
#       # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>% 
#       distinct(Nconf, .keep_all = TRUE) %>% 
#       summarise('Controlli effettuati' = n()) %>% 
#       
#       bind_cols(
#         campionamento() %>% 
#           group_by(ASL) %>% 
#           # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
#           distinct(Nconf, .keep_all = TRUE) %>%  
#           summarise('Campioni conferiti' = sum(NrCampioni)) %>%
#           ungroup() %>% 
#           dplyr::select('Campioni conferiti')) %>% 
#       
#       bind_cols(
#         campionamento() %>% 
#           left_join(nesami, by = "Nconf") %>%
#           group_by(ASL) %>% 
#           # mutate(motivo_prel = ifelse(is.na(motivo_prel), finalita, motivo_prel)) %>%
#           summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>%
#           ungroup() %>% 
#           dplyr::select('Esami eseguiti')) %>%
#     
#     adorn_totals(name = "TOTALE ATS", col = 2:4)
#   
# })



output$t1SA <- DT::renderDataTable({
  
  if (input$finalita == "Tutte le finalità") {
    
  summaryCamp() %>%
    #mutate(ASL = gsub(".*- ","", ASL)) %>%
    rename(Distretto = ASL) %>%
    datatable(rownames = FALSE,
              # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
              #                                   input$finalita),
              style = 'bootstrap',
              class = "row-border",
              selection = 'none',
              options = list(dom = 't'))
    
  } else {  
    
    summaryCamp() %>%
      #mutate(ASL = gsub(".*- ","", ASL)) %>%
      rename(Distretto = ASL) %>%
      datatable(rownames = FALSE,
                # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
                #                                   input$finalita),
                style = 'bootstrap',
                selection = 'single',
                callback = JS("$('tbody').css('cursor', 'pointer')"),
                options = list(dom = 't')
                )
    
  }
  

    
})

outputOptions(output, "t1SA", suspendWhenHidden = FALSE)


# tabella drill down
output$conditionSA <- reactive({
  length(input$t1SA_rows_selected) > 0
})
outputOptions(output, "conditionSA", suspendWhenHidden = FALSE)


ASLdrill <- reactive({
  shiny::validate(
    need(length(input$t1SA_rows_selected) > 0, "")
  )
  
  
  select_asl <- summaryCamp()[as.integer(input$t1SA_rows_selected), ]$ASL
  
  #tabella totale----
  
  if(select_asl == "TOTALE ATS"){  
    proveSA %>% 
      filter(finalita == input$finalita, annoiniz == input$selanno) %>% 
      dplyr::select(Nconf, Nconf2, Nconf3, verbale, codaz, numero_del_campione, numero_del_campione_chr,
                    dtinizio_chr, dtfine_chr, dtinizio, dtfine,
                    specie, materiale, prova, tecnica, "esito" = valore,
                    "distretto" = ASL
                    ) %>% 
      DT::datatable(
        style = 'bootstrap', 
        class = "row-border",
        colnames = c("Conferimento_anno",#0
                     "Conferimento_numeric",#1
                     "Conferimento",#2
                     "Verbale",#3
                     "Codice Azienda",#4
                     "Numero Campione_numeric",#5
                     "Numero Campione",#6
                     "Data inizio",#7
                     "Data fine",#8
                     "data_inizio_numeric",#9
                     "data_fine_numeric",#10
                     "Specie",#11
                     "Materiale",#12
                     "Prova",#13
                     "Tecnica",#14
                     "Esito",#15
                     "Distretto"),#16
        rownames = FALSE,
        selection = 'none',
        # extensions = 'Buttons',
        filter = c('top'),
        callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');",
                      "var a = document.createElement('a');",
                      "$(a).addClass('dt-button fa fa-arrow-circle-down fa-3x');",
                      "a.href = document.getElementById('tsadrill_download').href;",
                      "a.download = '';",
                      "$(a).attr('target', '_blank');",
                      "$(a).text('');",
                      "$('div.dwnld_SA').append(a);",
                      "$('#tsadrill_download').hide();"),   # &#61442 - &#xf002 - \u2315 # 🔍 
        #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
        #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
        options = list(
          order = list(list(9, 'desc'),
                       list(1, 'asc'),
                       list(5, 'asc')),
          dom = '<"dwnld_SA">rltip',
          searching = TRUE,
          autowidth = FALSE,
          pageLength = 5,
          lengthMenu = list(c(5, 25, 50, -1), 
                            c('5', '25', '50', 'Tutti')),
          # buttons = list(
          #   list(extend = "excel", text = "Scarica Tutto",
          #        filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
          #        title = NULL,
          #        titleAttr = "Excel",
          #        exportOptions = list(
          #          modifier = list(page = "all"),
          #          columns = c(2,3,4,6,7,8,11,12,13,14,15,16)))
          #   ),    
          columnDefs = list(
            list(orderData = 1, targets = 2),
            list(orderData = 5, targets = 6),
            list(orderData = 9, targets = 7),
            list(orderData = 10, targets = 8),
            list(visible = FALSE, targets = c(0, 1, 5, 9, 10)),
            list(className = 'dt-body-right', targets = c(2, 6, 7, 8)),
            list(width = '60px', targets =c(7, 8))
            ),
          language = list(decimal = ",",
                          thousands = ".",
                          search = "Cerca: ",
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
    proveSA %>% 
      filter(ASL == select_asl, finalita == input$finalita, annoiniz == input$selanno) %>% 
      dplyr::select(Nconf, Nconf2, Nconf3, verbale, codaz, numero_del_campione, numero_del_campione_chr,
                    dtinizio_chr, dtfine_chr, dtinizio, dtfine,
                    specie, materiale, prova, tecnica, "esito" = valore
      ) %>% 
      DT::datatable(
        style = 'bootstrap', 
        class = "row-border",
        colnames = c("Conferimento_anno",#0
                     "Conferimento_numeric",#1
                     "Conferimento",#2
                     "Verbale",#3
                     "Codice Azienda",#4
                     "Numero Campione_numeric",#5
                     "Numero Campione",#6
                     "Data inizio",#7
                     "Data fine",#8
                     "data_inizio_numeric",#9
                     "data_fine_numeric",#10
                     "Specie",#11
                     "Materiale",#12
                     "Prova",#13
                     "Tecnica",#14
                     "Esito"),#15
        rownames = FALSE,
        selection = 'none',
        # extensions = 'Buttons',
        filter = c('top'),
        callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');",
                      "var a = document.createElement('a');",
                      "$(a).addClass('dt-button fa fa-arrow-circle-down fa-3x');",
                      "a.href = document.getElementById('tsadrill_download').href;",
                      "a.download = '';",
                      "$(a).attr('target', '_blank');",
                      "$(a).text('');",
                      "$('div.dwnld_SA').append(a);",
                      "$('#tsadrill_download').hide();"),   # &#61442 - &#xf002 - \u2315 # 🔍 
        #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
        #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
        options = list(
          order = list(list(9, 'desc'),
                       list(1, 'asc'),
                       list(5, 'asc')),
          dom = '<"dwnld_SA">rltip',
          searching = TRUE,
          autowidth = FALSE,
          pageLength = 5,
          lengthMenu = list(c(5, 25, 50, -1), 
                            c('5', '25', '50', 'Tutti')),
          # buttons = list(
          #   list(extend = "excel", text = "Scarica Tutto",
          #        filename = paste(input$finalita, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
          #        title = NULL,
          #        titleAttr = "Excel",
          #        exportOptions = list(
          #          modifier = list(page = "all"),
          #          columns = c(2,3,4,6,7,8,11,12,13,14,15)))
          # ),    
          columnDefs = list(
            list(orderData = 1, targets = 2),
            list(orderData = 5, targets = 6),
            list(orderData = 9, targets = 7),
            list(orderData = 10, targets = 8),
            list(visible = FALSE, targets = c(0, 1, 5, 9, 10)),
            list(className = 'dt-body-right', targets = c(2, 6, 7, 8))
            # list(width = '60px', targets =c(7, 8))
          ),
          language = list(decimal = ",",
                          thousands = ".",
                          search = "Cerca: ",
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

output$asldrill <- DT::renderDataTable(server = TRUE,{
  ASLdrill()
})

#DOWNLOAD----
ASLdrill_dwnld <- reactive({
  shiny::validate(
    need(length(input$t1SA_rows_selected) > 0, "")
  )
  
  
  select_asl <- summaryCamp()[as.integer(input$t1SA_rows_selected), ]$ASL
  
  if(select_asl == "TOTALE ATS"){  
    proveSA %>%
      mutate(dtinizio = as.Date(dtinizio),
             dtfine = as.Date(dtfine)) %>%  
      filter(finalita == input$finalita, annoiniz == input$selanno) %>% 
      dplyr::select('Distretto' = ASL,
                    'Conferimento' = Nconf2,
                    'Finalità' = finalita,
                    'Verbale' = verbale,
                    'Codice Azienda' = codaz,
                    'Numero campione' = numero_del_campione,
                    'Data inizio' = dtinizio,
                    'Data fine' = dtfine,
                    'Specie' = specie,
                    'Materiale' = materiale,
                    'Prova' = prova,
                    'Tecnica' = tecnica,
                    'Esito' = valore)

    } else {
      proveSA %>%
        mutate(dtinizio = as.Date(dtinizio),
               dtfine = as.Date(dtfine)) %>%  
        filter(ASL == select_asl, finalita == input$finalita, annoiniz == input$selanno) %>% 
        dplyr::select('Distretto' = ASL,
                      'Conferimento' = Nconf2,
                      'Finalità' = finalita,
                      'Verbale' = verbale,
                      'Codice Azienda' = codaz,
                      'Numero campione' = numero_del_campione,
                      'Data inizio' = dtinizio,
                      'Data fine' = dtfine,
                      'Specie' = specie,
                      'Materiale' = materiale,
                      'Prova' = prova,
                      'Tecnica' = tecnica,
                      'Esito' = valore)
  }
  
})


output$tsadrill_download <- downloadHandler(
  #https://stackoverflow.com/questions/50948024/shiny-downloadhandler-openxlsx-does-not-generate-a-xlsx-file
  filename = function() {
    paste('Sanità Animale - ',input$finalita, ' - attività ufficiale al ',
          format(as.Date(substr(max(proveSA$dtreg[proveSA$annoiniz == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(file) {
    
    wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb, "Sheet1")
    
    x <- ASLdrill_dwnld()
    options(openxlsx.dateFormat = "dd/mm/yyyy")
    openxlsx::writeData(wb, "Sheet1", x, rowNames = FALSE)
    
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)


#RICERCA CODICE ALLEVAMENTO ----
## tabella principale di sintesi----

summarycodaz <- reactive({
  proveSA %>% 
    distinct(Nconf, .keep_all = TRUE) %>% 
    filter(codaz == input$codiceall, annoiniz == input$selanno) %>%
    dplyr::select(Nconf, Nconf2, Nconf3, verbale, finalita,
                  dtprel, dtprel_chr, dtconf, dtconf_chr, dtreg, dtreg_chr,
                  specie, materiale, conferente, proprietario, veterinario, comune,
                  NrCampioni, NrCampioni_chr)
})




output$t3SA <- DT::renderDataTable(server = FALSE,{
  summarycodaz() %>% 
    datatable(style = 'bootstrap',
              colnames = c("Conferimento_anno",#0
                           "Conferimento_numeric",#1
                           "Conferimento",#2
                           "Verbale",#3
                           "Finalità", #4
                           "data prelievo_numeric",#5
                           "Data Prelievo",#6
                           "data conferimento_numeric",#7
                           "Data Conferimento",#8
                           "data registrazione_numeric",#9
                           "Data Registrazione",#10
                           "Specie",#11
                           "Materiale",#12
                           "Conferente",#13
                           "Proprietario",#14
                           "Veterinario",#15
                           "Comune",#16
                           "campioni_accettati_numeric",#17
                           "Campioni accettati"),#18
              rownames = FALSE,
              selection = 'single',
              extensions = 'Buttons',
              filter = c('top'),
              callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');",
                            "$('tbody').css('cursor', 'pointer')"),   # &#61442 - &#xf002 - \u2315 # 🔍 
              options = list(
                order = list(list(1, 'desc')),
                dom = 'Brltip',
                scrollX = TRUE,
                searching = TRUE,
                autoWidth = TRUE,
                pageLength = 5,
                lengthMenu = list(c(5, 25, 50, -1), 
                                  c('5', '25', '50', 'Tutti')),
                buttons = list(
                  list(extend = "excel", text = "Scarica Tutto",
                       filename = paste("Codice azienda", input$codiceall, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
                       title = NULL,
                       titleAttr = "Excel",
                       exportOptions = list(
                         modifier = list(page = "all"),
                         columns = c(2,4,6,8,10,11,12,13,14,15,16,18)))
                  ),    
                columnDefs = list(
                  list(orderData = 1, targets = 2),
                  list(orderData = 5, targets = 6),
                  list(orderData = 7, targets = 8),
                  list(orderData = 9, targets = 10),
                  list(orderData = 17, targets = 18),
                  
                  list(visible = FALSE, targets = c(0, 1, 3, 5, 7, 9, 17)),
                  list(className = 'dt-body-right', targets = c(2, 6, 8, 10, 18))
                 # list(className = 'dt-body-center', targets = c(1)),
                 # list(className = 'dt-head-center', targets = "_all"),
                #  list(width = '10px', targets =c(1, 3, 6, 8)),
                #  list(width = '40px', targets =c(2, 4)),
                #  list(width = '60px', targets =c(9))
                  # list(width = '60px', targets =c(6, 7))
                ),
                language = list(decimal = ",",
                                thousands = ".",
                                search = "Cerca: ",
                                lengthMenu = "Mostra _MENU_ conferimenti",
                                paginate = list(previous = "Precedente", `next` = "Successiva"),
                                info = "_START_ - _END_ di _TOTAL_ conferimenti",
                                infoFiltered = "(su un totale di _MAX_ conferimenti)",
                                infoEmpty = "Nessun conferimento disponibile",
                                zeroRecords = "---")        
              )
    )
})


## tabella di drilldown----
output$conditionRicSA <- reactive({
  length(input$t3SA_rows_selected) > 0
})
outputOptions(output, "conditionRicSA", suspendWhenHidden = FALSE)

azcodedrill <- reactive({
  shiny::validate(
    need(length(input$t3SA_rows_selected) > 0, "")
  )
  
  select_conf <- summarycodaz()[as.integer(input$t3SA_rows_selected), ]$Nconf
  
  proveSA %>% 
    filter(Nconf == select_conf) %>% 
    dplyr::select(Nconf, Nconf2, Nconf3, verbale, codaz, numero_del_campione, numero_del_campione_chr,
                  dtinizio_chr, dtfine_chr, dtinizio, dtfine,
                  specie, materiale, prova, tecnica, "esito" = valore
    ) %>% 
    DT::datatable(
      style = 'bootstrap', 
      class = "row-border",
      colnames = c("Conferimento_anno",#0
                   "Conferimento_numeric",#1
                   "Conferimento",#2
                   "Verbale",#3
                   "Codice Azienda",#4
                   "Numero Campione_numeric",#5
                   "Numero Campione",#6
                   "Data inizio",#7
                   "Data fine",#8
                   "data_inizio_numeric",#9
                   "data_fine_numeric",#10
                   "Specie",#11
                   "Materiale",#12
                   "Prova",#13
                   "Tecnica",#14
                   "Esito"),#15
      rownames = FALSE,
      selection = 'none',
      extensions = 'Buttons',
      filter = c('top'),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
      #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
      #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
      options = list(
        order = list(list(1, 'desc'), list(5, "asc")),
        dom = 'Brltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 5,
        lengthMenu = list(c(5, 25, 50, -1), 
                          c('5', '25', '50', 'Tutti')),
        buttons = list(
          list(extend = "excel", text = "Scarica Tutto",
               filename = paste("conferimento", stringi::stri_sub(select_conf, from = 5), "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")), #format(Sys.Date(),"%d-%m-%Y")               title = NULL,
               titleAttr = "Excel",
               exportOptions = list(
                 modifier = list(page = "all"),
                 columns = c(2,3,4,6,7,8,11,12,13,14,15)))
          ),    
        columnDefs = list(
          list(orderData = 1, targets = 2),
          list(orderData = 5, targets = 6),
          list(orderData = 9, targets = 7),
          list(orderData = 10, targets = 8),
          list(visible = FALSE, targets = c(0, 1, 5, 9, 10)),
          list(className = 'dt-body-right', targets = c(2, 6, 7, 8))
          ),
        language = list(decimal = ",",
                        thousands = ".",
                        search = "Cerca: ",
                        lengthMenu = "Mostra _MENU_ esami",
                        paginate = list(previous = "Precedente", `next` = "Successiva"),
                        info = "_START_ - _END_ di _TOTAL_ esami",
                        infoFiltered = "(su un totale di _MAX_ esami)",
                        infoEmpty = "Nessun esame disponibile",
                        zeroRecords = "---")        
      )
    )
  # confSA %>%
  #   filter(Nconf == select_conf) %>%
  #   left_join(proveSA, by = c("Nconf")) %>%
  #   mutate(dtinizio_chr = format(dtinizio, "%d/%m/%Y"),
  #          dtfine_chr = format(dtfine, "%d/%m/%Y"),
  #          Nconf_chr = as.character(Nconf),
  #          numero_del_campione_chr = as.character(numero_del_campione),
  #          specie = str_to_sentence(specie.x),
  #          materiale = str_to_sentence(materiale.x)) %>%
  #   dplyr::select(Nconf, Nconf_chr, verbale, codaz, dtinizio, dtfine, dtinizio_chr, dtfine_chr, specie,
  #                 materiale, prova, tecnica, 
  #                 numero_del_campione, numero_del_campione_chr, "esito" = valore) %>% 
  #   DT::datatable(
  #     style = 'bootstrap', 
  #     class = "row-border",
  #     colnames = c("conferimento_0",#0
  #                  "conferimento",#1
  #                  "verbale",#2
  #                  "codice azienda",#3
  #                  "inizio_0",#4
  #                  "fine_0",#5
  #                  "data inizio",#6
  #                  "data fine",#7
  #                  "specie",#8
  #                  "materiale",#9
  #                  "prova",#10
  #                  "tecnica",#11
  #                  "campione_0",#12
  #                  "campione",#13
  #                  "esito"),#14
  #     rownames = FALSE,
  #     selection = 'none',
  #     extensions = 'Buttons',
  #     filter = c('top'),
  #     callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),
  #     options = list(
  #       order = list(list(4, 'desc'), list(5, 'desc'), list(12, 'asc')),
  #       dom = 'Brltip',
  #       searching = TRUE,
  #       autowidth = FALSE,
  #       pageLength = 5,
  #       lengthMenu = list(c(5, 25, 50, 100, -1), 
  #                         c('5', '25', '50', '100', 'Tutti')),
  #       buttons = list(
  #         list(extend = "excel", text = "Scarica Tutto",
  #              filename = paste("conferimento", select_conf, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")), #format(Sys.Date(),"%d-%m-%Y")
  #              title = NULL,
  #              titleAttr = "Excel",
  #              exportOptions = list(
  #                modifier = list(page = "all"),
  #                columns = c(1,2,3,6,7,8,9,10,11,13,14)))
  #         # ,
  #         # list(extend = "excel", text = "Scarica Selezione",
  #         #      filename = paste("conferimento", select_conf, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),  #format(Sys.Date(),"%d-%m-%Y")
  #         #      title = NULL,
  #         #      titleAttr = "Excel",
  #         #      exportOptions = list(
  #         #        modifier = list(page = "current"),
  #         #        columns = c(1,2,3,6,7,8,9,10,11,13,14)))
  #       ),    
  #       columnDefs = list(
  #         list(orderData = 4, targets = 6),
  #         list(orderData = 5, targets = 7),
  #         list(orderData = 0, targets = 1),
  #         list(orderData = 12, targets = 13),
  #         list(visible = FALSE, targets = c(0, 4, 5, 12)),
  #         list(className = 'dt-body-right', targets = c(6, 7, 13)),
  #         list(className = 'dt-body-center', targets = c(1)),
  #         list(className = 'dt-head-center', targets = "_all"),
  #         list(width = '10px', targets =c(1, 13)),
  #         list(width = '30px', targets =c(2)),
  #         list(width = '60px', targets =c(6, 7))
  #         #   list(width = '80px', targets =c(5)),
  #         #   list(width = '80px', targets =c(6)),      
  #         #   list(width = '100px', targets =c(7)),
  #         #   list(className = 'dt-body-right', targets = c(5,6)),
  #         #   list(targets = c(1,2,3,4), className = 'dt-middle')
  #       ),
  #       language = list(search = "Cerca: ",
  #                       lengthMenu = "Mostra _MENU_ esami",
  #                       paginate = list(previous = "Precedente", `next` = "Successiva"),
  #                       info = "_START_ - _END_ di _TOTAL_ esami",
  #                       infoFiltered = "(su un totale di _MAX_ esami)",
  #                       infoEmpty = "Nessun esame disponibile",
  #                       zeroRecords = "---")        
  #     )
  #   )
})

output$cadrill <- DT::renderDataTable(server = FALSE,{
  azcodedrill()
})



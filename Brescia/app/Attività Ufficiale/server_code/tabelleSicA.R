alimenti <- reactive({
  if(input$finalita2 == "Tutte le finalità") {
    proveSicA %>% 
      filter(annoiniz == input$selanno)
  } else {
    proveSicA %>% 
      filter(annoiniz == input$selanno, finalita == input$finalita2)
  }
})

#tabella summary----

summaryalim <- reactive({  
  alimenti() %>% 
    distinct(Nconf, .keep_all = TRUE) %>% 
    group_by(ASL) %>% 
    summarise('Controlli effettuati' = n()) %>%
    ungroup() %>% 

    bind_cols(
      alimenti() %>% 
        # mutate(Nconf_camp = paste(Nconf, numero_del_campione, sep="-")) %>%
        # distinct(Nconf_camp, .keep_all = TRUE) %>%
        distinct(Nconf, numero_del_campione, .keep_all = TRUE) %>%
        group_by(ASL) %>% 
        summarise('Campioni conferiti' = n()) %>%
        ungroup() %>% 
        dplyr::select('Campioni conferiti')) %>% 
    
    bind_cols(
      alimenti() %>% 
        group_by(ASL) %>% 
        summarise('Esami eseguiti' = n()) %>%
        ungroup() %>% 
        dplyr::select('Esami eseguiti')) %>%
    
    adorn_totals(name = "TOTALE AUSL", col = 2:4)
  
  })



output$t1SicA <- DT::renderDataTable({
  
  if (input$finalita2 == "Tutte le finalità") {
    
    summaryalim() %>%
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
    
    summaryalim() %>%
      #mutate(ASL = gsub(".*- ","", ASL)) %>%
      rename(Distretto = ASL) %>%
      datatable(rownames = FALSE,
                # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:black; font-size:100% ;',
                #                                   input$finalita),
                style = 'bootstrap',
                selection = 'single',
                callback = JS("$('tbody').css('cursor', 'pointer')"),
                options = list(dom = 't'))
    
  }
  
})

outputOptions(output, "t1SicA", suspendWhenHidden = FALSE)


# tabella drill down
output$conditionSicA <- reactive({
  length(input$t1SicA_rows_selected) > 0
})
outputOptions(output, "conditionSicA", suspendWhenHidden = FALSE)


ASLdrillalim <- reactive({
  shiny::validate(
    need(length(input$t1SicA_rows_selected) > 0, "") #, "Seleziona la FINALITÀ e il DISTRETTO per vedere il dettaglio delle attività"
  )
  
  
select_aslalim <- summaryalim()[as.integer(input$t1SicA_rows_selected), ]$ASL
  
  
#tabella totale----

if(select_aslalim == "TOTALE AUSL"){  
  proveSicA %>% 
    filter(finalita == input$finalita2, annoiniz == input$selanno) %>% 
    dplyr::select(Nconf, Nconf2, Nconf3, verbale, codaz, numero_del_campione, numero_del_campione_chr,
                  dtinizio_chr, dtfine_chr, dtinizio, dtfine,
                  matrice, prova, tecnica, "esito" = valore,
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
                   "Matrice",#11
                   "Prova",#12
                   "Tecnica",#13
                   "Esito",#14
                   "Distretto"),#15
      rownames = FALSE,
      selection = 'none',
      # extensions = 'Buttons',
      filter = c('top'),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');",
                    "var a = document.createElement('a');",
                    "$(a).addClass('dt-button fa fa-arrow-circle-down fa-3x');",
                    "a.href = document.getElementById('tsalimdrill_download').href;",
                    "a.download = '';",
                    "$(a).attr('target', '_blank');",
                    "$(a).text('');",
                    "$('div.dwnld_SicA').append(a);",
                    "$('#tsalimdrill_download').hide();"),   # &#61442 - &#xf002 - \u2315 # 🔍 
      #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
      #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
      options = list(
        order = list(list(1, 'desc'), list(5, "asc")),
        dom = '<"dwnld_SicA">rltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 5,
        lengthMenu = list(c(5, 25, 50, -1), 
                          c('5', '25', '50', 'Tutti')),
        # buttons = list(
        # #   list(extend = "excel", text = "Scarica Tutto",
        # #        filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
        # #        title = NULL,
        # #        titleAttr = "Excel",
        # #        exportOptions = list(
        # #          modifier = list(page = "all"),
        # #          columns = c(2,3,4,6,7,8,11,12,13,14,15)))
        # ),    
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
  proveSicA %>% 
    filter(ASL == select_aslalim, finalita == input$finalita2, annoiniz == input$selanno) %>% 
    dplyr::select(Nconf, Nconf2, Nconf3, verbale, codaz, numero_del_campione, numero_del_campione_chr,
                  dtinizio_chr, dtfine_chr, dtinizio, dtfine,
                  matrice, prova, tecnica, "esito" = valore
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
                   "Matrice",#11
                   "Prova",#12
                   "Tecnica",#13
                   "Esito"),#14
      rownames = FALSE,
      selection = 'none',
      # extensions = 'Buttons',
      filter = c('top'),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');",
                    "var a = document.createElement('a');",
                    "$(a).addClass('dt-button fa fa-arrow-circle-down fa-3x');",
                    "a.href = document.getElementById('tsalimdrill_download').href;",
                    "a.download = '';",
                    "$(a).attr('target', '_blank');",
                    "$(a).text('');",
                    "$('div.dwnld_SicA').append(a);",
                    "$('#tsalimdrill_download').hide();"),   # &#61442 - &#xf002 - \u2315 # 🔍 
      #cerca --> character --> copy to clipboard --> da https://graphemica.com/%F0%9F%94%8D#character%20left-pointing%20magnifying%20glass
      #https://stackoverflow.com/questions/32915485/how-to-prevent-unicode-characters-from-rendering-as-emoji-in-html-from-javascript
      options = list(
        order = list(list(1, 'desc'), list(5, "asc")),
        dom = '<"dwnld_SicA">rltip',
        searching = TRUE,
        autowidth = FALSE,
        pageLength = 5,
        lengthMenu = list(c(5, 25, 50, -1), 
                          c('5', '25', '50', 'Tutti')),
        # buttons = list(
        # #   list(extend = "excel", text = "Scarica Tutto",
        # #        filename = paste(input$finalita2, "- attività ufficiale al", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
        # #        title = NULL,
        # #        titleAttr = "Excel",
        # #        exportOptions = list(
        # #          modifier = list(page = "all"),
        # #          columns = c(2,3,4,6,7,8,11,12,13,14)))
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

output$asldrillalim <- DT::renderDataTable(server = TRUE,{
  ASLdrillalim()
})

#DOWNLOAD----
ASLdrillalim_dwnld <- reactive({
  shiny::validate(
    need(length(input$t1SicA_rows_selected) > 0, "") #, "Seleziona la FINALITÀ e il DISTRETTO per vedere il dettaglio delle attività"
  )
  
  select_aslalim <- summaryalim()[as.integer(input$t1SicA_rows_selected), ]$ASL

  if(select_aslalim == "TOTALE AUSL"){  
    proveSicA %>% 
      mutate(dtinizio = as.Date(dtinizio),
             dtfine = as.Date(dtfine)) %>%  
      filter(finalita == input$finalita2, annoiniz == input$selanno) %>% 
      dplyr::select('Distretto' = ASL,
                    'Conferimento' = Nconf2,
                    'Finalità' = finalita,
                    'Verbale' = verbale,
                    'Codice Azienda' = codaz,
                    'Numero campione' = numero_del_campione,
                    'Data inizio' = dtinizio,
                    'Data fine' = dtfine,
                    'Matrice' = matrice,
                    'Prova' = prova,
                    'Tecnica' = tecnica,
                    'Esito' = valore)
    
  } else {
    proveSicA %>%
      mutate(dtinizio = as.Date(dtinizio),
             dtfine = as.Date(dtfine)) %>%  
      filter(ASL == select_aslalim, finalita == input$finalita2, annoiniz == input$selanno) %>% 
      dplyr::select('Distretto' = ASL,
                    'Conferimento' = Nconf2,
                    'Finalità' = finalita,
                    'Verbale' = verbale,
                    'Codice Azienda' = codaz,
                    'Numero campione' = numero_del_campione,
                    'Data inizio' = dtinizio,
                    'Data fine' = dtfine,
                    'Matrice' = matrice,
                    'Prova' = prova,
                    'Tecnica' = tecnica,
                    'Esito' = valore)
  }
  
})


output$tsalimdrill_download <- downloadHandler(
  #https://stackoverflow.com/questions/50948024/shiny-downloadhandler-openxlsx-does-not-generate-a-xlsx-file
  filename = function() {
    paste('Sicurezza Alimentare - ',input$finalita2, ' - attività ufficiale al ',
          format(as.Date(substr(max(proveSicA$dtreg[proveSicA$annoiniz == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(file) {
    
    wb <- openxlsx::createWorkbook()
    
    openxlsx::addWorksheet(wb, "Sheet1")
    
    x <- ASLdrillalim_dwnld()
    options(openxlsx.dateFormat = "dd/mm/yyyy")
    openxlsx::writeData(wb, "Sheet1", x, rowNames = FALSE)
    
    openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  }
)

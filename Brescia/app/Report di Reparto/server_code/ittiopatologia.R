#ITTIOPATOLOGIA----

observeEvent(input$ittio_bttn1, {
  shinyjs::show("ittio_cp1")
  shinyjs::hide("ittio_cp2")
  runjs('document.getElementById("table_ittio").scrollIntoView();')
  })

observeEvent(input$ittio_bttn2, {
  shinyjs::show("ittio_cp2")
  shinyjs::hide("ittio_cp1")
  runjs('document.getElementById("ittio_esami").scrollIntoView();')
})


observe({
  mindate <- min(ittiopatologia$dtinizio[ittiopatologia$annoiniz == input$selanno], na.rm = TRUE)
  maxdate <- max(ittiopatologia$dtinizio[ittiopatologia$annoiniz == input$selanno], na.rm = TRUE)
  
  updateDateRangeInput(session,
                       "dateRangeEsam_ittio",
                       start = mindate,
                       end = maxdate
                       )
  })

#GRAFICI----

##NUMERO DI ESAMI ESEGUITI----

output$ittioP1 <- renderPlotly({
  #req(input$prove)
  ittiopatologia %>%
    # mutate(weekiniz = strftime(dtinizio, format = "%U-%Y")) %>% 
    filter(annoiniz == input$selanno) %>% 
    #mutate(sett = strftime(dtinizio, format = "%U-%Y")) %>% 
    group_by(weekiniz) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(
      annoiniz = sub(".*-","", weekiniz),
      settiniz = as.numeric(sub("-.*","", weekiniz)),
      sett_inizio = as.Date(paste(annoiniz, settiniz, 1, sep = "-"), "%Y-%U-%u"),
      intervallo = case_when(
        settiniz == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
        settiniz %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
        settiniz == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
    plot_ly(x = ~settiniz, y = ~n, type = "bar", #source = "D",
            hoverinfo = 'text',
            hovertext = ~paste('</br>', n , 'esami eseguiti',
                               '</br>', intervallo)) %>%
    #event_register('plotly_click') %>% 
    layout(#title = input$settore,
      hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                        font = list(family = "Montserrat")),
      font = list(family = 'Montserrat'),
      xaxis = list(title = 'Settimana',
                   tickformat = ',d'), 
      yaxis = list(title = 'Esami eseguiti')) %>%
    config(displayModeBar = FALSE)
  })

# outputOptions(output, "ittioP1", suspendWhenHidden = FALSE)

##TEMPI MEDI PER PROVA----

output$ittioP2 <- renderPlotly({
  
  req(input$ittio_prova)
  
  ittiopatologia %>%
    filter(annoiniz == input$selanno) %>% 
    mutate(tempo_esecuzione = as.numeric(difftime(dtfine, dtconf, units = c("days")))) %>%
    group_by(prova, weekiniz) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
    ungroup() %>% 
    mutate(
      annoiniz = sub(".*-","", weekiniz),
      settiniz = as.numeric(sub("-.*","", weekiniz)),
      sett_inizio = as.Date(paste(annoiniz, settiniz, 1, sep = "-"), "%Y-%U-%u"),
      intervallo = case_when(
        settiniz == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
        settiniz %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
        settiniz == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
    filter(prova == input$ittio_prova) %>% 
    plot_ly(x = ~settiniz,
            hoverinfo = 'text',
            hovertext = ~paste('</br>', 'Tempo medio:', tmesec, 'giorni',
                               '</br>', "Settimana:", settiniz,
                               '</br>', intervallo)) %>%
    add_trace(y = ~tmesec, type = 'scatter', mode = 'lines+markers') %>% 
    layout(#title = input$settore,
      #autosize = F, height = 450,
      hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                        font = list(family = "Montserrat")),
      font = list(family = 'Montserrat'),
      xaxis = list(title = 'Settimana',
                   #rangemode = "tozero",
                   tickformat = ',d'
                   ), 
      yaxis = list(title = 'Tempo medio di esecuzione (giorni)')
    ) %>%
    config(displayModeBar = FALSE)
  
})






#TABELLE----

##TEMPO MEDIO----
output$head_ittioT1 <- renderUI({
  n <- ittiopatologia %>%
    filter(annoiniz == input$selanno) %>% 
    count()
  
  tags$div(
    tags$h4("Numero di esami e tempo medio di esecuzione", style = "font-weight: 600;"), 
    tags$h5(paste("(", n, " esami totali"," - dati aggiornati al ",
                  format(as.Date(substr(
                    max(ittiopatologia$dtreg[ittiopatologia$annoiniz == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
                  ")", sep=""))
  )
})


dt_ittio <- reactive({
  ittiopatologia %>%
    filter(annoiniz == input$selanno) %>%
    group_by(prova) %>%
    count() %>%
    bind_cols(
      ittiopatologia %>%
        filter(annoiniz == input$selanno) %>% 
        mutate(tempo_esecuzione = as.numeric(difftime(dtfine, dtconf, units = c("days")))) %>%
        group_by(prova) %>%
        summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
        ungroup() %>%
        dplyr::select("tempo medio di esecuzione (gg)" = tmesec)) %>%
    arrange(desc(n))
  })

output$ittioT1 <- renderDataTable({
  dt_ittio() %>% 
    datatable(
      style = 'bootstrap',
      class = "row-border",
      rownames = FALSE,
      selection = 'none',
      colnames = c("Tipo di prova",#0
                   "Esami eseguiti",#1
                   "Tempo medio (gg)"),#2
      options = list(
        class = 'compact row-border',
        dom = 'pt',
        pageLength = 10,
        pagingType = "numbers",
        #autoWidth = TRUE,
        columnDefs = list(
          list(width = '70px', targets =c(1)),
          list(width = '90px', targets =c(2))),
        language = list(paginate = list(previous = "Precedente", `next` = "Successiva"))
        )
      )
  })

# outputOptions(output, "ittioT1", suspendWhenHidden = FALSE)

##VEDI CONFERIMENTI----
# output$table_ittio <- renderUI({
#   dataTableOutput("table_diagn", width = "100%")
#   })

conf_ittio <- reactive({
  
  ittiopatologia  %>% 
    filter(annoiniz == input$selanno) %>%
    distinct(Nconf, .keep_all = TRUE) %>%
    select(Nconf, Nconf3, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
           conferente, dest_rdp, NrCampioni, Nconf2)
  })

output$table_ittio <- renderDataTable(server = TRUE, {
  
  conf_ittio() %>% 
    DT::datatable(
      style = 'bootstrap',
      rownames = F,
      selection = 'single',
      #extensions = 'Buttons',
      colnames = c("Conferimento_anno",#0
                   "Conferimento",#1
                   "Settore",#2
                   "Tipo prelievo",#3
                   "Finalità",#4
                   "Data prelievo",#5
                   "Data conferimento",#6
                   "Data registrazione",#7
                   "Conferente",#8
                   "Destinatario RdP",#9
                   "Campioni conferiti",#10
                   "Conferimento_numeric"),#11
      filter = list(position = "top"),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');
                     $('tbody').css('cursor', 'pointer');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
      options = list(
        class = 'compact row-border',
        dom = 'tip',
        pageLength = 5,
        order = list(list(7, 'desc')),
        # buttons = list(
        #   list(extend = "excel", text = "Scarica Tutto",
        #        filename = paste("Laboratorio di ittiopatologia", "- dati al", format(as.Date(substr(max(ittiopatologia$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
        #        title = NULL,
        #        titleAttr = "Excel",
        #        exportOptions = list(
        #          modifier = list(page = "all"),
        #          columns = c(1,2,3,4,5,6,7,8,9,10)))),
        columnDefs = list(
          list(className = 'dt-body-right', targets = c(1)),
          list(orderData = 11, targets = 1),
          list(visible = FALSE, targets = c(0, 11))),
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
      ) %>%
    formatDate(
      columns = c(6 ,7 , 8), 
      method =  "toLocaleDateString", 
      params = list(
        'it-IT', 
        list(
          year = 'numeric', 
          month = 'numeric',
          day = 'numeric'))
      )
  })

###esami drilldown----
output$selconf_ittio <- renderUI({
  
  sel_conf <- conf_ittio()[as.integer(input$table_ittio_rows_selected), ]$Nconf
  selconf <- as.numeric(gsub("^.{0,4}", "", sel_conf))
  
  paste0("CONFERIMENTO ", selconf," - DETTAGLIO PROVE ESEGUITE")
  
})

output$drillui_ittio <- renderUI({
  
  if (length(input$table_ittio_rows_selected) > 0){
    
    div(style = "margin-bottom: 100px;",
        uiOutput("selconf_ittio", style = "font-weight: 600;font-size: 18px;margin-top: 10px;margin-bottom: 10px;"),
        br(),
        dataTableOutput("drill_ittio")
        )
    }
  })


prove_ittio <- reactive({
  shiny::validate(
    need(length(input$table_ittio_rows_selected) > 0, "")
  )
  
  select_conf <- conf_ittio()[as.integer(input$table_ittio_rows_selected), ]$Nconf
  
  ittiopatologia %>%
    filter(annoiniz == input$selanno,
           Nconf == select_conf) %>%
    dplyr::select(Nconf3, verbale, codaz, numero_del_campione, dtinizio, dtfine, specie,
                  materiale, prova, tecnica, "esito" = valore, Nconf, Nconf2) %>% 
    DT::datatable(
      style = 'bootstrap',
      class = "row-border",
      rownames = FALSE,
      selection = 'none',
      #extensions = 'Buttons',
      colnames = c("Conferimento",#0
                   "Verbale",#1
                   "Codice Azienda",#2
                   "Numero campione",#3
                   "Data inizio",#4
                   "Data fine",#5
                   "Specie",#6
                   "Materiale",#7
                   "Prova",#8
                   "Tecnica",#9
                   "Esito",#10
                   "Conferimento_anno",#11
                   "Conferimento_numeric"),#12
      filter = list(position = "top"),
      callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
      options = list(
        class = 'compact row-border',
        dom = 'tip',
        pageLength = 5,
        order = list(list(3, 'asc')),
        # buttons = list(
        #   list(extend = "excel", text = "Scarica Tutto",
        #        filename = paste("Laboratorio di ittiopatologia", "- dati al", format(as.Date(substr(max(ittiopatologia$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y")),
        #        title = NULL,
        #        titleAttr = "Excel",
        #        exportOptions = list(
        #          modifier = list(page = "all"),
        #          columns = c(0,1,2,3,4,5,6,7,8,9,10)))),
        columnDefs = list(
          list(className = 'dt-body-right', targets = c(0)),
          list(orderData = 12, targets = 0),
          list(visible = FALSE, targets = c(11, 12))),
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

output$drill_ittio <- DT::renderDataTable(server = TRUE,{
  prove_ittio()
})



##VEDI ESAMI----
ittio_esami <- reactive({
  
  if(input$ittio_prove == "Tutte le prove"){
    
    ittiopatologia %>%
      filter(annoiniz == input$selanno,
             # prova == input$esam,
             dtinizio >= input$dateRangeEsam_ittio[1] & dtinizio <= input$dateRangeEsam_ittio[2]) %>%
      dplyr::select(Nconf2, verbale, codaz, numero_del_campione, dtinizio, dtfine, specie,
                    materiale, prova, tecnica, "esito" = valore, Nconf)
    
    } else {
      
      ittiopatologia %>%
        filter(annoiniz == input$selanno, 
               prova == input$ittio_prove,
               dtinizio >= input$dateRangeEsam_ittio[1] & dtinizio <= input$dateRangeEsam_ittio[2]) %>%
        dplyr::select(Nconf2, verbale, codaz, numero_del_campione, dtinizio, dtfine, specie,
                      materiale, prova, tecnica, "esito" = valore, Nconf)
      
    }
  })



# observeEvent(input$doesami, {
#   shiny::req(input$esam)
#   shiny::req(input$dateRangeEsam)
output$ittio_esami <- DT::renderDataTable(server = TRUE,{
  
  if (input$doesami_ittio == 0)
    return()
  
  isolate({ittio_esami() %>% 
      DT::datatable(
        style = 'bootstrap',
        class = "row-border",
        rownames = FALSE,
        selection = 'none',
        colnames = c("Conferimento",#0
                     "Verbale",#1
                     "Codice Azienda",#2
                     "Numero campione",#3
                     "Data inizio",#4
                     "Data fine",#5
                     "Specie",#6
                     "Materiale",#7
                     "Prova",#8
                     "Tecnica",#9
                     "Esito",#10
                     "Conferimento_0"),#11
        filter = list(position = "top"),
        callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
        options = list(
          class = 'compact row-border',
          dom = 'tip',
          pageLength = 5,
          order = list(list(4, 'desc'),
                       list(0, 'asc'),
                       list(3, 'asc')),
          columnDefs = list(
            #   list(orderData = 4, targets = 6),
            #   list(orderData = 5, targets = 7),
            #   list(orderData = 0, targets = 1),
            #   list(orderData = 12, targets = 13),
            list(visible = FALSE, targets = c(11))),
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
})
# })



#DOWNLOAD----
output$ittio_downloadConf <- downloadHandler(
  filename = function() {
    paste('Laboratorio di Ittiopatologia - conferimenti al ',
          format(as.Date(substr(max(ittiopatologia$dtreg[ittiopatologia$annoiniz == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(con) {
    writexl::write_xlsx(
      format_headers = FALSE,
      ittiopatologia %>%
        filter(annoiniz == input$selanno) %>% 
        mutate(dtprel = as.Date(dtprel),
               dtconf = as.Date(dtconf),
               dtreg = as.Date(dtreg)) %>%
        distinct(Nconf, .keep_all = TRUE) %>%
        arrange(desc(dtreg), Nconf2) %>% 
        select('Conferimento' = Nconf2,
               'Settore' = settore,
               'Tipo prelievo' = tipo_prelievo,
               "Finalità" = finalita,
               "Data prelievo" = dtprel,
               "Data conferimento" = dtconf,
               "Data registrazione" = dtreg,
               "Conferente" = conferente,
               "Destinatario RdP" = dest_rdp,
               "Campioni conferiti" = NrCampioni),
      con)
  }
)

output$ittio_downloadEsam <- downloadHandler(
  filename = function() {
    paste('Laboratorio di Ittiopatologia - prove al ',
          format(as.Date(substr(max(ittiopatologia$dtinizio[ittiopatologia$annoiniz == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(con) {
    writexl::write_xlsx(
      format_headers = FALSE,
      ittiopatologia %>%
        filter(annoiniz == input$selanno) %>% 
        mutate(dtinizio = as.Date(dtinizio),
               dtfine = as.Date(dtfine)) %>% 
        arrange(desc(dtinizio), Nconf2, numero_del_campione) %>% 
        dplyr::select("Conferimento" = Nconf2,
                      "Verbale" = verbale,
                      "Codice Azienda" = codaz,
                      "Numero campione" = numero_del_campione,
                      "Data inizio" = dtinizio,
                      "Data fine" = dtfine,
                      "Specie" = specie,
                      "Materiale" = materiale,
                      "Prova" = prova,
                      "Tecnica" = tecnica,
                      "Esito" = valore),
      con)
  }
)

#https://glin.github.io/reactable/articles/examples.html
#csv button download 
#filter_slider

#ACCETTAZIONE----

#Using conditionalPanels in Shiny based on plotly clicks
#https://stackoverflow.com/questions/51935175/using-conditionalpanels-in-shiny-based-on-plotly-clicks
output$condition <- reactive({
  length(selected_bar()) > 0
})
outputOptions(output, "condition", suspendWhenHidden = FALSE)

# # show the back button to hide drill down table
# output$back <- renderUI({
#   if (length(selected_bar()))
#     actionButton("clear", "Nascondi Tabella", icon("chevron-left"))
# })
# 
# # show the back button to hide drill down table
# output$back2 <- renderUI({
#   if (length(selected_bar2()))
#     actionButton("clear2", "Nascondi Tabella", icon("chevron-left"))
# })
# 
# # clear the selection
# observeEvent(input$clear,{
#   selected_bar(NULL)
# })
#
# # clear the selection
# observeEvent(input$clear2,{
#   selected_bar2(NULL)
# })

observeEvent(input$settore,{
  selected_bar(NULL)
  selected_bar2(NULL)
})

observeEvent(input$visual,{
  selected_bar(NULL)
  selected_bar2(NULL)
})



#GRAFICI----
#ACC-settimanale----
fgacc <- reactive({
  
  if(input$settore == "Tutti"){
    conf %>% 
      filter(annoreg == input$selanno) %>% 
      distinct(Nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>%
      mutate(
        anno_reg = sub(".*-","", weekreg),
        sett_reg = as.numeric(sub("-.*","", weekreg)),
        sett_inizio = as.Date(paste(anno_reg, sett_reg, 1, sep = "-"), "%Y-%U-%u"),
        intervallo = case_when(
          sett_reg == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
          sett_reg %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
          sett_reg == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
      plot_ly(x = ~sett_reg, y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', n, 'conferimenti registrati',
                                 #'</br> settimana: ', sett_reg,
                                 '</br>', intervallo)) %>%
      event_register('plotly_click') %>% 
      layout(#title = input$settore,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        xaxis = list(title = 'Settimana',
                     tickformat = ',d'), 
        yaxis = list(title = 'Conferimenti registrati')
        #legend = list(title=list(text='<b> Species of Iris </b>'))
      ) %>%
      config(displayModeBar = FALSE)
    
  } else {
    
    conf %>%
      filter(annoreg == input$selanno, settore == input$settore) %>% 
      distinct(Nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>% 
      mutate(
        anno_reg = sub(".*-","", weekreg),
        sett_reg = as.numeric(sub("-.*","", weekreg)),
        sett_inizio = as.Date(paste(anno_reg, sett_reg, 1, sep = "-"), "%Y-%U-%u"),
        intervallo = case_when(
          sett_reg == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
          sett_reg %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
          sett_reg == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
      plot_ly(x = ~sett_reg, y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', n, 'conferimenti registrati',
                                 #'</br> settimana: ', sett_reg,
                                 '</br>', intervallo)) %>%
      event_register('plotly_click') %>% 
      layout(#title = input$settore,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        xaxis = list(title = 'Settimana',
                     tickformat = ',d'), 
        yaxis = list(title = 'Conferimenti registrati')
        #legend = list(title=list(text='<b> Species of Iris </b>'))
      ) %>%
      config(displayModeBar = FALSE)
  }
})

output$plotacc <- renderPlotly({
  fgacc()
})



## drilldown table from grafico accettazione----

selected_bar <- reactiveVal(NULL)

# get the date of the selected bar
observe({
  selected_date <- event_data(event = "plotly_click", source = "A")$x
  
  selected_bar(selected_date)
})

confacc <- reactive({
  
  if(input$settore == "Tutti"){
    conf %>%
      mutate(Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
             anno_reg = sub(".*-","", weekreg),
             sett_reg = as.numeric(sub("-.*","", weekreg)),
             sett_inizio = as.Date(paste(anno_reg, sett_reg, 1, sep = "-"), "%Y-%U-%u"),
             intervallo = case_when(
               sett_reg == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
               sett_reg %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
               sett_reg == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
      filter(annoreg == input$selanno)
    
  } else {
    conf %>%
      mutate(Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
             anno_reg = sub(".*-","", weekreg),
             sett_reg = as.numeric(sub("-.*","", weekreg)),
             sett_inizio = as.Date(paste(anno_reg, sett_reg, 1, sep = "-"), "%Y-%U-%u"),
             intervallo = case_when(
               sett_reg == 52 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(as.Date(paste(input$selanno, 12, 31, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y")),
               sett_reg %in% 1:51 ~ paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y")),
               sett_reg == 0 ~ paste("dal", format(as.Date(paste(input$selanno, 1, 1, sep = "-"), "%Y-%m-%d"),"%d/%m/%Y"),"al", format(sett_inizio - 1, "%d/%m/%Y")))) %>% 
      filter(annoreg == input$selanno, settore == input$settore)
    
  }
})


output$table <- renderReactable({
  
  if(length(selected_bar())) {
    conf_table <- confacc() %>%
      filter(sett_reg == selected_bar()) %>% 
      select(Nconf2, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
             conferente, dest_rdp, NrCampioni)
    
    reactable(conf_table,
              filterable = TRUE,
              defaultPageSize = 5,
              defaultSorted = list(dtreg = "desc", Nconf2 = "desc"),
              defaultColDef = colDef(
                headerStyle = list(background = "#f5f5f5")),
              columns = list(
                Nconf2 = colDef(name = "Conferimento",
                               minWidth = 90),
                settore = colDef(name = "Settore"),
                tipo_prelievo = colDef(name = "Tipo prelievo",
                                       minWidth = 85),
                finalita = colDef(name = "Finalità"),
                dtprel = colDef(name = "Data prelievo",
                                format = colFormat(date = TRUE),
                                minWidth = 70),
                dtconf = colDef(name = "Data conferimento",
                                format = colFormat(date = TRUE),
                                minWidth = 90),
                dtreg = colDef(name = "Data registrazione",
                               format = colFormat(date = TRUE),
                               minWidth = 90),
                conferente = colDef(name = "Conferente"),
                dest_rdp = colDef(name = "Destinatario RdP"),
                NrCampioni = colDef(name = "Campioni conferiti",
                                    minWidth = 70)
              ),
              language = reactableLang(
                #sortLabel = "Sort {name}",
                #filterPlaceholder = "🔍",
                #filterLabel = "Filter {name}",
                #searchPlaceholder = "Search",
                #searchLabel = "Search",
                noData = "---",
                pageNext = "Successiva",
                pagePrevious = "Precedente",
                pageNumbers = "{page} di {pages}",
                pageInfo = "{rowStart}\u2013{rowEnd} di {rows} conferimenti"
              )
    )
  }
  
})


#ACC-giornaliero----
date_breaks_acc <- reactive({
  
  if(input$settore == "Tutti"){
    
  date_range_acc <- seq(min(as.Date(conf$dtreg[conf$annoreg == input$selanno])), max(as.Date(conf$dtreg[conf$annoreg == input$selanno])), by = 1) 
  date_breaks_acc <- as.character(date_range_acc[!date_range_acc %in% as.Date(conf$dtreg)])
  date_breaks_acc
  
  } else {
    
    date_range_acc <- seq(min(as.Date(conf$dtreg[conf$annoreg == input$selanno])), max(as.Date(conf$dtreg[conf$annoreg == input$selanno])), by = 1) 
    date_breaks_acc <- as.character(date_range_acc[!date_range_acc %in% as.Date(conf$dtreg[conf$settore == input$settore])])
    date_breaks_acc
  }
})

fgacc2 <- reactive({

  if(input$settore == "Tutti"){
    conf %>% 
      mutate(dtreg = as.Date(dtreg)) %>%
      filter(annoreg == input$selanno) %>% 
      distinct(Nconf, .keep_all = TRUE) %>% 
      group_by(dtreg) %>% 
      count() %>% 
      ungroup() %>%
      plot_ly(x = ~dtreg , y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', str_to_title(format(dtreg, "%A %d %B %Y")),
                                 '</br>', n, 'conferimenti registrati')) %>%
      event_register('plotly_click') %>% 
      layout(
        #height = 500,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        yaxis = list(title = 'Conferimenti registrati',
                     fixedrange = FALSE,
                     autorange = TRUE),
        xaxis = list(
          fixedrange = FALSE,
          title = '',
          type = "date",
          
          # tickmode = "linear",
          # dtick = 86400000,
          
          tickformat = "%d %b %y",
          
          # tickformatstops = list(
          #   list(
          #     dtickrange = list(NULL, 86400000),
          #     value = "%d %B")
          #   ,
          #   list(
          #     dtickrange = list(86400000, 604800000),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list(604800000, "M1"),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list("M1", "M12"),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list("M12", NULL),
          #     value = "%d %B")),
          range = c(max(as.Date(conf$dtreg[conf$annoreg == input$selanno]))-7, max(as.Date(conf$dtreg[conf$annoreg == input$selanno]))),
          rangebreaks = list(
            list(values = date_breaks_acc())),
          rangeselector = list(
            buttons = list(
              list(
                count = 7,
                label = "ultima<br>settimana",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "ultimo<br>mese",
                step = "month",
                stepmode = "backward"),
              # list(
              #   count = 3,
              #   label = "ultimi<br>3 mesi",
              #   step = "month",
              #   stepmode = "backward"),
              list(label = "vedi<br>tutto",
                   step = "all"))),
          rangeslider = list(type = "date", visible = TRUE)
        )
      ) %>% 
      config(displayModeBar = FALSE) %>%
      config(locale = 'it')
    
  } else {
    
    conf %>%
      mutate(dtreg = as.Date(dtreg)) %>%
      filter(annoreg == input$selanno, settore == input$settore) %>% 
      distinct(Nconf, .keep_all = TRUE) %>% 
      group_by(dtreg) %>% 
      count() %>% 
      ungroup() %>%
      plot_ly(x = ~dtreg , y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', str_to_title(format(dtreg, "%A %d %B %Y")),
                                 '</br>', n, 'conferimenti registrati')) %>%
      event_register('plotly_click') %>% 
      layout(
        #height = 500,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        yaxis = list(title = 'Conferimenti registrati',
                     fixedrange = FALSE,
                     autorange = TRUE),
        xaxis = list(
          fixedrange = FALSE,
          title = '',
          type = "date",
          
          # tickmode = "linear",
          # dtick = 86400000,
          
          tickformat = "%d %b",
          
          # tickformatstops = list(
          #   list(
          #     dtickrange = list(NULL, 86400000),
          #     value = "%d %B")
          #   ,
          #   list(
          #     dtickrange = list(86400000, 604800000),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list(604800000, "M1"),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list("M1", "M12"),
          #     value = "%d %B"
          #   ),
          #   list(
          #     dtickrange = list("M12", NULL),
          #     value = "%d %B")),
          range = c(max(as.Date(conf$dtreg[conf$annoreg == input$selanno]))-7, max(as.Date(conf$dtreg[conf$annoreg == input$selanno]))),
          rangebreaks = list(
            list(values = date_breaks_acc())),
          rangeselector = list(
            buttons = list(
              list(
                count = 7,
                label = "ultima<br>settimana",
                step = "day",
                stepmode = "backward"),
              list(
                count = 1,
                label = "ultimo<br>mese",
                step = "month",
                stepmode = "backward"),
              # list(
              #   count = 3,
              #   label = "ultimi<br>3 mesi",
              #   step = "month",
              #   stepmode = "backward"),
              list(label = "vedi<br>tutto",
                   step = "all"))),
          rangeslider = list(type = "date", visible = TRUE)
        )
      ) %>% 
      config(displayModeBar = FALSE) %>%
      config(locale = 'it') 
  }
})

output$plotacc2 <- renderPlotly({
  fgacc2()
})


## drilldown table from grafico accettazione----

selected_bar2 <- reactiveVal(NULL)

# get the date of the selected bar
observe({
  selected_date2 <- event_data(event = "plotly_click", source = "A")$x
  
  selected_bar2(selected_date2)
})


confacc2 <- reactive({
  
  if(input$settore == "Tutti"){
    conf %>%
      mutate(dtreg = as.Date(dtreg)) %>% # <- queste righe vanno poi messe in ETL
      filter(annoreg == input$selanno)
    
  } else {
    conf %>%
      mutate(dtreg = as.Date(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoreg == input$selanno, settore == input$settore)
    
  }
})


output$table2 <- renderReactable({
  
  if(length(selected_bar2())) {
    conf_table2 <- confacc2() %>%
      filter(dtreg == selected_bar2()) %>% 
      select(Nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
             conferente, dest_rdp, NrCampioni)
    
    reactable(conf_table2,
              filterable = TRUE,
              defaultPageSize = 5,
              defaultSorted = list(dtreg = "desc", Nconf = "desc"),
              defaultColDef = colDef(
                headerStyle = list(background = "#f5f5f5")),
              columns = list(
                Nconf = colDef(name = "Conferimento",
                               minWidth = 90),
                settore = colDef(name = "Settore"),
                tipo_prelievo = colDef(name = "Tipo prelievo",
                                       minWidth = 85),
                finalita = colDef(name = "Finalità"),
                dtprel = colDef(name = "Data prelievo",
                                format = colFormat(date = TRUE),
                                minWidth = 70),
                dtconf = colDef(name = "Data conferimento",
                                format = colFormat(date = TRUE),
                                minWidth = 90),
                dtreg = colDef(name = "Data registrazione",
                               format = colFormat(date = TRUE),
                               minWidth = 90),
                conferente = colDef(name = "Conferente"),
                dest_rdp = colDef(name = "Destinatario RdP"),
                NrCampioni = colDef(name = "Campioni conferiti",
                                    minWidth = 70)
              ),
              language = reactableLang(
                #sortLabel = "Sort {name}",
                #filterPlaceholder = "🔍",
                #filterLabel = "Filter {name}",
                #searchPlaceholder = "Search",
                #searchLabel = "Search",
                noData = "---",
                pageNext = "Successiva",
                pagePrevious = "Precedente",
                pageNumbers = "{page} di {pages}",
                pageInfo = "{rowStart}\u2013{rowEnd} di {rows} conferimenti"
              )
    )
    
  }
  
})


#DOWNLOAD----
output$downloadConfAcc <- downloadHandler(
  filename = function() {
    paste('Conferimenti al ',
          format(as.Date(substr(max(conf$dtreg[conf$annoreg == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(con) {
    writexl::write_xlsx(
      format_headers = FALSE,
      conf %>%
        filter(annoreg == input$selanno) %>% 
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

output$downloadConfAcc2 <- downloadHandler(
  filename = function() {
    paste('Conferimenti al ',
          format(as.Date(substr(max(conf$dtreg[conf$annoreg == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
          '.xlsx',
          sep='')
  },
  content = function(con) {
    writexl::write_xlsx(
      format_headers = FALSE,
      conf %>%
        filter(annoreg == input$selanno) %>% 
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


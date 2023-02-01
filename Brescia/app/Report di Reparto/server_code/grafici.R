# dtSplot <- reactive(
#   siero %>% filter(prova == input$prove)
# )

#https://plotly-r.com/control-modebar.html
#https://plotly-r.com/controlling-tooltips.html
#https://plotly.com/r/hover-text-and-formatting/

#https://community.plotly.com/t/plotly-hovertemplate-date-time-format/39944/5 #Advanced Hovertemplate
#https://github.com/plotly/plotly.R/issues/733
#https://stackoverflow.com/questions/50003531/r-plotly-hover-label-text-alignment

# Accettazione----

# } else {
#   if (input$settore == "Alimenti Zootecnici"){
#     return()
#     
#   } else { 
#     

##ACC-settimanale----
fgacc <- reactive({
  
  if(input$settore == "Tutti"){
    conf %>% 
      filter(annoconf == 2022) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekconf) %>% 
      count() %>% 
      ungroup() %>%
      mutate(
        anno_conf = sub(".*-","", weekconf),
        sett_conf = as.numeric(sub("-.*","", weekconf)),
        sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
        intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y"))) %>% 
      plot_ly(x = ~sett_conf, y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br> conferimenti registrati: ', n,
                            '</br> settimana: ', sett_conf,
                            '</br>', intervallo)) %>%
      event_register('plotly_click') %>% 
      layout(#title = input$settore,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        xaxis = list(title = 'Settimana'), 
        yaxis = list(title = 'Conferimenti registrati')
        #legend = list(title=list(text='<b> Species of Iris </b>'))
        ) %>%
      config(displayModeBar = FALSE)
    
    } else {
      
      conf %>%
        filter(annoconf == 2022, settore == input$settore) %>% 
        distinct(nconf, .keep_all = TRUE) %>% 
        group_by(weekconf) %>% 
        count() %>% 
        ungroup() %>% 
        mutate(
          anno_conf = sub(".*-","", weekconf),
          sett_conf = as.numeric(sub("-.*","", weekconf)),
          sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
          intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y"))) %>% 
        plot_ly(x = ~sett_conf, y = ~n, type = "bar", source = "A",
                hoverinfo = 'text',
                hovertext = ~paste('</br> conferimenti registrati: ', n,
                                   '</br> settimana: ', sett_conf,
                                   '</br>', intervallo)) %>%
        event_register('plotly_click') %>% 
        layout(#title = input$settore,
          hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                            font = list(family = "Montserrat")),
          font = list(family = 'Montserrat'),
          xaxis = list(title = 'Settimana'), 
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
      mutate(anno_conf = sub(".*-","", weekconf),
             sett_conf = as.numeric(sub("-.*","", weekconf)),
             sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
             intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y"))) %>% # <- queste righe vanno poi messe in ETL
      filter(annoconf == 2022)
    
    } else {
      conf %>%
        mutate(anno_conf = sub(".*-","", weekconf),
               sett_conf = as.numeric(sub("-.*","", weekconf)),
               sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
               intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y"))) %>% # <- queste righe vanno poi messe in ETL
        filter(annoconf == 2022, settore == input$settore)
      
    }
  })


output$table <- renderReactable({
  
  if(length(selected_bar())) {
    conf_table <- confacc() %>%
      filter(sett_conf == selected_bar()) %>% 
      select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
             conferente, dest_rdp, NrCampioni)
    
    reactable(conf_table,
              filterable = TRUE,
              defaultPageSize = 5,
              defaultSorted = list(dtconf = "desc", nconf = "desc"),
              defaultColDef = colDef(
                headerStyle = list(background = "#f5f5f5")),
              columns = list(
                nconf = colDef(name = "Conferimento",
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
                )
              )
    }
  
  })


##ACC-giornaliero----
fgacc2 <- reactive({
  
  if(input$settore == "Tutti"){
    conf %>% 
      mutate(dtconf = as.Date(dtconf)) %>%
      filter(annoconf == 2022) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(dtconf) %>% 
      count() %>% 
      ungroup() %>%
      plot_ly(x = ~dtconf , y = ~n, type = "bar", source = "A",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', str_to_title(format(dtconf, "%A %d %B %Y")),
                                 '</br> conferimenti registrati: ', n)) %>%
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
          range = c(max(as.Date(conf$dtconf[conf$annoconf == 2022]))-7, max(as.Date(conf$dtconf[conf$annoconf == 2022]))),
          rangebreaks = list(
            list(values = date_breaks_acc)),
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
              list(
                count = 3,
                label = "ultimi<br>3 mesi",
                step = "month",
                stepmode = "backward"),
              list(label = "vedi<br>tutto",
                   step = "all"))),
          rangeslider = list(type = "date", visible = TRUE)
          )
        ) %>% 
      config(displayModeBar = FALSE) %>%
      config(locale = 'it')

    } else {
    
    conf %>%
      mutate(dtconf = as.Date(dtconf)) %>%
      filter(annoconf == 2022, settore == input$settore) %>% 
        distinct(nconf, .keep_all = TRUE) %>% 
        group_by(dtconf) %>% 
        count() %>% 
        ungroup() %>%
        plot_ly(x = ~dtconf , y = ~n, type = "bar", source = "A",
                hoverinfo = 'text',
                hovertext = ~paste('</br>', str_to_title(format(dtconf, "%A %d %B %Y")),
                                   '</br> conferimenti registrati: ', n)) %>%
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
            range = c(max(as.Date(conf$dtconf[conf$annoconf == 2022]))-7, max(as.Date(conf$dtconf[conf$annoconf == 2022]))),
            rangebreaks = list(
              list(values = date_breaks_diagn)),
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
                list(
                  count = 3,
                  label = "ultimi<br>3 mesi",
                  step = "month",
                  stepmode = "backward"),
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
      mutate(dtconf = as.Date(dtconf)) %>% # <- queste righe vanno poi messe in ETL
      filter(annoconf == 2022)
    
  } else {
    conf %>%
      mutate(dtconf = as.Date(dtconf)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == 2022, settore == input$settore)
    
  }
})


output$table2 <- renderReactable({
  
  if(length(selected_bar2())) {
    conf_table2 <- confacc2() %>%
      filter(dtconf == selected_bar2()) %>% 
      select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
             conferente, dest_rdp, NrCampioni)
    
    reactable(conf_table2,
              filterable = TRUE,
              defaultPageSize = 5,
              defaultSorted = list(dtconf = "desc", nconf = "desc"),
              defaultColDef = colDef(
                headerStyle = list(background = "#f5f5f5")),
              columns = list(
                nconf = colDef(name = "Conferimento",
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
              )
    )
    
  }
  
})



# Lab Sierologia----

output$Sp1 <- renderPlotly({
  
  #req(input$prove)
  
  siero %>%
    #group_by(prova, tecnica, dtinizio) %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
    ggplot() +
    aes(x = sett, y = n) +
    geom_point(alpha = 0.8, color = "lightgrey") +
    geom_line(group = 1, alpha = 0.8, color = "lightgrey") +
    #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
    theme_bw() +
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
  })



 
output$Sp2 <- renderPlot({
  siero %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>%
    group_by(prova, tecnica,week) %>% 
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
    ggplot() +
    aes(x = week, y = tmesec) +
    geom_line() +
    geom_point() +
    facet_wrap(prova ~ tecnica, scales = "free") +
    theme_bw() +
    labs(#title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
  })


# Lab Diagnostica ----

fg_diagn <- reactive({
  
  if(input$visual_diagn == "Settimanale"){
    diagnostica %>% 
      filter(annoconf == 2022) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekconf) %>% 
      count() %>% 
      ungroup() %>%
      mutate(
        anno_conf = sub(".*-","", weekconf),
        sett_conf = as.numeric(sub("-.*","", weekconf)),
        sett_inizio = as.Date(paste(anno_conf, sett_conf, 1, sep = "-"), "%Y-%U-%u"),
        intervallo = paste("dal", format(sett_inizio, "%d/%m/%Y"),"al", format(sett_inizio + 6, "%d/%m/%Y"))) %>% 
      plot_ly(x = ~sett_conf, y = ~n, type = "bar", source = "C",
              hoverinfo = 'text',
              hovertext = ~paste('</br> conferimenti registrati: ', n,
                                 '</br> settimana: ', sett_conf,
                                 '</br>', intervallo)) %>%
      event_register('plotly_click') %>% 
      layout(#title = input$settore,
        hoverlabel = list(align = "left", #https://plotly.com/r/reference/#layout-hoverlabel-align
                          font = list(family = "Montserrat")),
        font = list(family = 'Montserrat'),
        xaxis = list(title = 'Settimana'), 
        yaxis = list(title = 'Conferimenti registrati')
        #legend = list(title=list(text='<b> Species of Iris </b>'))
      ) %>%
      config(displayModeBar = FALSE)
    
  } else {
    
    diagnostica %>% 
      #mutate(dtconf = as.Date(dtconf)) %>%
      filter(annoconf == 2022) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(dtconf) %>% 
      count() %>% 
      ungroup() %>%
      plot_ly(x = ~dtconf , y = ~n, type = "bar", source = "C",
              hoverinfo = 'text',
              hovertext = ~paste('</br>', str_to_title(format(dtconf, "%A %d %B %Y")),
                                 '</br> conferimenti registrati: ', n)) %>%
      event_register('plotly_click') %>% 
      layout(
        height = 500,
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
          range = c(max(as.Date(conf$dtconf[conf$annoconf == 2022]))-7, max(as.Date(conf$dtconf[conf$annoconf == 2022]))),
          rangebreaks = list(
            list(values = date_breaks_diagn)),
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
              list(
                count = 3,
                label = "ultimi<br>3 mesi",
                step = "month",
                stepmode = "backward"),
              list(label = "vedi<br>tutto",
                   step = "all"))),
          rangeslider = list(type = "date", visible = TRUE)
        )
      ) %>% 
      config(displayModeBar = FALSE) %>%
      config(locale = 'it')
  }
})

output$plot_diagn <- renderPlotly({
  fg_diagn()
})

## drilldown table from grafico accettazione----

selected_bar_diagn <- reactiveVal(NULL)

# get the date of the selected bar
observe({
  selected_date_diagn <- event_data(event = "plotly_click", source = "C")$x
  
  selected_bar_diagn(selected_date_diagn)
})

conf_diagn <- reactive({
    diagnostica %>%
      distinct(nconf, .keep_all = TRUE) %>%
      filter(annoconf == 2022) %>%
      select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
             conferente, dest_rdp, NrCampioni)
  
  
  
  # if(input$visual_diagn == "Settimanale") {
  #   diagnostica %>%
  #     filter(sett_conf == selected_bar_diagn()) %>%
  #     distinct(nconf, .keep_all = TRUE) %>%
  #     filter(annoconf == 2022) %>%
  #     select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
  #            conferente, dest_rdp, NrCampioni)
  #
  # } else {
  #   if(input$visual_diagn == "Giornaliero") {
  #     diagnostica %>%
  #       filter(dtconf == selected_bar_diagn()) %>%
  #       distinct(nconf, .keep_all = TRUE) %>%
  #       filter(annoconf == 2022) %>%
  #       select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg,
  #              conferente, dest_rdp, NrCampioni)
  #
  #  }
  # }
})


output$table_diagn <- renderDataTable({
  
  # if(length(selected_bar_diagn())){
    
    # dt <- conf_diagn()

    #https://glin.github.io/reactable/articles/examples.html#expandable-row-details
    #https://github.com/daattali/shinycssloaders/
    
    DT::datatable(conf_diagn(),
                  style = 'bootstrap',
                  rownames = F,
                  selection = 'single',
                  options = list(
                    class = 'compact row-border',
                    dom = 'tip',
                    pageLength = 5))

    
  # }
  
})





output$Dp1 <- renderPlotly({

  #req(input$prove)
  
  diagnostica %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA)) %>% 
    ggplot() +
    aes(x = sett, y = n) +
    geom_point(alpha = 0.8, color = "lightgrey") +
    geom_line(group = 1, alpha = 0.8, color = "lightgrey") +
    #geom_line(aes(y = sett, x =dtinizio), color = "blue") +  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free") +
    theme_bw() +
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
  })




output$Dp2 <- renderPlot({
  req(input$diagnos)
  
  diagnostica %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>%
    group_by(prova, week) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
    filter(prova == input$diagnos) %>% 
    ggplot() +
    aes(x = week, y = tmesec) +
    geom_line() +
    geom_point() +
    #facet_wrap(prova ~ tecnica, scales = "free")+
    theme_bw() +
    labs(title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
  })





















# Lab Microbiologia Alimenti ------------------------------------------------------------------

output$MAp1 <- renderPlotly({
  
  #req(input$prove)
  
  alimenti %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA)) %>% 
    ggplot() +
    aes(x = sett, y = n) +
    geom_point(alpha = 0.8, color = "lightgrey") +
    geom_line(group = 1, alpha = 0.8, color = "lightgrey") +
    #geom_line(aes(y = sett, x = dtinizio ), color = "blue") +  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free") +
    theme_bw() +
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
  })




output$MAp2 <- renderPlot({
  
  req(input$microalim)
  
  alimenti %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>%
    group_by(prova, week) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE), 1)) %>%
    filter(prova == input$microalim) %>% 
    ggplot() +
    aes(x= week, y = tmesec) +
    geom_line() +
    geom_point() +
    #facet_wrap(prova ~ tecnica, scales = "free") +
    theme_bw() +
    labs(title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
  })








#*----
###Laboratorio Biologia Molecolare ------------------------------------------------------------------

# output$bmp1 <- renderPlotly({
#   
#   #req(input$prove)
#   
#   biomol %>%
#     mutate(sett = week(dtinizio)) %>% 
#     group_by(sett) %>% 
#     count() %>% 
#     ungroup() %>% 
#     #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
#     ggplot()+
#     aes(x=sett, y=n)+
#     geom_point(alpha = 0.8, color = "lightgrey")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
#     #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
#     #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
#     theme_bw()+
#     labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
# })
# 
# 
# 
# 
# output$bmp2 <- renderPlot({
#   req(input$biomolec)
#   biomol %>%
#     mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
#            week = week(dtfine)) %>% 
#     filter(tempo_esecuzione >= 0) %>% 
#     group_by(prova, week) %>%
#     summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>%
#     filter(prova == input$biomolec) %>% 
#     ggplot()+
#     aes(x= week, y = tmesec)+
#     geom_line()+ geom_point()+
#     #facet_wrap(prova ~ tecnica, scales = "free")+
#     theme_bw()+
#     labs(title = "andamento settimanale tempi medi di esecuzione",
#          y = "tempo medio di esecuzione prove",
#          x = "settimana")
# })
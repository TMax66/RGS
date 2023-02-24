#GRAFICI output$p1
#ESCLUSO ALIMENTI ZOOTECNICI - SCRIVI CODICE IN CASO DI DATASET VUOTO

confMap <- reactive({
    conf %>% 
    mutate(ASL1 = case_when(
      ASL == "DISTRETTO 1 DI BRESCIA" ~ paste0("A.T.S. BRESCIA - DISTR. BRESCIA - GARDONE V.T."),
      ASL == "DISTRETTO 4 DI ROVATO"  ~ paste0("A.T.S. BRESCIA - DISTR. ROVATO"),
      ASL == "DISTRETTO 2 DI LONATO" ~ paste0("A.T.S. BRESCIA - DISTR. LONATO"),
      ASL == "DISTRETTO 3 DI LENO"  ~ paste0("A.T.S. BRESCIA - DISTR. LENO"),
      ASL == "DISTRETTO VALCAMONICA - SEBINO"  ~ paste0("A.T.S. MONTAGNA - DISTR. VALCAMONICA SEBINO"),
      TRUE ~ ASL)) %>% 
    filter(!ASL1 %in% c("A.T.S. MONTAGNA - DISTR. MEDIO ALTO LARIO",
                        "A.T.S. MONTAGNA - DISTR. VALTELLINA CENTRO",
                        "A.T.S. MONTAGNA - DISTR. VALTELLINA OVEST")) %>% 
    filter(anno == input$selanno)
})

confer <- reactive({
  confMap() %>%  
    group_by(ASL1) %>%
    distinct(Nconf, .keep_all = TRUE) %>%
    summarise('Conferimenti' = n()) %>% 
    bind_cols(
      confMap() %>%  
        group_by(ASL1) %>% 
        distinct(Nconf, .keep_all = TRUE) %>% 
        summarise('Campioni' = sum(NrCampioni)) %>%
        ungroup() %>% 
        dplyr::select(Campioni) %>%
        
        bind_cols(
          confMap() %>% 
            left_join(nesami, by = "Nconf") %>%
            group_by(ASL1) %>% 
            summarise('Esami' = sum(n, na.rm = TRUE)) %>%
            ungroup() %>% 
            dplyr::select(Esami)) 
    )
})

labels <- reactive({
  paste("<strong>",
        distretti$distretto3,
        "</strong><br>",
        
        "Conferimenti accettati:",
        confer()$Conferimenti,
        "<br>",
        "Campioni conferiti",
        confer()$Campioni,
        "<br>",
        "Esami eseguiti",
        confer()$Esami) %>%
  lapply(htmltools::HTML)
})



#MAPPA----
output$mappa_home <- renderLeaflet({
  
  leaflet(
    options = leafletOptions(
      zoomControl = FALSE,
      attributionControl = FALSE,
      minZoom = 9, maxZoom = 9)
    ) %>%
    addTiles() %>%
    setView(10.27, 45.78, zoom = 9) %>% 
    addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
    addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
    addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 1, color = "black") %>%
    addPolygons(data = distretti, stroke = TRUE, weight = 1,
                fillOpacity = 0.7,
                fillColor = ~factpal(distretto),
                label = ~labels()) %>% 
    #addPolygons(data = comuni, stroke = TRUE, weight = 1, fill = FALSE, color = "white") %>% 
    addLabelOnlyMarkers(data = distretti[-c(1),], ~lng, ~lat,
                        label = ~labels2[-1], 
                        labelOptions = labelOptions(
                          noHide = T,
                          direction = 'center',
                          textOnly = T,
                          style = list(
                            "font-family" = "Montserrat",
                            "color" = "black",
                            "font-size" = "9px",
                            "border" = "2px solid #000",
                            "background" = "white",
                            "padding" = "0px 5px 0px 5px",
                            "text-align" = "center"
                          ))) %>% 
    addLabelOnlyMarkers(data = distretti[c(1),], ~lng, ~lat,
                        label = ~labels2[1], 
                        labelOptions = labelOptions(
                          noHide = T,
                          direction = 'top',
                          textOnly = T,
                          style = list(
                            "font-family" = "Montserrat",
                            "color" = "black",
                            "font-size" = "9px",
                            "border" = "2px solid #000",
                            "background" = "white",
                            "padding" = "0px 5px 0px 5px",
                            "text-align" = "center"
                          ))) %>% 
    addPolygons(data = ASL_ATS, stroke = TRUE, fill =  FALSE, weight = 1.5, color = "black", opacity = 1) 
  # addCircleMarkers(lng=coord$lng, lat = coord$lat, radius = 0.5 , label = coord$codaz)
})


#TABELLE----
##Sanità Animale----

confHSA <- reactive({
  conf %>%
    filter(annoreg == input$selanno, settore == "Sanità Animale")
})

output$t1 <- renderDataTable(
                         confHSA() %>%
                           filter(codaz != "000IX000") %>%
                           group_by(ASL) %>% 
                           distinct(codaz) %>% 
                           #mutate(ASL = gsub(".*- ","",ASL)) %>% 
                           summarise('Allevamenti controllati' = n()) %>%
                           
                           bind_cols(
                             confHSA() %>%
                               group_by(ASL) %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               summarise('Campioni conferiti' = sum(NrCampioni)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             esami <- confHSA() %>%
                               left_join(nesami, by = "Nconf") %>%
                               ungroup() %>%
                               group_by(ASL) %>% 
                               summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             confHSA() %>%
                               group_by(ASL) %>% 
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
                               summarise('Tempo medio consegna' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             confHSA() %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
                               group_by(ASL) %>% 
                               summarise('Tempo medio attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>% 
                               dplyr::select(-ASL)) %>%
                           rename(Distretto = ASL) %>%
                           
                           adorn_totals(name = "TOTALE ATS", col = 2:4) %>%
                           
                           datatable(rownames = FALSE,
                                     class = "row-border",
                                     style = 'bootstrap',
                                     selection = 'none',
                                     options = list(
                                       autowidth = TRUE,
                                       dom = 't',
                                       ordering = F,
                                       columnDefs = list(
                                         list(width = '130px', targets = c(1)),
                                         list(className = 'dt-right', targets = c(1,2,3,4,5)),
                                         list(className = 'dt-left', targets = c(0))) 
                                     )
                         )
)


##Alimenti Uomo----

confHAU <- reactive({
  conf %>% filter(annoreg == input$selanno, settore == "Alimenti Uomo")
})

output$t2 <- renderDataTable(
                         confHAU() %>%
                           group_by(ASL) %>% 
                           distinct(Nconf) %>% 
                           #mutate(ASL = gsub(".*- ","",ASL)) %>% 
                           summarise('Controlli effettuati' = n()) %>%
                           
                           bind_cols(
                             confHAU() %>%  
                               group_by(ASL) %>% 
                               distinct(Nconf, .keep_all = TRUE) %>%  
                               summarise('Campioni conferiti' = sum(NrCampioni)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             esami <- confHAU() %>%
                               left_join(nesami, by = "Nconf") %>%
                               ungroup() %>% 
                               group_by(ASL) %>% 
                               summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             confHAU() %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
                               group_by(ASL) %>% 
                               summarise('Tempo medio consegna' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
                               dplyr::select(-ASL)) %>% 
                           
                           bind_cols(
                             confHAU() %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>% 
                               group_by(ASL) %>% 
                               summarise('Tempo medio attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>% 
                               dplyr::select(-ASL)) %>%
                           rename(Distretto = ASL) %>%
                           
                           adorn_totals(name = "TOTALE ATS", col = 2:4)%>%
                           
                           datatable(rownames = FALSE,
                                     class = "row-border",
                                     style = 'bootstrap',
                                     selection = 'none',
                                     options = list(
                                       autowidth = TRUE,
                                       dom = 't',
                                       ordering = F,
                                       columnDefs = list(
                                         list(width = '130px', targets = c(1)),
                                         list(className = 'dt-right', targets = c(1,2,3,4,5)),
                                         list(className = 'dt-left', targets = c(0))) 
                                     )
                           )
)

##Alimenti Zootecnici----

confHAZ <- reactive({
  conf %>% filter(annoreg == input$selanno, settore == "Alimenti Zootecnici")
})

output$t3 <- renderDataTable(
                         confHAZ() %>%
                           group_by(ASL) %>%
                           distinct(Nconf) %>%
                           #mutate(ASL = gsub(".*- ","",ASL)) %>%
                           summarise('Controlli effettuati' = n()) %>%
                           
                           bind_cols(
                             confHAZ() %>%
                               group_by(ASL) %>%
                               distinct(Nconf, .keep_all = TRUE) %>%  
                               summarise('Campioni conferiti' = sum(NrCampioni)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             esami <- confHAZ() %>%
                               left_join(nesami, by = "Nconf") %>%
                               ungroup() %>% 
                               group_by(ASL) %>% 
                               summarise('Esami eseguiti' = sum(n, na.rm = TRUE)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             confHAZ() %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
                               group_by(ASL) %>% 
                               summarise('Tempo medio consegna' = round(mean(tempo_consegna, na.rm = TRUE), 1)) %>% 
                               dplyr::select(-ASL)) %>%
                           
                           bind_cols(
                             confHAZ() %>%
                               distinct(Nconf, .keep_all = TRUE) %>%
                               mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf))) %>% 
                               group_by(ASL) %>% 
                               summarise('Tempo medio attesa esito' = round(mean(tempo_attesa_esito, na.rm = TRUE), 1))%>%  
                               dplyr::select(-ASL))%>%
                           rename(Distretto = ASL) %>%
                           
                           adorn_totals(name = "TOTALE ATS", col = 2:4) %>%
                           
                           datatable(rownames = FALSE,
                                     class = "row-border",
                                     style = 'bootstrap',
                                     selection = 'none',
                                     options = list(
                                       autowidth = TRUE,
                                       dom = 't',
                                       ordering = F,
                                       columnDefs = list(
                                         list(width = '130px', targets = c(1)),
                                         list(className = 'dt-right', targets = c(1,2,3,4,5)),
                                         list(className = 'dt-left', targets = c(0))) 
                                     )
                           )
)


#GRAFICI----
output$p1 <- renderPlotly({
  
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
  
  # else {
  #   if (input$settore == "Alimenti Zootecnici"){
  #     return()
  #     
  #   }
  
    else { 
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
      
    # }
      
  }
})
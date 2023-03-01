#mappa iniziale----

output$buffmap <- renderLeaflet({
  
  leaflet(
    options = leafletOptions(
      attributionControl = FALSE)
    ) %>% 
    #addTiles() %>%
    setView(10.27, 45.78, zoom = 9) %>% 
    addProviderTiles(provider = "CartoDB.Positron") %>% # addProviderTiles() in alternativa a addTiles()
    addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
    addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
    addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 3, color = "black") %>%
    addPolygons(data = comuni, stroke = TRUE, fill =  FALSE, weight = 0.5, color = "gray") %>%
    addMeasure(position = "topright",
               primaryLengthUnit = "meters",
               secondaryLengthUnit = "kilometers",
               primaryAreaUnit = "hectares",
               secondaryAreaUnit = NULL,
               activeColor = "#1f77b4",
               localization = "it",
               decPoint = ",",
               thousandsSep = ".") %>%
    addEasyButton(easyButton(
      icon = "fa-search-minus", title = "Reset Zoom",
      onClick = JS("function(btn, map){ map.setView([45.78, 10.27], 9); }")))
  
  
})


#FOCOLAI ALLEVAMENTI DI INTERESSE----
##estrazione informazioni focolaio da codice aziendale----
focolaio <- reactive(
  coordbv %>%
    filter(codaz == input$codaz2)
)

#allentro <- reactive(entro)


##estrazione allevamenti NEL BUFFER----

foc <- reactive({
  focolaio() %>% dplyr::select(long, lat)
  })

all <- reactive({
  coordbv %>%
    dplyr::select(codaz, COMUNE, XT_DENOMINAZIONE, long, lat) %>%
    na.omit()
  })

protezione <- reactive({
  pointDistance(foc(), all()[, 4:5], lonlat = TRUE)<=input$area
  })

entro <- reactive({
  subset(all(), protezione())
  })

#allcod <- reactive({all()} ) # Deseleziona gli allevamenti, serve per resettare la mappa iniziale

observeEvent(input$codaz2,{   # inserito il codice azienda lo mappa sulla cartina con un marker
  
  leafletProxy("buffmap",session, data = focolaio()) %>%
    addMarkers(~long, ~lat)
  
  })

##mappa----
observeEvent( # cliccando genera mappa produce il buffer con gli allevamenti entro il raggio indicato e produce la tabella
  input$prot,
  {
    #req(nchar(input$codaz2)==8)
    
    if(input$codaz2 %in% coordbv$codaz)
    {
      removeUI("#allfoc")
      removeUI("#mappacamp") #cancella la tabella ...    
      
      leafletProxy("buffmap", session, data = entro()) %>%
        clearMarkers() %>% clearShapes() %>%
        #setView(10.27, 45.78, zoom = 9) %>%
        addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
        addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
        addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>%
        addPolygons(data = comuni, stroke = TRUE, fill =  FALSE, weight = 0.5, color = "gray") %>% 
        addMarkers(~focolaio()$long,  ~focolaio()$lat) %>% 
        fitBounds(lng1 = max(entro()$long),lat1 = max(entro()$lat),
                  lng2 = min(entro()$long),lat2 = min(entro()$lat)) %>%
        addCircles( ~focolaio()$long, ~focolaio()$lat, radius = input$area, weight = 1) %>% 
        addCircles(~long, ~lat, radius = 50, weight = 2, color = "red",
                   label = paste("Codice Azienda:",
                                 #"<br>",
                                 "<strong>",
                                 entro()$codaz,
                                 "</strong>",
                                 "<br>",
                                 "Denominazione Azienda:",
                                 #"<br>",
                                 "<strong>",
                                 entro()$XT_DENOMINAZIONE,
                                 "</strong>") %>%
                     lapply(htmltools::HTML)
        ) 
      
      insertUI("#placeholder", "afterEnd",
               ui = div(id = "allfoc",
                        wellPanel(style="width: 1000px;",
                          h4("Elenco Aziende selezionate",style="text-align: center; font-weight: 600; margin-bottom: 20px;"),                                   
                 entro() %>% 
                 #left_join(coordbv, by = "codaz") %>%
                 select(codaz, XT_DENOMINAZIONE, COMUNE) %>% #, `Tipologia Allevamento`, "Indirizzo" = `Indirizzo sede Allev`) %>% 
                 datatable(rownames = FALSE,
                           style = 'bootstrap',
                           colnames = c("Codice Azienda",#0
                                        "Denominazione Azienda",#1
                                        "Comune"), #2
                           # "tipologia allevamento",#2
                           # "indirizzo"),#3
                           selection = 'none',
                           extensions = 'Buttons',
                           filter = c('top'),
                           callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),
                           options = list(
                             order = list(list(0, 'asc')),
                             searching = TRUE,
                             autoWidth = TRUE,
                             dom = 'Brltip',
                             pageLength = 5,
                             lengthMenu = list(c(5, 25, 50, -1), 
                                               c('5', '25', '50', 'Tutti')),
                             buttons = list(
                               list(extend = "excel", text = "Scarica Tutto",
                                    filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
                                    title = NULL,
                                    titleAttr = "Excel",
                                    exportOptions = list(
                                      modifier = list(page = "all"),
                                      columns = c(0, 1, 2)))
                               # ,
                               # list(extend = "excel", text = "Scarica Selezione",
                               #      filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
                               #      title = NULL,
                               #      titleAttr = "Excel",
                               #      exportOptions = list(
                               #        modifier = list(page = "current"),
                               #        columns = c(0,1)))
                             ),
                             columnDefs = list(
                               #list(className = 'dt-body-right', targets = c(0)),
                               list(width = '150px', targets = c(0))
                               # list(width = '300px', targets =c(1)),
                               # list(width = '300px', targets = c(2))
                             ),
                             language = list(decimal = ",",
                                             thousands = ".",
                                             search = "Cerca: ",
                                             lengthMenu = "Mostra _MENU_ aziende",
                                             paginate = list(previous = "Precedente", `next` = "Successiva"),
                                             info = "_START_ - _END_ di _TOTAL_ aziende",
                                             infoFiltered = "(su un totale di _MAX_ aziende)",
                                             infoEmpty = "Nessuna azienda disponibile",
                                             zeroRecords = "---"))
                 )))) # questo codice permette di far ricomparire
      # la tabella dopo aver cliccato su reset
    }
    else {
      showModal(
        div(id = "codazModalDiv",
            modalDialog(
              title = HTML("<strong>Attenzione!</strong>"),
              HTML("Il Codice Azienda selezionato non è disponibile."),
              # <br><br>
              # <p><em>nota: ogni allevamento è individuato da un codice aziendale alfanumerico di 8 cifre.</em></p>"),
              easyClose = TRUE,
              footer = modalButton("Chiudi"))
            )
        )
      }
    
  })


observeEvent(
  input$unprot,
  {
    
    reset("codaz2")# cancella l'input codice
    reset("area")#cancella l'input slider
    
    removeUI("#mappacamp")
    removeUI("#allfoc") #cancella la tabella ...

    leafletProxy("buffmap", session, data = entro()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      setView(10.27, 45.78, zoom = 9) %>% 
      addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>%
      addPolygons(data = comuni, stroke = TRUE, fill =  FALSE, weight = 0.5, color = "gray") 
    
  })


# output$allfoc <- DT::renderDataTable({ 
#   
#   entro() %>% 
#     left_join(coordbv, by = "codaz") %>%
#     select(codaz, COMUNE) %>% #, `Tipologia Allevamento`, "Indirizzo" = `Indirizzo sede Allev`) %>% 
#     datatable(rownames = FALSE,
#               style = 'bootstrap',
#               colnames = c("codice azienda",#0
#                            "comune"), #1
#                            # "tipologia allevamento",#2
#                            # "indirizzo"),#3
#               selection = 'none',
#               extensions = 'Buttons',
#               filter = c('top'),
#               callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),
#               options = list(
#                 order = list(list(0, 'asc')),
#                 searching = TRUE,
#                 autoWidth = TRUE,
#                 dom = 'Brltip',
#                 pageLength = 5,
#                 lengthMenu = list(c(5, 25, 50, 100, -1), 
#                                   c('5', '25', '50', '100', 'Tutti')),
#                 buttons = list(
#                   list(extend = "excel", text = "Scarica Tutto",
#                        filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
#                        title = NULL,
#                        titleAttr = "Excel",
#                        exportOptions = list(
#                          modifier = list(page = "all"),
#                          columns = c(0,1))),
#                   list(extend = "excel", text = "Scarica Selezione",
#                        filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
#                        title = NULL,
#                        titleAttr = "Excel",
#                        exportOptions = list(
#                          modifier = list(page = "current"),
#                          columns = c(0,1)))
#                 ),
#                 columnDefs = list(
#                   list(className = 'dt-head-center', targets = "_all"),
#                   list(width = '250px', targets =c(0)),
#                   #list(width = '300px', targets =c(3)),
#                   list(width = '250px', targets =c(1))
#                 ),
#                 language = list(search = "Cerca: ",
#                                 lengthMenu = "Mostra _MENU_ aziende",
#                                 paginate = list(previous = "Precedente", `next` = "Successiva"),
#                                 info = "_START_ - _END_ di _TOTAL_ aziende",
#                                 infoFiltered = "(su un totale di _MAX_ aziende)",
#                                 infoEmpty = "Nessuna azienda disponibile",
#                                 zeroRecords = "---"))
#     )
# })




#ALLEVAMENTI CAMPIONATI----


campmappa <- reactive({
  proveSA %>%
    distinct(Nconf, .keep_all = TRUE) %>% 
    filter(annoiniz == input$selanno, finalita == input$finalitamappa) %>%
    mutate(codaz = toupper(codaz)) %>%  
    left_join(coordbv, by = "codaz")
  
})


observeEvent( #cliccando genera mappa produce la mappa con gli allevamenti controllati per il campionamento selezionato
  input$prot2,
  {
    req(input$finalitamappa)
    
    removeUI("#allfoc")
    removeUI("#mappacamp") #cancella la tabella ...    
    
    leafletProxy("buffmap", session, data = campmappa()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.27, 45.78, zoom = 9) %>% 
      addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>%
      addPolygons(data = comuni, stroke = TRUE, fill =  FALSE, weight = 0.5, color = "gray") %>% 
    addCircles(~long,  ~lat, radius = 200, weight = 2, color = "red",
                 label = paste("Comune:",
                               #"<br>",
                               "<strong>",
                               campmappa()$comune,
                               "</strong>",
                               "<br>",
                               "Codice Azienda:",
                               #"<br>",
                               "<strong>",
                               campmappa()$codaz,
                               "</strong>",
                               "<br>",
                               "Denominazione Azienda:",
                               #"<br>",
                               "<strong>",
                               campmappa()$XT_DENOMINAZIONE,
                               "</strong>") %>%
                   lapply(htmltools::HTML))
    
    
    insertUI("#placeholder2", "afterEnd",
             ui = div(id = "mappacamp",
                      wellPanel(
                        h4("Elenco Aziende selezionate",style="text-align: center; font-weight: 600; margin-bottom: 20px;"),
                  campmappa() %>%
               dplyr::select(Nconf2, Nconf3, codaz, comune, dtprel, dtprel_chr, dtconf, dtconf_chr,
                             specie, NrCampioni, NrCampioni_chr) %>% 
               datatable(style = 'bootstrap',
                         colnames = c("Conferimento_numeric",#0
                                      "Conferimento",#1
                                      "Codice Azienda",#2
                                      "Comune", #3
                                      "data prelievo_numeric",#4
                                      "Data Prelievo",#5
                                      "data conferimento_numeric",#6
                                      "Data Conferimento",#7
                                      "Specie",#8
                                      "campioni_accettati_numeric",#9
                                      "Campioni accettati"),#10
                         rownames = FALSE,
                         selection = 'none',
                         extensions = 'Buttons',
                         filter = c('top'),
                         callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
                         options = list(
                           order = list(list(6, 'desc')),
                           dom = 'Brltip', #irltp
                           scrollX = TRUE,
                           searching = TRUE,
                           autoWidth = TRUE,
                           pageLength = 5,
                           lengthMenu = list(c(5, 25, 50, -1), 
                                             c('5', '25', '50', 'Tutti')),
                           buttons = list(
                             list(extend = "excel", text = "Scarica Tutto",
                                  filename = paste0(input$finalitamappa, " - attività ufficiale ", input$selanno, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
                                  title = NULL,
                                  titleAttr = "Excel",
                                  exportOptions = list(
                                    modifier = list(page = "all"),
                                    columns = c(1,2,3,5,7,8,10)))
                             # ,
                             # list(extend = "excel", text = "Scarica Selezione",
                             #      filename = paste0(input$finalitamappa, " - attività ufficiale ", input$selanno, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
                             #      title = NULL,
                             #      titleAttr = "Excel",
                             #      exportOptions = list(
                             #        modifier = list(page = "current"),
                             #        columns = c(1,2,3,5,7,8,10)))
                           ),
                           columnDefs = list(
                             list(orderData = 4, targets = 5),
                             list(orderData = 6, targets = 7),
                             list(orderData = 0, targets = 1),
                             list(visible = FALSE, targets = c(0, 4, 6, 9)),
                             list(className = 'dt-body-right', targets = c(1, 5, 7, 10)),
                             #list(className = 'dt-body-center', targets = c(5, 7)),
                             list(className = 'dt-head-center', targets = "_all"),
                             list(width = '130px', targets = c(1, 2)),
                             list(width = '50px', targets = c(10)),
                             #list(width = '130px', targets =c(3)),
                             list(width = '130px', targets = c(5, 7))
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
                         )))
               )) # questo codice permette di far ricomparire
    # la tabella dopo aver cliccato su reset
    
  })

observeEvent(
  input$unprot2,
  {
    
    # reset("anno4")# cancella l'input codice
    reset("finalitamappa")#cancella l'input slider
    
    removeUI("#mappacamp") #cancella la tabella ...
    
    leafletProxy("buffmap", session, data = campmappa()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.27, 45.78, zoom = 9) %>% 
      addPolygons(data = regione, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = province_regione, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = provincia, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>%
      addPolygons(data = comuni, stroke = TRUE, fill =  FALSE, weight = 0.5, color = "gray") 
  })



# output$mappacamp <- DT::renderDataTable({ 
#   
#   campmappa() %>%
#     mutate(dtprel_chr = format(dtprel, "%d/%m/%Y"),
#            dtconf_chr = format(dtconf, "%d/%m/%Y"),
#            nconf_chr = as.character(nconf),
#            ncamp_accettati_chr = as.character(ncamp_accettati),
#            specie = str_to_sentence(specie),
#            materiale = str_to_sentence(materiale)) %>% 
#     dplyr::select(nconf,nconf_chr, codaz, comune, dtprel, dtprel_chr, dtconf, dtconf_chr,
#                   specie, ncamp_accettati, ncamp_accettati_chr) %>% 
#     datatable(style = 'bootstrap',
#               colnames = c("conferimento_0",#0
#                            "conferimento",#1
#                            "codice azienda",#2
#                            "comune", #3
#                            "data prelievo_0",#4
#                            "data prelievo",#5
#                            "data conferimento_0",#6
#                            "data conferimento",#7
#                            "specie",#8
#                            "ncamp_accettati_0",#9
#                            "campioni accettati"),#10
#               rownames = FALSE,
#               selection = 'none',
#               extensions = 'Buttons',
#               filter = c('top'),
#               callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
#               options = list(
#                 order = list(list(6, 'desc'), list(4, 'desc')),
#                 dom = 'Brltip', #irltp
#                 scrollX = TRUE,
#                 searching = TRUE,
#                 autoWidth = TRUE,
#                 pageLength = 5,
#                 lengthMenu = list(c(5, 25, 50, 100, -1), 
#                                   c('5', '25', '50', '100', 'Tutti')),
#                 buttons = list(
#                   list(extend = "excel", text = "Scarica Tutto",
#                        filename = paste0(input$finalitamappa, " - attività ufficiale ", input$anno4, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
#                        title = NULL,
#                        titleAttr = "Excel",
#                        exportOptions = list(
#                          modifier = list(page = "all"),
#                          columns = c(1,2,3,5,7,8,10))),
#                   list(extend = "excel", text = "Scarica Selezione",
#                        filename = paste0(input$finalitamappa, " - attività ufficiale ", input$anno4, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
#                        title = NULL,
#                        titleAttr = "Excel",
#                        exportOptions = list(
#                          modifier = list(page = "current"),
#                          columns = c(1,2,3,5,7,8,10)))
#                 ),
#                 columnDefs = list(
#                   list(orderData = 4, targets = 5),
#                   list(orderData = 6, targets = 7),
#                   list(orderData = 0, targets = 1),
#                   list(visible = FALSE, targets = c(0, 4, 6, 9)),
#                   list(className = 'dt-body-right', targets = c(10)),
#                   list(className = 'dt-body-center', targets = c(1, 5, 7)),
#                   list(className = 'dt-head-center', targets = "_all"),
#                   list(width = '130px', targets =c(1, 2)),
#                   list(width = '50px', targets =c(10)),
#                   list(width = '130px', targets =c(3)),
#                   list(width = '130px', targets =c(5, 7))
#                   # list(width = '60px', targets =c(6, 7))
#                 ),
#                 language = list(search = "Cerca: ",
#                                 lengthMenu = "Mostra _MENU_ conferimenti",
#                                 paginate = list(previous = "Precedente", `next` = "Successiva"),
#                                 info = "_START_ - _END_ di _TOTAL_ conferimenti",
#                                 infoFiltered = "(su un totale di _MAX_ conferimenti)",
#                                 infoEmpty = "Nessun conferimento disponibile",
#                                 zeroRecords = "---")        
#               )
#     )
# })

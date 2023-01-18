#codice per generare mappa iniziale in tab Mappe ( SANITA' ANIMALE)----

output$mobuffmap <- renderLeaflet({
  
  leaflet() %>% 
    addTiles() %>%
    setView(10.89347, 44.53753, zoom = 9) %>% 
    addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>% 
    addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
    addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>% 
    addPolygons(data = commo, stroke = TRUE, fill =  FALSE, weight = 1.5, color = "gray")
  
})



###  

#Codice per generale allevamenti in un buffer da un allevamento input----

##estrazione informazioni focolaio da codice aziendale----
focolaio <- reactive(
  coordbv %>%
    filter(codaz == input$codaz2)
)

allentro <- reactive(entro)


##estrazione allevamenti NEL BUFFER----

foc <- reactive(focolaio() %>% dplyr::select(long, lat))
all <- reactive(coordbv %>% dplyr::select(codaz, long, lat) %>% na.omit())

protezione <- reactive({pointDistance(foc(), all()[, 2:3], lonlat = TRUE)<=input$area})

entro <- reactive({ subset(all(), protezione())})

allcod <- reactive({all()} ) # Deseleziona gli allevamenti, serve per resettare la mappa iniziale


observeEvent( #inserito il codice lo mappa sulla cartina con un marker
  input$codaz2,
  {
    leafletProxy("mobuffmap",session, data = focolaio() ) %>%
      addMarkers(~long,  ~lat) #%>%
    #addCircles( ~long,  ~lat, radius =10000, weight = 1)
  })

observeEvent( #cliccando genera mappa produce il buffer con gli allevamenti entro il raggio indicato e produce la tabella
  input$prot,
  {
    #req(nchar(input$codaz2)==8)
    
    if(input$codaz2 %in% coordbv$codaz)
    {
      removeUI("#allfoc")
      removeUI("#mappacamp") #cancella la tabella ...    
      
      leafletProxy("mobuffmap", session, data = entro()) %>%
        clearMarkers() %>% clearShapes() %>%
        addMarkers(~focolaio()$long,  ~focolaio()$lat) %>% 
        fitBounds(lng1 = max(entro()$long),lat1 = max(entro()$lat),
                  lng2 = min(entro()$long),lat2 = min(entro()$lat)) %>%
        addCircles( ~focolaio()$long, ~focolaio()$lat, radius = input$area, weight = 1) %>% 
        addCircles(~long, ~lat, radius = 50, weight = 2, color = "red",
                   label = paste("Codice Azienda:",
                                 #"<br>",
                                 "<strong>",
                                 entro()$codaz,
                                 "</strong>") %>%
                     lapply(htmltools::HTML)
        ) 
      
      insertUI("#placeholder", "afterEnd", ui = div(id = "allfoc", entro() %>% 
                 left_join(coordbv, by = "codaz") %>%
                 select(codaz, COMUNE) %>% #, `Tipologia Allevamento`, "Indirizzo" = `Indirizzo sede Allev`) %>% 
                 datatable(rownames = FALSE,
                           style = 'bootstrap',
                           colnames = c("codice azienda",#0
                                        "comune"), #1
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
                             lengthMenu = list(c(5, 25, 50, 100, -1), 
                                               c('5', '25', '50', '100', 'Tutti')),
                             buttons = list(
                               list(extend = "excel", text = "Scarica Tutto",
                                    filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
                                    title = NULL,
                                    titleAttr = "Excel",
                                    exportOptions = list(
                                      modifier = list(page = "all"),
                                      columns = c(0,1))),
                               list(extend = "excel", text = "Scarica Selezione",
                                    filename = paste("Codice Azienda", input$codaz2, "- Area buffer di", input$area/1000, "km"),
                                    title = NULL,
                                    titleAttr = "Excel",
                                    exportOptions = list(
                                      modifier = list(page = "current"),
                                      columns = c(0,1)))
                             ),
                             columnDefs = list(
                               list(className = 'dt-head-center', targets = "_all"),
                               list(width = '250px', targets =c(0)),
                               #list(width = '300px', targets =c(3)),
                               list(width = '250px', targets =c(1))
                             ),
                             language = list(search = "Cerca: ",
                                             lengthMenu = "Mostra _MENU_ aziende",
                                             paginate = list(previous = "Precedente", `next` = "Successiva"),
                                             info = "_START_ - _END_ di _TOTAL_ aziende",
                                             infoFiltered = "(su un totale di _MAX_ aziende)",
                                             infoEmpty = "Nessuna azienda disponibile",
                                             zeroRecords = "---"))
                 ))) # questo codice permette di far ricomparire
      # la tabella dopo aver cliccato su reset
    }
    else {
      showModal(div(id="codazModalDiv", modalDialog(
        title = HTML("<strong>Attenzione!</strong>"),
        HTML("Il Codice Azienda selezionato non è disponibile."),
        # <br><br>
        # <p><em>nota: ogni allevamento è individuato da un codice aziendale alfanumerico di 8 cifre.</em></p>"),
        easyClose = TRUE,
        footer = modalButton("Chiudi"))))
    }
    
  })


observeEvent(
  input$unprot,
  {
    
    reset("codaz2")# cancella l'input codice
    reset("area")#cancella l'input slider
    
    removeUI("#allfoc") #cancella la tabella ...
    
    leafletProxy("mobuffmap", session, data = allcod()) %>%
      clearShapes() %>%
      clearMarkers() %>%
      setView(10.89347, 44.53753, zoom = 9) %>% 
      addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>% 
      addPolygons(data = commo, stroke = TRUE, fill =  FALSE, weight = 1.5, color = "gray")
    
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




#Codici per generare mappe ocn allevamenti campionati----


campmappa <- reactive({
  confSA %>% 
    filter(anno == input$anno4, finalita == input$finalitamappa) %>%
    mutate(codaz = toupper(codaz)) %>%  
    left_join(coordbv, by = "codaz")
  
})


observeEvent( #cliccando genera mappa produce la mappa con gli allevamenti controllati per il campionamento selezionato
  input$prot2,
  {
    
    removeUI("#allfoc")
    removeUI("#mappacamp") #cancella la tabella ...    
    
    leafletProxy("mobuffmap", session, data = campmappa()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.89347, 44.53753, zoom = 9) %>% 
      addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>% 
      addPolygons(data = commo, stroke = TRUE, fill =  FALSE, weight = 1.5, color = "gray") %>% 
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
                               "</strong>") %>%
                   lapply(htmltools::HTML))
    
    
    insertUI("#placeholder2", "afterEnd", ui = div(id = "mappacamp", campmappa() %>%
               mutate(dtprel_chr = format(dtprel, "%d/%m/%Y"),
                      dtconf_chr = format(dtconf, "%d/%m/%Y"),
                      nconf_chr = as.character(nconf),
                      ncamp_accettati_chr = as.character(ncamp_accettati),
                      specie = str_to_sentence(specie),
                      materiale = str_to_sentence(materiale)) %>% 
               dplyr::select(nconf,nconf_chr, codaz, comune, dtprel, dtprel_chr, dtconf, dtconf_chr,
                             specie, ncamp_accettati, ncamp_accettati_chr) %>% 
               datatable(style = 'bootstrap',
                         colnames = c("conferimento_0",#0
                                      "conferimento",#1
                                      "codice azienda",#2
                                      "comune", #3
                                      "data prelievo_0",#4
                                      "data prelievo",#5
                                      "data conferimento_0",#6
                                      "data conferimento",#7
                                      "specie",#8
                                      "ncamp_accettati_0",#9
                                      "campioni accettati"),#10
                         rownames = FALSE,
                         selection = 'none',
                         extensions = 'Buttons',
                         filter = c('top'),
                         callback = JS("$(\"input[type='search']\").attr('placeholder','🔍');"),   # &#61442 - &#xf002 - \u2315 # 🔍 
                         options = list(
                           order = list(list(6, 'desc'), list(4, 'desc')),
                           dom = 'Brltip', #irltp
                           scrollX = TRUE,
                           searching = TRUE,
                           autoWidth = TRUE,
                           pageLength = 5,
                           lengthMenu = list(c(5, 25, 50, 100, -1), 
                                             c('5', '25', '50', '100', 'Tutti')),
                           buttons = list(
                             list(extend = "excel", text = "Scarica Tutto",
                                  filename = paste0(input$finalitamappa, " - attività ufficiale ", input$anno4, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
                                  title = NULL,
                                  titleAttr = "Excel",
                                  exportOptions = list(
                                    modifier = list(page = "all"),
                                    columns = c(1,2,3,5,7,8,10))),
                             list(extend = "excel", text = "Scarica Selezione",
                                  filename = paste0(input$finalitamappa, " - attività ufficiale ", input$anno4, " (dati aggiornati al ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), ")"),
                                  title = NULL,
                                  titleAttr = "Excel",
                                  exportOptions = list(
                                    modifier = list(page = "current"),
                                    columns = c(1,2,3,5,7,8,10)))
                           ),
                           columnDefs = list(
                             list(orderData = 4, targets = 5),
                             list(orderData = 6, targets = 7),
                             list(orderData = 0, targets = 1),
                             list(visible = FALSE, targets = c(0, 4, 6, 9)),
                             list(className = 'dt-body-right', targets = c(10)),
                             list(className = 'dt-body-center', targets = c(1, 5, 7)),
                             list(className = 'dt-head-center', targets = "_all"),
                             list(width = '130px', targets =c(1, 2)),
                             list(width = '50px', targets =c(10)),
                             #list(width = '130px', targets =c(3)),
                             list(width = '130px', targets =c(5, 7))
                             # list(width = '60px', targets =c(6, 7))
                           ),
                           language = list(search = "Cerca: ",
                                           lengthMenu = "Mostra _MENU_ conferimenti",
                                           paginate = list(previous = "Precedente", `next` = "Successiva"),
                                           info = "_START_ - _END_ di _TOTAL_ conferimenti",
                                           infoFiltered = "(su un totale di _MAX_ conferimenti)",
                                           infoEmpty = "Nessun conferimento disponibile",
                                           zeroRecords = "---")        
                         ))
               )) # questo codice permette di far ricomparire
    # la tabella dopo aver cliccato su reset
    
  })

observeEvent(
  input$unprot2,
  {
    
    reset("anno4")# cancella l'input codice
    reset("finalitamappa")#cancella l'input slider
    
    removeUI("#mappacamp") #cancella la tabella ...
    
    leafletProxy("mobuffmap", session, data = campmappa()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.89347, 44.53753, zoom = 9) %>% 
      addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>% 
      addPolygons(data = commo, stroke = TRUE, fill =  FALSE, weight = 1.5, color = "gray") 
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


#codice per generare mappa iniziale in tab Mappe ( SANITA' ANIMALE)----

output$mobuffmap <- renderLeaflet({
  
  leaflet() %>% 
    addTiles() %>%
    setView(10.89347, 44.53753, zoom = 9) %>% 
    addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>% 
    addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
    addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black")
  
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
    leafletProxy("mobuffmap", session, data = entro()) %>%
      fitBounds(lng1 = max(entro()$long),lat1 = max(entro()$lat),
                lng2 = min(entro()$long),lat2 = min(entro()$lat)) %>%
      addCircles(~long,  ~lat, radius =100, weight = 1) %>%
      addCircles( ~focolaio()$long,  ~focolaio()$lat, radius =input$area, weight = 1)
    
    insertUI("#placeholder", "afterEnd", ui = DT::dataTableOutput('allfoc')) # questo codice permette di far ricomparire
    # la tabella dopo aver cliccato su reset

  })

observeEvent(
  input$unprot,
   {
    leafletProxy("mobuffmap", session,data = allcod()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.89347, 44.53753, zoom = 9) %>%
      addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black")
     
   
     reset("codaz2")# cancella l'input codice
     reset("area")#cancella l'input slider

     removeUI("#allfoc") #cancella la tabella ...
    
  

  

  })
 

output$allfoc <- DT::renderDataTable({ 
  
  entro()
   
})

#Codici per generare mappe ocn allevamenti campionati----

 
campmappa <- reactive(
    confSA %>% 
      filter(anno == input$anno2, finalita == input$finalitamappa) %>% 
      left_join(coordbv))


observeEvent( #cliccando genera mappa produce la mappa con gli allevamenti controllati per il campionamento selezionato
  input$prot2,
  {
    leafletProxy("mobuffmap", session, data = campmappa()) %>%
      addCircles(~long,  ~lat, radius =500, weight = 1.5) 
      insertUI("#placeholder2", "afterEnd", ui = DT::dataTableOutput('mappacamp')) # questo codice permette di far ricomparire
    # la tabella dopo aver cliccato su reset
    
  })

observeEvent(
  input$unprot2,
  {
    leafletProxy("mobuffmap", session,data = allcod()) %>%
      clearMarkers() %>% clearShapes() %>%
      setView(10.89347, 44.53753, zoom = 9) %>%
      addPolygons(data = ER, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = PRer, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = MO, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black")
    
    
    reset("anno2")# cancella l'input codice
    reset("finalitamappa")#cancella l'input slider
    
    removeUI("#mappacamp") #cancella la tabella ...
    
    
    
    
    
  })


output$mappacamp <- DT::renderDataTable({ 
  
  campmappa()
  
})


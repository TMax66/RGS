server<-function(input, output, session) { 
  
  
  ## data aggiornamento dei dati-----
  output$aggdati <- renderUI({
    paste0("Dati aggiornati al: ", format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"))
  })

  ## mappa ausl modena----
  output$bgmap <- renderLeaflet({
    
    leaflet() %>% 
    addTiles() %>%
      setView(9.6772698, 45.6982642, zoom = 9) %>% 
      addPolygons(data = LOM, fill = FALSE, weight = 1, color = "black") %>% 
      addPolygons(data = PRlom, fill = FALSE, weight = 1, color = "black") %>%
      addPolygons(data = BG, stroke = TRUE, fill =  FALSE, weight = 2.5, color = "black") %>%
      addPolygons(data = distretti, stroke = TRUE, weight = 1,
                  fillOpacity = 1,
                  fillColor = ~factpal(distretto),
                  label = ~labels) %>% 
      addPolygons(data = combg, stroke = TRUE, weight = 1, fill = FALSE, color = "white") %>%
    addLabelOnlyMarkers(data = distretti[-c(2),], ~lng, ~lat,
                        label = ~labels2[-2],
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
                        "padding" = "5px 10px 5px 10px",
                        "text-align" = "center"
                        ))) %>%
    addLabelOnlyMarkers(data = distretti[c(2),], ~lng, ~lat,
                        label = ~labels2[2],
                        labelOptions = labelOptions(
                        noHide = T,
                        direction = 'right',
                        textOnly = T,
                        style = list(
                        "font-family" = "Montserrat",
                        "color" = "black",
                        "font-size" = "9px",
                        "border" = "2px solid #000",
                        "background" = "white",
                        "padding" = "5px 10px 5px 10px",
                        "text-align" = "center"
                        )))
    #addCircleMarkers(lng=coord$lng, lat = coord$lat, radius = 0.5 , label = coord$codaz)
  
  })
  
  ## graficiHome----
  
  source("server_code/grafici.R", local = TRUE)
  
  ## tabelleHome----
  
  source("server_code/tabelleHome.R", local = TRUE)
  
  
  ## tabelle Sanità Animale----
  
  source("server_code/tabelleSA.R", local = TRUE)
  
  output$tsa <- renderUI({
    req(input$finalita)
    wellPanel(
    fluidRow(
      dataTableOutput("t1SA")
    )
    )
    
  })
  
  output$tsadrill <- renderUI({
    req(input$finalita)
    wellPanel(style = "padding-top:5px;padding-bottom:24px",
    fluidRow(
      column(12,
      dataTableOutput("asldrill")
    )))
  })
  
  
  output$finalita_text_1 <- renderText({ input$finalita })
  

### ricerca codice allevamento-----
  output$tcode <- renderUI({
    req(input$codiceall)
    wellPanel(
      fluidRow(
        dataTableOutput("t3SA")
      )
    )
    
  })
  
  output$tcodedrill <- renderUI({
    req(input$codiceall)
    wellPanel(
      fluidRow(
        dataTableOutput("cadrill")
      )
    )
    
  })
  
  
 # output$finalita_text_ricerca <- renderText({ "Inserisci il CODICE AZIENDA e seleziona il CONFERIMENTO per vedere i dettagli delle prove" })
   output$finalita_text_ricerca <- renderText({
    validate(need(nrow(summarycodaz()) > 0, ""))
    "Seleziona il CONFERIMENTO per vedere i dettagli delle prove"
   })
   
  
  ## Codice mappa buffer e campionamenti----
  source("server_code/mappa.R", local = TRUE)
  
  
  
  
  
  # dataTableOutput("t3SA"), br(), hr(),
  # dataTableOutput("cadrill")
  
  
  
  
  
  
  ## tabelle Sicurezza Alimentare----
  
  source("server_code/tabelleSicA.R", local = TRUE)
  
  
  output$tsica <- renderUI({
    req(input$finalita2)
    wellPanel(
      fluidRow(
        dataTableOutput("t1SicA")
      ))
  })
  
  output$tsalimdrill <- renderUI({
    req(input$finalita2)
    wellPanel(style = "padding-top:5px;padding-bottom:24px",
    fluidRow(
      column(12,
      dataTableOutput("asldrillalim")
    )))
  })
    

  output$finalita_text_2 <- renderText({ input$finalita2 })
  
 

#tabelle Alimenti Zootecnici----
  
  source("server_code/tabelleAZ.R", local = TRUE)
  
  
  output$tza <- renderUI({
    req(input$finalita3)
    wellPanel(
      fluidRow(
        dataTableOutput("t1AZ")
      ))
  })
  
  output$aslAZdrill <- renderUI({
    req(input$finalita3)
    wellPanel(style = "padding-top:5px;padding-bottom:24px",
    fluidRow(
      column(12,
      dataTableOutput("asldrillalimZot")
    )))
  })
 
  output$finalita_text_3 <- renderText({ input$finalita3 })
  
  
  
  
  
  
  
  
  
  
  
  
  ## tabelle alimenti zootecnici
  
 # source("server_code/tabelleAZ.R", local = TRUE)
  
}
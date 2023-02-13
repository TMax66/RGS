confHSA <- reactive({
  conf %>%
    rename(ncamp_accettati = NrCampioni) %>%
    distinct(Nconf, .keep_all = TRUE) %>%
    filter(anno == 2022,
           settore == "Sanità Animale")
  })


output$thomeSA <- renderDataTable({
  confHSA() %>%
    summarise('Conferimenti accettati' = n()) %>%
    
    bind_cols(
      confHSA() %>%
        summarise('Campioni conferiti' = sum(ncamp_accettati))
      ) %>%
    bind_cols(
      confHSA() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))
      ) %>%
    bind_cols(
      confHSA() %>%
        mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>%
        summarise('Tempo attesa mediano (gg)' = round(quantile(tempo_attesa_esito, probs = 0.5, na.rm = TRUE)))
      ) %>%
    DT::datatable(rownames = FALSE,
                  class = "compact",
                  style = 'bootstrap',
                  selection = 'none',
                  options = list(
                    scrollX = TRUE,
                    dom = 't',
                    ordering = F,
                    columnDefs = list(
                      list(className = 'dt-left', targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$('table').css({'font-family': 'Montserrat'});",
                      "}"
                    ))
                  )

  })

confHAU <-  reactive({
  conf %>%
    distinct(Nconf, .keep_all = TRUE) %>%
    rename(ncamp_accettati = NrCampioni) %>%
    filter(anno == 2022, settore == "Alimenti Uomo")
  })


output$thomeAU <- renderDataTable({
  confHAU() %>%
    summarise('Conferimenti accettati' = n()) %>%
    
    bind_cols(
      confHAU() %>%
        summarise('Campioni conferiti' = sum(ncamp_accettati))) %>%
    bind_cols(
      esami <- confHAU() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))) %>%
    bind_cols(
      confHAU() %>%
        mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>%
        summarise('Tempo attesa mediano (gg)' = round(median(tempo_attesa_esito, na.rm = TRUE), 1))) %>%
    DT::datatable(rownames = FALSE,
                  class = "compact",
                  style = 'bootstrap',
                  selection = 'none',
                  options = list(
                    scrollX = TRUE,
                    dom = 't',
                    ordering = F,
                    columnDefs = list(
                      list(className = 'dt-left', targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$('table').css({'font-family': 'Montserrat'});",
                      "}"
                    ))
    )
  
  })
 

confHAZ <- reactive({
  conf %>%
    rename(ncamp_accettati = NrCampioni) %>%
    distinct(Nconf, .keep_all = TRUE) %>%
    filter(anno == 2022, settore == "Alimenti Zootecnici")
  })


output$thomeAZ <- renderDataTable({
  confHAZ() %>%
    summarise( 'Conferimenti accettati' = n()) %>%
    
    bind_cols(
      confHAZ() %>%
        summarise('Campioni conferiti' = sum(ncamp_accettati))) %>%
    bind_cols(
      confHAZ() %>%
        left_join(nesami, by = "Nconf") %>%
        ungroup() %>%
        summarise('Esami eseguiti' = sum(n, na.rm = TRUE))) %>%
    bind_cols(
      confHAZ() %>%
        mutate(tempo_attesa_esito = as.numeric((dtprimordp-dtconf)/86400)) %>%
        summarise('Tempo attesa mediano (gg)' = round(median(tempo_attesa_esito, na.rm = TRUE), 1))) %>%
    DT::datatable(rownames = FALSE,
                  class = "compact",
                  style = 'bootstrap',
                  selection = 'none',
                  options = list(
                    scrollX = TRUE,
                    dom = 't',
                    ordering = F,
                    columnDefs = list(
                      list(className = 'dt-left', targets = "_all")),
                    initComplete = JS(
                      "function(settings, json) {",
                      "$('table').css({'font-family': 'Montserrat'});",
                      "}"
                    ))
    )
  
  })
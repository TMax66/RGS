# dtSplot <- reactive(
#   siero %>% filter(prova == input$prove)
# )


# Accettazione------------------------------------------

fgacc <- reactive({
  
  if(input$settore == "Tutti"){  
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == input$selanno) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>% 
     plot_ly(x = ~weekreg, y = ~n, type= 'bar', source = "A")
  } else { 
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == input$selanno, settore == input$settore) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>% 
      plot_ly(x = ~weekreg, y = ~n, type= 'bar', source = "A") 
  }
})

output$plotacc <- renderPlotly({
  fgacc()
})



## drilldown table from grafico accettazione----

selected_bar <- reactiveVal(NULL)


# get the date of the selected bar
observe({
  selected_date <- event_data(event = "plotly_click")$x
  selected_bar(selected_date)
})



confacc <- reactive({
  
  if(input$settore == "Tutti"){  
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == input$selanno)
      
  } else { 
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == input$selanno, settore == input$settore)
      
  }
})


output$table <- renderReactable({
  if(length(selected_bar())) {
    conf_table <- confacc() %>% 
      filter(weekreg == selected_bar()) %>% 
      select(nconf, settore, tipo_prelievo, finalita, dtprel, dtconf, dtreg, conferente, dest_rdp, NrCampioni)

    reactable(conf_table)
  }

})










# 




# Laboraotorio Sierologia------------------------------------------------------------------

output$Sp1 <- renderPlotly({
  
  #req(input$prove)

    siero %>% 
    filter(anno == input$selanno) %>% 
      #group_by(prova, tecnica, dtinizio) %>% 
    mutate(sett = week(dtinizio)) %>% 
      group_by(sett) %>% 
      count() %>% 
      ungroup() %>% 
      #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
      ggplot()+
      aes(x=sett, y=n)+
      geom_point(alpha = 0.8, color = "lightgrey")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
      #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
      #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
      theme_bw()+
      labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
})



 
output$Sp2 <- renderPlot({   
siero %>%
    filter(anno == input$selanno) %>% 
      mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400), 
           week = week(dtfine)) %>%
  group_by(prova, tecnica,week) %>% 
  summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>% 
  
  ggplot()+
  aes(x= week, y = tmesec)+
  geom_line()+ geom_point()+
  facet_wrap(prova ~ tecnica, scales = "free")+
  theme_bw()+
  labs(title = "andamento settimanale tempi medi di esecuzione", 
       y = "tempo medio di esecuzione prove", 
       x = "settimana")
})


# Laboratorio Diagnostica ------------------------------------------------------------------

output$Dp1 <- renderPlotly({

  #req(input$prove)

  diagnostica %>%
    filter(anno == input$selanno) %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
    ggplot()+
    aes(x=sett, y=n)+
    geom_point(alpha = 0.8, color = "lightgrey")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
    #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
    theme_bw()+
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
})




output$Dp2 <- renderPlot({
  req(input$diagnos)
  diagnostica %>%
    filter(anno == input$selanno) %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>%
    group_by(prova, week) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>%
    filter(prova == input$diagnos) %>% 
    ggplot()+
    aes(x= week, y = tmesec)+
    geom_line()+ geom_point()+
    #facet_wrap(prova ~ tecnica, scales = "free")+
    theme_bw()+
    labs(title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
})

# Laboratorio Microbiologia Alimenti ------------------------------------------------------------------

output$MAp1 <- renderPlotly({
  
  #req(input$prove)
  
  alimenti %>%
    filter(anno == input$selanno) %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
    ggplot()+
    aes(x=sett, y=n)+
    geom_point(alpha = 0.8, color = "lightgrey")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
    #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
    theme_bw()+
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
})




output$MAp2 <- renderPlot({
  req(input$microalim)
  alimenti %>%
    filter(anno == input$selanno) %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>%
    group_by(prova, week) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>%
    filter(prova == input$microalim) %>% 
    ggplot()+
    aes(x= week, y = tmesec)+
    geom_line()+ geom_point()+
    #facet_wrap(prova ~ tecnica, scales = "free")+
    theme_bw()+
    labs(title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
})

# Laboratorio Biologia Molecolare ------------------------------------------------------------------

output$bmp1 <- renderPlotly({
  
  #req(input$prove)
  
  biomol %>%
    filter(anno == input$selanno) %>%
    mutate(sett = week(dtinizio)) %>% 
    group_by(sett) %>% 
    count() %>% 
    ungroup() %>% 
    #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
    ggplot()+
    aes(x=sett, y=n)+
    geom_point(alpha = 0.8, color = "lightgrey")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
    #geom_line(aes(y = sett, x =dtinizio ), color = "blue")+  
    #facet_wrap(prova~tecnica, nrow = 1, scales = "free")+
    theme_bw()+
    labs(y = "numero di esami settimanali", x = "settimana (data inizio analisi)")
})




output$bmp2 <- renderPlot({
  req(input$biomolec)
  biomol %>%
    filter(anno == input$selanno) %>%
    mutate(tempo_esecuzione = as.numeric((dtfine-dtreg)/86400),
           week = week(dtfine)) %>% 
    filter(tempo_esecuzione >= 0) %>% 
    group_by(prova, week) %>%
    summarise(tmesec = round(mean(tempo_esecuzione, na.rm = TRUE),1)) %>%
    filter(prova == input$biomolec) %>% 
    ggplot()+
    aes(x= week, y = tmesec)+
    geom_line()+ geom_point()+
    #facet_wrap(prova ~ tecnica, scales = "free")+
    theme_bw()+
    labs(title = "andamento settimanale tempi medi di esecuzione",
         y = "tempo medio di esecuzione prove",
         x = "settimana")
})
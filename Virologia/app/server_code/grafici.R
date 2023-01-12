# dtSplot <- reactive(
#   siero %>% filter(prova == input$prove)
# )


# Accettazione------------------------------------------

output$plotacc <- renderPlotly({
  
  if(input$settore == "Tutti"){  
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == 2022) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>% 
      #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
      ggplot()+
      aes(x=weekreg, y=n)+
      geom_point(size = 1, alpha = 0.8, color = "royalblue")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
      # geom_line(aes(y = week, x =dtconf ), color = "blue")+ #geom_smooth(se = FALSE)+
      theme_bw()+
      labs(y = "numero conferimenti settimanali", x = "settimana")
  } else { 
    conf %>% 
      mutate(annoconf = year(dtconf), 
             annoprel = year(dtprel), 
             annoreg = year(dtreg),
             weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
      filter(annoconf == 2022, settore == input$settore) %>% 
      distinct(nconf, .keep_all = TRUE) %>% 
      group_by(weekreg) %>% 
      count() %>% 
      ungroup() %>% 
      #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
      ggplot()+
      aes(x=weekreg, y=n)+
      geom_point(size = 1, alpha = 0.8, color = "royalblue")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
      # geom_line(aes(y = week, x =dtconf ), color = "blue")+ #geom_smooth(se = FALSE)+
      theme_bw()+
      labs(y = "numero conferimenti settimanali", x = "settimana")
  }
})





# Laboraotorio Sierologia------------------------------------------------------------------

output$Sp1 <- renderPlotly({
  
  #req(input$prove)

    siero %>% 
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
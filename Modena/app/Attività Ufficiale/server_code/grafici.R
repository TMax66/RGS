output$p1 <- renderPlotly({
  
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
      rename(settimana = weekreg) %>% 
      #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
      ggplot()+
      aes(x=settimana, y=n)+
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
      rename(settimana = weekreg) %>%
      #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
      ggplot()+
      aes(x=settimana, y=n)+
      geom_point(size = 1, alpha = 0.8, color = "royalblue")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
     # geom_line(aes(y = week, x =dtconf ), color = "blue")+ #geom_smooth(se = FALSE)+
      theme_bw()+
      labs(y = "numero conferimenti settimanali", x = "settimana")
  }
})





# output$p2 <- renderPlot( 
#   conf %>% 
#     mutate(annoconf = year(dtconf), 
#            annoprel = year(dtprel), 
#            annoreg = year(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
#     filter(annoconf == 2022, settore == "Sanità Animale") %>% 
#     distinct(nconf, .keep_all = TRUE) %>% 
#     mutate(tempo_consegna = as.numeric((dtconf-dtprel)/86400)) %>%
#     ggplot()+
#     aes(x = tempo_consegna)+
#     geom_histogram(fill = "lightblue", color = "black")+
#     labs(y = "n.conferimenti", x = " tempo di invio dei campioni all'IZSLER" )+
#     theme_bw(), 
#   width = 200,
#   height = 120
# )


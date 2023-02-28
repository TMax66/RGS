output$p1 <- renderPlotly({
  
  if(input$settore == "Tutti"){  
    ggplotly(tooltip = c("text"), conf %>% 
               mutate(annoconf = year(dtconf), 
                      annoprel = year(dtprel), 
                      annoreg = year(dtreg),
                      weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
               filter(annoconf == input$selanno) %>% 
               distinct(nconf, .keep_all = TRUE) %>% 
               group_by(weekreg) %>% 
               count() %>% 
               ungroup() %>% 
               rename(settimana = weekreg) %>% 
               mutate(weekstart = as.Date(paste(input$selanno, settimana, 1, sep = "-"), "%Y-%U-%u")) %>% 
               mutate(intervallo = paste("dal", format(weekstart, "%d/%m/%Y"),"al", format(weekstart + 4, "%d/%m/%Y"))) %>%
               #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
               ggplot()+
               aes(x = settimana, y = n, text = paste(n, " conferimenti registrati", "<br>", intervallo, sep = ""))+
               geom_point(size = 1, alpha = 0.8, color = "royalblue")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
               # geom_line(aes(y = week, x =dtconf ), color = "blue")+ #geom_smooth(se = FALSE)+
               theme_bw()+
               labs(y = "numero conferimenti settimanali", x = "settimana")) %>%
      config(displayModeBar = FALSE)
    
    } else {
      if (input$settore == "Alimenti Zootecnici"){
      return()
      
    } else { 
    ggplotly(tooltip = c("text"), conf %>% 
               mutate(annoconf = year(dtconf), 
                      annoprel = year(dtprel), 
                      annoreg = year(dtreg),
                      weekreg = week(dtreg)) %>% # <-  queste righe vanno poi messe in ETL
               filter(annoconf == input$selanno, settore == input$settore) %>% 
               distinct(nconf, .keep_all = TRUE) %>% 
               group_by(weekreg) %>% 
               count() %>% 
               ungroup() %>% 
               rename(settimana = weekreg) %>%
               mutate(weekstart = as.Date(paste(input$selanno, settimana, 1, sep = "-"), "%Y-%U-%u")) %>% 
               mutate(intervallo = paste("dal", format(weekstart, "%d/%m/%Y"),"al", format(weekstart + 4, "%d/%m/%Y"))) %>%
               #mutate(sett = rollmean(n, k = 7, fill = NA) ) %>% 
               ggplot()+
               aes(x = settimana, y = n, text = paste(n, " conferimenti registrati", "<br>", intervallo, sep = ""))+
               geom_point(size = 1, alpha = 0.8, color = "royalblue")+ geom_line(group=1, alpha = 0.8, color = "lightgrey")+
               # geom_line(aes(y = week, x =dtconf ), color = "blue")+ #geom_smooth(se = FALSE)+
               theme_bw()+
               labs(y = "numero conferimenti settimanali", x = "settimana")) %>%
      config(displayModeBar = FALSE)
    }
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


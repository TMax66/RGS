server<-function(input, output, session) { 

output$aggconf <- renderUI({
    paste0("Dati aggiornati al:",
           format(as.Date(substr(max(conf$dtconf, na.rm = TRUE),
                                 start = 1, stop = 11)), "%d-%m-%Y"))
  })
  
output$conferimenti <- renderDataTable( 
  conf, #%>% filter(Anno == input$anno),
  server = TRUE, 
  rownames = FALSE,
  # extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',pageLength = 5) 
  #buttons = c('excel'))
)

output$aggprove <- renderUI({
  paste0("Dati aggiornati al:",
         format(as.Date(substr(max(conf$dtconf, na.rm = TRUE),
                               start = 1, stop = 11)), "%d-%m-%Y"))
})
output$esami <- renderDataTable( 
  prove, #%>% filter(Anno == input$anno),
  server = TRUE, 
  rownames = FALSE,
  # extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip', pageLength = 5) 
  #buttons = c('excel'))
)




#tabelle e grafici

source("server_code/tabelle.R", local = TRUE)
source("server_code/tabelle home.R", local = TRUE)
source("server_code/grafici.R", local = TRUE)

# show the back button to hide drill down table
output$back <- renderUI({
  if (length(selected_bar())) 
    actionButton("clear", "Hide Table", icon("chevron-left"))
})



 


# clear the selection
observeEvent(input$clear,{
  selected_bar(NULL)
})

 


#tabella pivot
##################PIVOT TABLE#################
output$pivot <- renderRpivotTable({
  rpivotTable(esami, aggregatorName="Integer Sum", vals="n.esami")
})


}
server<-function(input, output, session) { 

# output$aggconf <- renderUI({
#     paste0("Dati aggiornati al:",
#            format(as.Date(substr(max(conf$dtreg, na.rm = TRUE),
#                                  start = 1, stop = 11)), "%d-%m-%Y"))
#   })
#   
# output$conferimenti <- renderDataTable( 
#   conf, #%>% filter(Anno == input$anno),
#   server = TRUE, 
#   rownames = FALSE,
#   # extensions = 'Buttons',
#   options = list(
#     dom = 'Bfrtip',pageLength = 5) 
#   #buttons = c('excel'))
# )
# 
# output$aggprove <- renderUI({
#   paste0("Dati aggiornati al:",
#          format(as.Date(substr(max(conf$dtreg, na.rm = TRUE),
#                                start = 1, stop = 11)), "%d-%m-%Y"))
# })
# output$esami <- renderDataTable( 
#   prove, #%>% filter(Anno == input$anno),
#   server = TRUE, 
#   rownames = FALSE,
#   # extensions = 'Buttons',
#   options = list(
#     dom = 'Bfrtip', pageLength = 5) 
#   #buttons = c('excel'))
# )
# 
# 
# 
# 
# #tabelle e grafici
# 
# source("server_code/tabelle.R", local = TRUE)
# source("server_code/tabelle home.R", local = TRUE)
# source("server_code/grafici.R", local = TRUE)


#tabella pivot
##################PIVOT TABLE#################
output$pivot <- renderRpivotTable({
  rpivotTable(esami, aggregatorName="Integer Sum", vals="n.esami")
})


}
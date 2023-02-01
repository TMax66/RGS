server<-function(input, output, session) {
  
  # output$value1 <- renderText({
  #   if (input$visual == "FALSE") "Andamento settimanale"
  #   else "Andamento giornaliero"
  # }) 
  
output$aggconf <- renderUI({
  paste0("Dati aggiornati al: ",
         format(as.Date(substr(max(conf$dtconf, na.rm = TRUE),
                               start = 1, stop = 11)), "%d-%m-%Y")
         )
  })

output$conferimenti <- renderDataTable(server = TRUE,{
  conf %>%  #%>% filter(Anno == input$anno),
    DT::datatable(
      rownames = FALSE,
      #extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
    )
  })

output$aggprove <- renderUI({
  paste0("Dati aggiornati al: ",
         format(as.Date(substr(max(conf$dtconf, na.rm = TRUE),
                               start = 1, stop = 11)), "%d-%m-%Y")
         )
  })

output$esami <- renderDataTable(server = TRUE,{
  prove %>%  #%>% filter(Anno == input$anno),
    DT::datatable(
      rownames = FALSE,
      #extensions = 'Buttons',
      options = list(
        dom = 'Bfrtip',
        pageLength = 5)
      )
  })
  


#tabelle e grafici ----

source("server_code/tabelle.R", local = TRUE)
source("server_code/tabelle home.R", local = TRUE)
source("server_code/grafici.R", local = TRUE)


#ACCETTAZIONE----
# show the back button to hide drill down table
output$back <- renderUI({
  if (length(selected_bar()))
    actionButton("clear", "Nascondi Tabella", icon("chevron-left"))
  })


# clear the selection
observeEvent(input$clear,{
  selected_bar(NULL)
  })

observeEvent(input$settore,{
  selected_bar(NULL)
  selected_bar2(NULL)
})

observeEvent(input$visual,{
  selected_bar(NULL)
  selected_bar2(NULL)
})


# show the back button to hide drill down table
output$back2 <- renderUI({
  if (length(selected_bar2()))
    actionButton("clear2", "Nascondi Tabella", icon("chevron-left"))
})


# clear the selection
observeEvent(input$clear2,{
  selected_bar2(NULL)
})


#DIAGNOSTICA----

# output$back_diagn <- renderUI({
#   if (length(selected_bar_diagn()))
#     actionButton("clear_diagn", "Nascondi Tabella", icon("chevron-left"))
# })
# 
# 
# # clear the selection
# observeEvent(input$clear_diagn,{
#   selected_bar_diagn(NULL)
# })


observeEvent(input$visual_diagn,{
  selected_bar_diagn(NULL)
})


output$table_diagno <- renderUI({
  # if (length(selected_bar_diagn())){
    #wellPanel(
      dataTableOutput("table_diagn", width = "100%"#, height = "500px"
                      )
    # )
    # } else {
    # print("descrizione")
    # }
  })

output$drill_diagno <- renderUI({
  if (length(input$table_diagn_rows_selected) > 0){
    #wellPanel(
    div(style = "margin-bottom: 100px;",
      dataTableOutput("drill_diagn")
    )
    #  )
    }
  })


output$head_Dt1 <- renderUI({
  n <- diagnostica %>%
       #group_by(prova) %>%
       count()
  
  tags$div(
    tags$h4("Esami eseguiti per tipo di prova", style = "font-weight: 600;"), 
    tags$h5(paste("(",n," esami totali"," - dati aggiornati al ",
                  format(as.Date(substr(max(diagnostica$dtconf, na.rm = TRUE),
                                        start = 1, stop = 11)), "%d-%m-%Y"),
                  ")", sep=""))
    )
})





#PIVOT TABLE####
output$pivot <- renderRpivotTable({
  rpivotTable(esami, aggregatorName = "Integer Sum", vals = "n.esami")
  })


}
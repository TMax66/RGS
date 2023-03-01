
server<-function(input, output, session) {

  # output$value1 <- renderText({
  #   if (input$visual == "FALSE") "Andamento settimanale"
  #   else "Andamento giornaliero"
  # }) 
  
output$aggconf <- renderUI({
  paste0("ultimo aggiornamento: ",
         format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), #conf$dtconf
                               start = 1, stop = 11)), "%e %B %Y")
         )
  })




# output$conferimenti <- renderDataTable(server = TRUE,{
#   conf %>%  #%>% filter(Anno == input$anno),
#     DT::datatable(
#       rownames = FALSE,
#       #extensions = 'Buttons',
#       options = list(
#         dom = 'Bfrtip',
#         pageLength = 5)
#     )
#   })

# output$aggprove <- renderUI({
#   paste0("Dati aggiornati al: ",
#          format(as.Date(substr(max(conf$dtconf, na.rm = TRUE),
#                                start = 1, stop = 11)), "%d-%m-%Y")
#          )
#   })

# output$esami <- renderDataTable(server = TRUE,{
#   prove %>%  #%>% filter(Anno == input$anno),
#     DT::datatable(
#       rownames = FALSE,
#       #extensions = 'Buttons',
#       options = list(
#         dom = 'Bfrtip',
#         pageLength = 5)
#       )
#   })
  

#tabelle e grafici ----

#source("server_code/tabelle.R", local = TRUE)
source("server_code/home.R", local = TRUE)
source("server_code/accettazione.R", local = TRUE)
source("server_code/diagnostica.R", local = TRUE)
source("server_code/sierologia.R", local = TRUE)
source("server_code/microbiologia.R", local = TRUE)


# #ACCETTAZIONE
# # show the back button to hide drill down table
# output$back <- renderUI({
#   if (length(selected_bar()))
#     actionButton("clear", "Nascondi Tabella", icon("chevron-left"))
#   })
# 
# 
# # clear the selection
# observeEvent(input$clear,{
#   selected_bar(NULL)
#   })
# 
# observeEvent(input$settore,{
#   selected_bar(NULL)
#   selected_bar2(NULL)
# })
# 
# observeEvent(input$visual,{
#   selected_bar(NULL)
#   selected_bar2(NULL)
# })
# 
# 
# # show the back button to hide drill down table
# output$back2 <- renderUI({
#   if (length(selected_bar2()))
#     actionButton("clear2", "Nascondi Tabella", icon("chevron-left"))
# })
# 
# 
# # clear the selection
# observeEvent(input$clear2,{
#   selected_bar2(NULL)
# })
# 
# 
# #DIAGNOSTICA
# 
# # output$back_diagn <- renderUI({
# #   if (length(selected_bar_diagn()))
# #     actionButton("clear_diagn", "Nascondi Tabella", icon("chevron-left"))
# # })
# # 
# # 
# # # clear the selection
# # observeEvent(input$clear_diagn,{
# #   selected_bar_diagn(NULL)
# # })
# 
# 
# observeEvent(input$visual_diagn,{
#   selected_bar_diagn(NULL)
# })
# 
# 
# output$table_diagno <- renderUI({
#   # if (length(selected_bar_diagn())){
#     #wellPanel(
#       dataTableOutput("table_diagn", width = "100%"#, height = "500px"
#                       )
#     # )
#     # } else {
#     # print("descrizione")
#     # }
#   })
# 
# output$selconf <- renderUI({
#   sel_conf <- conf_diagn()[as.integer(input$table_diagn_rows_selected), ]$Nconf
#   selconf <- as.numeric(gsub("^.{0,4}", "", sel_conf))
#   paste0("CONFERIMENTO ", selconf," - DETTAGLIO PROVE ESEGUITE")
# })
# 
# 
# output$drill_diagno <- renderUI({
#   if (length(input$table_diagn_rows_selected) > 0){
#     #wellPanel(
#     div(style = "margin-bottom: 100px;",
#         uiOutput("selconf", style = "font-weight: 600;font-size: 18px;margin-top: 10px;margin-bottom: 10px;"),
#         br(),
#         dataTableOutput("drill_diagn")
#         )
#     }
#   })
# 
# 
# output$head_Dt1 <- renderUI({
#   n <- diagnostica %>%
#        #group_by(prova) %>%
#        count()
#   
#   tags$div(
#     tags$h4("Numero di esami e tempo medio di esecuzione", style = "font-weight: 600;"), 
#     tags$h5(paste("(",n," esami totali"," - dati aggiornati al ",
#                   format(as.Date(substr(max(diagnostica$dtconf, na.rm = TRUE),
#                                         start = 1, stop = 11)), "%d-%m-%Y"),
#                   ")", sep=""))
#     )
# })
# 
# observeEvent(input$bttn1,{
#   shinyjs::show("cp1")
#   shinyjs::hide("cp2")
#   runjs('
#       document.getElementById("table_diagno").scrollIntoView();
#     ')
# })
# observeEvent(input$bttn2,{
#   shinyjs::show("cp2")
#   shinyjs::hide("cp1")
#   runjs('
#       document.getElementById("esami_diagn").scrollIntoView();
#     ')
# })
# 
# output$downloadConf <- downloadHandler(
#   filename = function() {
#     paste('Laboratorio di Diagnostica - conferimenti al ',
#           format(as.Date(substr(max(diagnostica$dtreg[diagnostica$annoiniz==2022], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
#           '.xlsx',
#           sep='')
#   },
#   content = function(con) {
#     writexl::write_xlsx(
#       format_headers = FALSE,
#       diagnostica %>%
#         mutate(Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
#                settore = as.factor(settore),
#                tipo_prelievo = as.factor(tipo_prelievo),
#                dtprel = format(as.Date(dtprel), "%d-%m-%Y"),
#                dtconf = format(as.Date(dtconf), "%d-%m-%Y"),
#                dtreg = format(as.Date(dtreg), "%d-%m-%Y")) %>%
#         distinct(Nconf, .keep_all = TRUE) %>%
#         filter(annoiniz == 2022) %>%
#         select('Conferimento' = Nconf2,
#                'Settore' = settore,
#                'Tipo prelievo' = tipo_prelievo,
#                "Finalità" = finalita,
#                "Data prelievo" = dtprel,
#                "Data conferimento" = dtconf,
#                "Data registrazione" = dtreg,
#                "Conferente" = conferente,
#                "Destinatario RdP" = dest_rdp,
#                "Campioni conferiti" = NrCampioni),
#       con)
#   }
#   )
# 
# output$downloadEsam <- downloadHandler(
#   filename = function() {
#     paste('Laboratorio di Diagnostica - prove al ',
#           format(as.Date(substr(max(diagnostica$dtreg[diagnostica$annoiniz==2022], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"),
#           '.xlsx',
#           sep='')
#   },
#   content = function(con) {
#     writexl::write_xlsx(
#       format_headers = FALSE,
#       diagnostica %>%
#         mutate(Nconf2 = as.numeric(gsub("^.{0,4}", "", Nconf)),
#                dtinizio = format(as.Date(dtinizio), "%d-%m-%Y"),
#                dtfine = format(as.Date(dtfine), "%d-%m-%Y")) %>% 
#         filter(annoiniz == 2022) %>%
#         dplyr::select("Conferimento" = Nconf2,
#                       "Verbale" = verbale,
#                       "Codice Azienda" = codaz,
#                       "Numero campione" = numero_del_campione,
#                       "Data inizio" = dtinizio,
#                       "Data fine" = dtfine,
#                       "Specie" = specie,
#                       "Materiale" = materiale,
#                       "Prova" = prova,
#                       "Tecnica" = tecnica,
#                       "Esito" = valore),
#       con)
#   }
# )
# 
# 
# 
#PIVOT####
#https://rdrr.io/cran/rpivotTable/man/rpivotTable.html
output$pivot <- renderRpivotTable({
  rpivotTable(esamidt, aggregatorName = "Somma intera", vals = "numero di esami", locale = "it")
  #change_locale(pivottable = esami, locale = "it")
  })


}
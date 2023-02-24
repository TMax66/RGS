server<-function(input, output, session) {
  
# data aggiornamento-----
output$aggdati <- renderUI({
  tags$div(
    tags$h3(paste("Attività ufficiale anno", input$selanno), style = "font-weight: 600;"), 
    tags$h5(paste("Dati aggiornati al ",
                  format(as.Date(substr(
                    max(conf$dtreg[conf$annoreg == input$selanno], na.rm = TRUE), start = 1, stop = 11)), "%d-%m-%Y"), sep=""))
    )
  })

output$aggconf <- renderUI({
  paste0("ultimo aggiornamento: ",
         format(as.Date(substr(max(conf$dtreg, na.rm = TRUE), #conf$dtconf
                               start = 1, stop = 11)), "%e %B %Y")
  )
})


# HOME----
source("server_code/home.R", local = TRUE)

#SANITÀ ANIMALE----
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
  
  wellPanel(style = "padding-top:19px;padding-bottom:24px",
            fluidRow(
              column(12,
                     dataTableOutput("asldrill")
                     )
              )
            )
  })

output$finalita_text_1 <- renderText({ input$finalita })
  
## ricerca codice allevamento-----
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
  
##mappa buffer----
source("server_code/mappaSA.R", local = TRUE)

  
#SICUREZZA ALIMENTARE----
source("server_code/tabelleSicA.R", local = TRUE)
  
output$tsica <- renderUI({
  req(input$finalita2)
  
  wellPanel(
    fluidRow(
      dataTableOutput("t1SicA")
      )
    )
  })
  
output$tsalimdrill <- renderUI({
  req(input$finalita2)
  
  wellPanel(style = "padding-top:19px;padding-bottom:24px",
    fluidRow(
      column(12,
             dataTableOutput("asldrillalim")
             )
      )
    )
  })

output$finalita_text_2 <- renderText({ input$finalita2 })

#ALIMENTI ZOOTECNICI----
source("server_code/tabelleAZ.R", local = TRUE)

output$tza <- renderUI({
  req(input$finalita3)
  
  wellPanel(
    fluidRow(
      dataTableOutput("t1AZ")
      )
    )
  })

output$aslAZdrill <- renderUI({
  req(input$finalita3)
  
  wellPanel(style = "padding-top:19px;padding-bottom:24px",
            fluidRow(
              column(12,
                     dataTableOutput("asldrillalimZot")
                     )
              )
            )
  })
 
output$finalita_text_3 <- renderText({ input$finalita3 })

  
}
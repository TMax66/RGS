ui <- navbarPage(
  title = div(
    # img(src = "logo-ausl-header.png",
    #               id = "logo",
    #               height = "40px", width = "40px",
    #               style = "position: relative;margin:-15px 0px;right:8px;"),
              "Attività ufficiale ATS BERGAMO"),    
  theme = bslib::bs_theme(3),
  
    tags$head(
    # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
    # tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css", integrity="sha256-+N4/V/SbAFiW1MPBCXnfnP9QSN3+Keu+NlB+0ev/YKQ=", crossorigin="anonymous"),
    tags$style(
    HTML(
    '
    /* ICONA FILTRO DATATABLE */
    input.form-control {
    font-family: Montserrat, Segoe UI Symbol;
    font-weight: 600;
    }

    body {
    font-family: Montserrat;
    }
    
    table.dataTable tbody td {
    vertical-align: middle;
    }

    #t1 tbody > tr:last-child,
    #t2 tbody > tr:last-child,
    #t3 tbody > tr:last-child {
    font-weight: bold;
    }
    
    
    #finalita_text_1, #finalita_text_2, #finalita_text_3 {
    text-align: center;
    font-weight: 500;
    font-size: 200%;
    }
    
    #finalita_text_ricerca {
    font-weight: 500;
    }
    
    



/*HOME*/

    #t1 .table {
    width: 95% !important;
        margin-left: -10px;
    }
    
    #t2 .table {
    width: 95% !important;
        margin-left: -10px;
    }
    
    #t3 .table {
    width: 95% !important;
        margin-left: -10px;
    }
    
    

/*SANITA ANIMALE*/

    #asldrill .dataTables_length {
    float: right;
    }
       
    #asldrill .dataTables_info {
    float: left;
    margin-top: 30px;
    }  

    #asldrill th.sorting {
    vertical-align: middle;
    }
    
    #asldrill .dataTables_paginate {
    margin-top: 30px;
    }    

    #t1SA tbody > tr:last-child {
    font-weight: bold;
    }
    

    
/*RICERCA CODICE AZIENDA*/

    #t3SA .dataTables_length,
    #cadrill .dataTables_length {
    float: right;
    }
       
    #t3SA .dataTables_info,
    #cadrill .dataTables_info {
    float: left;
    margin-top: 30px;
    }      
    
    
    #t3SA th.sorting,
    #cadrill th.sorting {
    vertical-align: middle;
    }

/*MAPPA CAMPIONAMENTI*/

    #mappacamp,
    #allfoc {
    margin-top: 30px;
    }

    #mappacamp .dataTables_length,
    #allfoc .dataTables_length {
    float: right;
    }
       
    #mappacamp .dataTables_info,
    #allfoc .dataTables_info {
    float: left;
    margin-bottom: 0px;
    margin-top: 30px;
    }      
    
    #mappacamp .dataTables_paginate,
    #allfoc .dataTables_paginate {
    margin-top: 30px;
    }    
    
    #mappacamp th.sorting,
    #allfoc th.sorting {
    vertical-align: middle;
    }
    
    #codazModalDiv .modal-dialog {
    width: fit-content !important;
    }
    
    div.form-group.has-feedback{
    width: 100%;
    }

    
    
    
/*SICUREZZA ALIMENTARE*/

    #asldrillalim .dataTables_length {
    float: right;
    }
       
    #asldrillalim .dataTables_info {
    float: left;
    margin-top: 30px;
    }   
    
    #asldrillalim th.sorting {
    vertical-align: middle;
    }
    
    #asldrillalim .dataTables_paginate {
    margin-top: 30px;
    }    

    #t1SicA tbody > tr:last-child {
    font-weight: bold;
    }
    
    
/*ALIMENTI ZOOTECNICI*/

    #asldrillalimZot .dataTables_length {
    float: right;
    }
       
    #asldrillalimZot .dataTables_info {
    float: left;
    margin-top: 30px;
    }   

    #asldrillalimZot th.sorting {
    vertical-align: middle;
    }

    #asldrillalimZot .dataTables_paginate {
    margin-top: 30px;
    }    
    
    #t1AZ tbody > tr:last-child {
    font-weight: bold;
    }
    
    
    
    
    #finalita ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le finalità"],
    #finalita2 ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le finalità"],
    #finalita3 ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le finalità"]
    /*#finalita ~ .selectize-control.single .selectize-input [data-value="Tutte le finalità"], */
    /*#finalita2 ~ .selectize-control.single .selectize-input [data-value="Tutte le finalità"],*/
    /*#finalita3 ~ .selectize-control.single .selectize-input [data-value="Tutte le finalità"] */
    {font-weight: bold }
    
    

    ')
    )
    ),
  
# HOME PAGE ----
  tabPanel(
    title = "Home", 
    value = "home",
    
    fluidPage(
      fluidRow(style = "margin-left: 150px; margin-right: 150px;",
               column(12,
                      wellPanel(
                        leafletOutput("bgmap", height = 500))
                      )
               ),
      fluidRow(style = "margin-left: 150px; margin-right: 150px;",
               column(12,
                      wellPanel(
                        fluidRow(
                          column(12,
                                 radioButtons(
                                   inputId = "settore",
                                   label = "Settore d'intervento",
                                   choices = c("Tutti",
                                               "Sanità Animale","Alimenti Uomo", "Alimenti Zootecnici"),
                                   inline = TRUE)
                                 ),
                          column(12,
                                 plotlyOutput("p1")
                                 )
                          )
                        )
                      )
               ),
      
      fluidRow(style = "margin-left: 0px; margin-right: 0px;",
               column(12, align = "center",
                      h3("Attività ufficiale anno 2022"),
                      h5(uiOutput("aggdati")),
                      
                      br(),
                      wellPanel(
                        h3("Sanità Animale"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                          tableOutput("t1")
                          ),
                          column(2, style = "float: right;
                                    max-width: 200px;
                                    min-width: 200px;
                                    width: 100%;
                                    height: 300px;
                                    margin:auto;
                                    max-height: 200px;
                                    min-height: 200px;
                                    ",
                                 img(
                                   style = "border: 5px solid #555;",
                                   height = '200px',
                                   width = '200px',
                                   src = "bovini2.png",
                                   align = "right"
                                   )
                                 )
                          )
                        ),
# https://stackoverflow.com/questions/7273338/how-to-vertically-align-an-image-inside-a-div
                      wellPanel(
                        h3("Controllo Alimenti"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                                 tableOutput("t2")
                                 ),
                          column(2, style = "float: right;
                                    max-width: 200px;
                                    min-width: 200px;
                                    width: 100%;
                                    height: 300px;
                                    margin:auto;
                                    max-height: 200px;
                                    min-height: 200px;
                                    ",
                                 img(
                                   style = "border: 5px solid #555;",
                                   height = '200px',
                                   width = '200px',
                                   src = "Mta2.png",
                                   align = "right")
                                 )
                          )
                        ),
                      wellPanel(
                        h3("Alimenti Zootecnici"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                                 tableOutput("t3")
                                 ),
                          column(2, style = "float: right;
                                    max-width: 200px;
                                    min-width: 200px;
                                    width: 100%;
                                    height: 300px;
                                    margin:auto;
                                    max-height: 200px;
                                    min-height: 200px;
                                    ",
                                 img(style = "border: 5px solid #555;",
                                     height = '200px',
                                     width = '200px',
                                     src = "mangimi2.png",
                                     align = "right")
                                 )
                                 )
                        )
)))), 

#______________________________________________________________________________________________






# SANITA' ANIMALE----
  tabPanel(
    title = "Sanità Animale",
    value = "SAnim",
    tabsetPanel(
    # tabsetcampionamenti-----
      tabPanel("Campionamenti",
               fluidPage(
                 br(),
                 fluidRow(
                   column(4, style = "padding-left:0px;",
                          wellPanel(
                            radioButtons(
                              inputId = "anno",
                              label = "Seleziona l'anno" ,
                              choices =c("2021", "2022"),
                              selected = "2022",
                              inline = TRUE,
                              width = NULL,
                              choiceNames = NULL,
                              choiceValues = NULL
                              ),
                            selectInput(
                              inputId = "finalita",
                              label = "Seleziona il tipo di campionamento",
                              choices = c("Tutte le finalità", levels(factor(confSA$finalita)))
                              )
                            )
                          ),
                   column(8, style = "padding-right:0px;",
                          uiOutput("tsa")
                          )
                   ), #chiude la fluidRow
                 hr(),
                 textOutput("finalita_text_1"),
                 hr(),
                 fluidPage(style = "padding-left:0px; padding-right:0px;",
                           fluidRow(
                             uiOutput("tsadrill")
                             )
                           )
                 )
               ),# ,chiude il tabPanel campionamenti
    # tabset ricerca codiceaziendale----
    tabPanel("Ricerca Codice Aziendale",
             fluidPage(
               br(),
               fluidRow(
                 column(4, style = "padding-left:0px;",
                        wellPanel(
                 h4("Ricerca informazioni per codice aziendale",
                 style = "margin-top: 0px;margin-bottom: 40px; font-weight: 700"),
                 textInput("codiceall",
                           "Inserisci il codice azienda",
                           "")))),
               #https://shiny.rstudio.com/reference/shiny/latest/submitbutton
               
               #hr(),
               fluidRow(style = "padding-left:15px; padding-right:0px;",
                        textOutput("finalita_text_ricerca")),
               br(),
               #hr(),
               fluidPage(style = "padding-left:0px; padding-right:0px;",
                         fluidRow(
                           uiOutput("tcode"),
                           uiOutput("tcodedrill")
                         )
               )
             )
    ), # chiude il tabPanel ricerca codiceaziendale
    
    # Mappe----
    
    tabPanel("Mappe",
             fluidPage(
               useShinyjs(),
               br(),
               fluidRow(style = "margin-bottom: 20px;",
                 column(6, style = "padding-left:0px;",
                        wellPanel(style = "margin-bottom: 0px;
    padding-top: 25px;
    padding-bottom: 25px;",
                          leafletOutput("bgbuffmap", height = 500)
                        )
                        ), 
                 
                 column(6, style = "padding-left:0px;",
                        wellPanel(style = "margin-bottom: 8px;
    padding-top: 25px;
    padding-bottom: 15px;",
                          h4("Mappa gli allevamenti d'interesse",
                             style = "margin-top: 0px;margin-bottom: 20px; font-weight: 700"),
                          textInput("codaz2", "Inserisci Codice Azienda"), #https://stackoverflow.com/questions/63107357/how-to-force-shiny-input-to-be-capitalized 
                          sliderInput("area", "Definisci l'area buffer", min = 1000, max = 10000, value = 3000,
                                      step = 1000, post = " mt", sep = "."),
                          actionButton("prot", "Genera mappa"),
                          actionButton("unprot", "Reset mappa")
                        ), 
                        wellPanel(style = "margin-bottom: 0px;
    padding-top: 25px;
    padding-bottom: 15px;",
                          h4("Campionamenti",
                             style = "margin-top: 0px;margin-bottom: 20px; font-weight: 700"),
                          radioButtons(
                            inputId = "anno4",
                            label = "Seleziona l'anno" ,
                            choices =c("2021", "2022"),
                            selected = "2022",
                            inline = TRUE,
                            width = NULL,
                            choiceNames = NULL,
                            choiceValues = NULL
                          ),
                          selectInput(
                            inputId = "finalitamappa", 
                            label = "Seleziona il tipo di campionamento", 
                            selected = NULL,
                            choices = c("", levels(factor(confSA$finalita)))
                          ), 
                          
                          actionButton("prot2", "Genera mappa"),
                          actionButton("unprot2", "Reset mappa")
                          
                        )
                 )), 
               
               
              fluidPage(style = "padding-left: 0px;
    padding-right: 0px;",
                fluidRow(
                 column(12, style = "padding-left:0px;",
                        wellPanel(
                          h4("Elenco Aziende selezionate",style="text-align: center; font-weight: 700"),
                          # dataTableOutput("allfoc")
                          tags$div(id = "placeholder"),
                          tags$div(id = "placeholder2")
                        ))
               )
             ))
    )# chiude il tabPanel mappe
    )# chiude il tabsetPanel sanità animale
    ),#chiude il tabPanel sanità animale
    
# 
# 
# # SICUREZZA ALIMENTARE----
  tabPanel(
    title = "Sicurezza Alimentare",
    value = "uff",
    fluidPage(
      fluidRow(
        column(4, style = "padding-left:0px;",
               wellPanel(
                 radioButtons(
                   inputId = "anno2",
                   label = "Seleziona l'anno" ,
                   choices =c("2021", "2022"),
                   selected = "2022",
                   inline = TRUE,
                   width = NULL,
                   choiceNames = NULL,
                   choiceValues = NULL
                   ),
                 selectInput(
                   inputId = "finalita2",
                   label = "Seleziona il tipo di campionamento",
                   choices = c("Tutte le finalità", levels(factor(confSicA$finalita)))
                   )
                 )
               ),
        column(8, style = "padding-right:0px;",
               uiOutput("tsica")
               )
        ), # chiude la fluidRow
      hr(),
      textOutput("finalita_text_2"),
      hr(),
      fluidPage(style = "padding-left:0px;padding-right:0px;",
                fluidRow(
                  uiOutput("tsalimdrill")
                  )
                )
      )
    ),
# 
# 
# # ALIMENTI ZOOTECNICI----
  tabPanel(
    title = "Alimenti Zootecnici",
    value = "uff",
    fluidPage(
    fluidRow(
      column(4, style = "padding-left:0px;",
             wellPanel(
               radioButtons(
                 inputId = "anno3",
                 label = "Seleziona l'anno" ,
                 choices =c("2021", "2022"),
                 selected = "2022",
                 inline = TRUE,
                 width = NULL,
                 choiceNames = NULL,
                 choiceValues = NULL
                 ),
               selectInput(
                 inputId = "finalita3",
                 label = "Seleziona il tipo di campionamento",
                 choices = c("Tutte le finalità", levels(factor(confAZ$finalita)))
                 )
               )
             ),
      column(8, style = "padding-right:0px;",
             uiOutput("tza")
             )
      ), # chiude la fluidRow
    hr(),
    textOutput("finalita_text_3"),
    hr(),
    fluidPage(style = "padding-left:0px;padding-right:0px;",
              fluidRow(
                uiOutput("aslAZdrill")
                )
              )
    )
    )
)


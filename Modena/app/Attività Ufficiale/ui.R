ui <- tagList(navbarPageWithInputs(
  "Attività ufficiale AUSL MODENA",
  # theme = bslib::bs_theme(3),
  inputs = radioGroupButtons(
    inputId = "selanno",
    label = NULL,
    status = "primary",
    choices = c("2022", "2023"),
    individual = TRUE,
    justified = FALSE,
    selected = 2023,
    checkIcon = list(
      yes = icon("check-square", 
                 lib = "font-awesome"),
      
      no = icon("square", 
                lib = "font-awesome"))
  ),
#SPINNER----  
# Javascript Code
  singleton(tags$head(HTML("
  <script type='text/javascript'>
  
  /* When recalculating starts, show loading screen */
  $(document).on('shiny:recalculating', function(event) {
  $('div#divLoading').addClass('show');
  });

  /* When new value or error comes in, hide loading screen */
  $(document).on('shiny:value shiny:error', function(event) {
  $('div#divLoading').removeClass('show');
  });

  </script>"))),

# CSS Code
  singleton(tags$head(HTML(paste0("
  <style type='text/css'>
  #divLoading {
  display: none;
  }
  
  #divLoading.show {
  display: block;
  position: fixed;
  z-index: 100;
  background-image: url(Loading_Spinner.svg);
  background-size: auto 20%;
  background-repeat: no-repeat;
  background-position: center;
  left: 0;
  bottom: 0;
  right: 0;
  top: 0;
  }
  
  #loadinggif.show {
  left: 50%;
  top: 50%;
  position: absolute;
  z-index: 101;
  -webkit-transform: translateY(-50%);
  transform: translateY(-50%);
  width: 100%;
  margin-left: -16px;
  margin-top: -16px;
  }
  
  div.content {
  width : 1000px;
  height : 1000px;
  }
  
  </style>")))),

#CSS----
  tags$head(
    # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
    # tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css", integrity="sha256-+N4/V/SbAFiW1MPBCXnfnP9QSN3+Keu+NlB+0ev/YKQ=", crossorigin="anonymous"),
    tags$style(
      HTML(
    '
    div.dt-buttons {
    margin-left: 8px;
    }
    
    /*BOTTONE DOWNLOAD JDS PERSONALIZZATO DATATABLE*/
    .dt-button {
    float: left;
    }
    
    a.dt-button {
    text-decoration: none !important;
    }
    /*------------------------------------------*/
    
    /*dt datatable style = bootstrap*/
    div.dataTables_wrapper div.dataTables_paginate li.paginate_button {
    padding: 1px 1px 1px 1px;
    }
    /*------------------------------*/
    
    #settore-label {
    width: 200px;
    float: left;
    }
      
    #selanno .btn-primary {
    background-color: #1f77b4
    }
    
    .radio-btn-icon-yes{
    float:right;
    padding-left:5px;
    }
      
    .radio-btn-icon-no{
    float:right;
    padding-left:5px;
    }
    
    body, table {
    font-family: Montserrat;
    }
        
    .navbar-form {
    position:absolute;
    right:0;
    }
    
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
    padding-top: 5px;
    font-weight: 500;
    font-size: 150%;
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

    /*ampiezza prima colonna tabelle home (stesso css di sotto)*/
    /*#t1 td:first-child,*/
    /*#t2 td:first-child,*/
    /*#t3 td:first-child {*/
    /*width: 300px;*/
    /*}*/
    
    #t1 td:nth-child(1),
    #t2 td:nth-child(1),
    #t3 td:nth-child(1) {
    width: 300px;
    }
    
    
/*SANITA ANIMALE*/

    #asldrill .dataTables_length {
    float: right;
    }
       
    #asldrill .dataTables_info {
    float: left;
    padding-top: 2px;
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
    padding-top: 2px;
    margin-top: 30px;
    }      
    
    
    #t3SA th.sorting,
    #cadrill th.sorting {
    vertical-align: middle;
    }
    
    #t3SA .dataTables_paginate,
    #cadrill .dataTables_paginate {
    margin-top: 30px;
    }   

/*MAPPA CAMPIONAMENTI*/

    #mappacamp,
    #allfoc {
    margin-top: 30px;
    }
    
    
    /*PER CENTRARE DATATABLE ALLFOC MODENA*/
    #allfoc,
    #mappacamp {
    text-align: -webkit-center;
    }
    
    #allfoc .btn-group,
    #mappacamp .btn-group {
    display: unset;
    }
    /*------------------------------------*/


    #mappacamp .dataTables_length,
    #allfoc .dataTables_length {
    float: right;
    }
       
    #mappacamp .dataTables_info,
    #allfoc .dataTables_info {
    float: left;
    padding-top: 2px;
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
    
    #allfoc div.dataTables_wrapper,
    #mappacamp div.dataTables_wrapper {
    width: fit-content;
    }
    
    
    
/*SICUREZZA ALIMENTARE*/
    #asldrillalim .dataTables_length {
    float: right;
    }
       
    #asldrillalim .dataTables_info {
    float: left;
    padding-top: 2px;
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
    padding-top: 2px;
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
    
    
    
    
      /*CSS PER FOOTER*/
      html {
      position: relative;
      min-height: 100%;
      }
      
      body {
      margin-bottom: 30px; /* Margin bottom by footer height */
      }
      
      .footer {
      position: absolute;
      bottom: 0;
      width: 100%;
      height: 30px; /* Set the fixed height of the footer here */
      background-color: #f5f5f5;
      padding-top: 5px;
      padding-left: 15px;
      padding-right: 15px;
      }
      
      /*#aggconf {*/
      /*float: right;*/
      /*}*/
      /*-------------------*/


    
    ')
    )
  ),
  
  # HOME PAGE ----
  tabPanel(
    title = "Home", 
    value = "home",
    
    fluidPage(tags$body(HTML("<div id='divLoading'> </div>")),
              style = "padding-right: 0px; padding-left: 0px;",
              grillade(gutter = "xl", n_col = 12,
                       knack(cols = 4,
                             
    # fluidRow(style = "margin-left: 150px; margin-right: 150px;",
    #          column(12,
    
                             wellPanel(style = "margin-bottom:0px; height:100%; padding-top:25px; padding-bottom:25px;",
                                       leafletOutput("mappa_home", height = "600px")))
    # )
    # )
    ,
    # fluidRow(style = "margin-left: 150px; margin-right: 150px;",
    #          column(12,
    
                       knack(cols = 8,
                       wellPanel(style = "margin-bottom:0px; height:100%; padding-top:25px; padding-bottom:25px;",
    # fluidRow(
    #          column(12,
                                 radioButtons(
                                   inputId = "settore",
                                   label = "Settore d'intervento",
                                   choices = c("Tutti",
                                               "Sanità Animale","Alimenti Uomo", "Alimenti Zootecnici"),
                                   inline = TRUE),
                                 br(),
    #          column(12,
                                plotlyOutput("p1", height = '550px')
                                )
                            )
    #        )
    # )
                      ),
      
      fluidRow(#style = "padding-left: 0px; padding-right: 0px; margin-right: 0px; margin-left: 0px;",
               column(12, align = "center",
                      br(),
                      uiOutput("aggdati"),
                      br(),
                      wellPanel(
                        style = "padding-right: 50px; padding-left: 50px;",
                        h3("Sanità Animale"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                                div(dataTableOutput("t1"), style = "width:95%")
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
                      br(),
                      # https://stackoverflow.com/questions/7273338/how-to-vertically-align-an-image-inside-a-div
                      wellPanel(
                        style = "padding-right: 50px; padding-left: 50px;",
                        h3("Controllo Alimenti"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                                 div(dataTableOutput("t2"), style = "width:95%")
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
                      br(),
                      wellPanel(
                        style = "padding-right: 50px; padding-left: 50px;",
                        h3("Alimenti Zootecnici"),
                        br(),
                        br(),
                        fluidRow(
                          style = "display:flex",
                          column(10, style = "float:left; heigth:300px;",
                                 div(dataTableOutput("t3"), style = "width:95%")
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
    tabsetPanel(type = c("pills"),
      ## tabsetcampionamenti-----
      tabPanel("Campionamenti",
               fluidPage(
                 br(),
                 fluidRow(
                   column(4, style = "padding-left:0px;",
                          wellPanel(
                            HTML("Seleziona il <b>TIPO DI CAMPIONAMENTO</b> e il <b>DISTRETTO</b> per vedere il dettaglio delle attività")),
                          wellPanel(
                            # radioButtons(
                            #   inputId = "anno",
                            #   label = "Seleziona l'anno" ,
                            #   choices =c("2021", "2022"),
                            #   selected = "2022",
                            #   inline = TRUE,
                            #   width = NULL,
                            #   choiceNames = NULL,
                            #   choiceValues = NULL
                            # ),
                            selectInput(
                              inputId = "finalita", 
                              label = "Seleziona il tipo di campionamento", 
                              choices = c("Tutte le finalità", levels(factor(confSA$finalita)))
                            )
                          )
                   ),
                   column(8, style = "padding-right:0px;",
                          wellPanel(style ="height:80px;",
                                    textOutput("finalita_text_1")),
                          uiOutput("tsa")
                   )
                 ), #chiude la fluidRow
                 # hr(),
                 # textOutput("finalita_text_1"),
                 # hr(),
                 conditionalPanel(
                   condition = "output.conditionSA",
                 fluidPage(style = "padding-left:0px; padding-right:0px;",
                           fluidRow(
                             downloadButton("tsadrill_download", "", style = "visibility: hidden;"),
                             uiOutput("tsadrill")
                           )
                 )
                 )
               )
      ), # chiude il tabPanel campionamenti
      ## tabset ricerca codiceaziendale----
      tabPanel("Ricerca Codice Aziendale",
               fluidPage(
                 br(),
                 fluidRow(
                   column(4, style = "padding-left:0px;",
                          wellPanel(
                            h4("Ricerca informazioni per codice aziendale",
                               style = "margin-top: 0px;margin-bottom: 20px; font-weight: 600"),
                            textInput("codiceall",
                                      "Inserisci il codice azienda",
                                      "")))),
                 fluidRow(style = "padding-left:15px; padding-right:0px;",
                          textOutput("finalita_text_ricerca")),
                 br(),
                 #hr(),
                 fluidPage(style = "padding-left:0px; padding-right:0px;",
                           fluidRow(
                             uiOutput("tcode"),
                             conditionalPanel(
                               condition = "output.conditionRicSA",
                             uiOutput("tcodedrill")
                           ))
      )
      )
      ), # chiude il tabPanel ricerca codiceaziendale
      ## Mappe ----
      tabPanel("Mappe",
               fluidPage(style = "padding-left:0px; padding-right:0px",
                 useShinyjs(),
                 br(),
                 grillade(gutter = "xl", n_col=2,
                          knack(
                            cols = 1,
                            rows = 2,
                          
                                 wellPanel(style = "margin-bottom: 0px;height:100%;
    padding-top: 25px;
    padding-bottom: 25px;",
                                           leafletOutput("buffmap", height = 575)
                                 
                          )), 
                          
                                 wellPanel(style = "margin-bottom: 0px;height:100%;
    padding-top: 25px;
    padding-bottom: 0px;",
                                           h4("Mappa gli allevamenti d'interesse",
                                              style = "margin-top: 0px;margin-bottom: 20px; font-weight: 600;text-align: center;"),
                                           br(),
                                           textInput("codaz2", "Inserisci Codice Azienda"), #https://stackoverflow.com/questions/63107357/how-to-force-shiny-input-to-be-capitalized 
                                           sliderInput("area", "Definisci l'area buffer", min = 1000, max = 10000, value = 3000,
                                                       step = 1000, post = " mt", sep = "."),
                                           br(),
                                           actionButton("prot", "Genera mappa"),
                                           actionButton("unprot", "Reset mappa")
                                 ), 
                                 wellPanel(style = "margin-bottom: 0px;height:100%;
    padding-top: 25px;
    padding-bottom: 0px;",
                                           h4("Mappa gli allevamenti campionati",
                                              style = "margin-top: 0px;margin-bottom: 20px; font-weight: 600;text-align: center;"),
                                           # radioButtons(
                                           #   inputId = "anno4",
                                           #   label = "Seleziona l'anno" ,
                                           #   choices =c("2021", "2022"),
                                           #   selected = "2022",
                                           #   inline = TRUE,
                                           #   width = NULL,
                                           #   choiceNames = NULL,
                                           #   choiceValues = NULL
                                           # ),
                                           br(),
                                           selectInput(
                                             inputId = "finalitamappa", 
                                             label = "Seleziona il tipo di campionamento", 
                                             selected = NULL,
                                             choices = c("", levels(factor(confSA$finalita)))
                                           ), 
                                           br(),
                                           actionButton("prot2", "Genera mappa"),
                                           actionButton("unprot2", "Reset mappa")
                                           
                                 )
                          ), 
                 
                 
                 fluidPage(style = "padding-left: 0px;
    padding-right: 0px;",
                           fluidRow(
                             column(12, 
                                    #wellPanel(
                                      #h4("Elenco Aziende selezionate",style="text-align: center; font-weight: 600"),
                                      # dataTableOutput("allfoc")
                                      tags$div(id = "placeholder"),
                                      tags$div(id = "placeholder2")
                                    )#)
                           )
                 ))
      )# chiude il tabPanel mappe
    )# chiude il tabsetPanel sanità animale
  ),#chiude il tabPanel sanità animale
  
  
  # SICUREZZA ALIMENTARE----
  tabPanel(
    title = "Sicurezza Alimentare",
    value = "uff",
    fluidPage(
      fluidRow(
        column(4, style = "padding-left:0px;",
               wellPanel(
                 HTML("Seleziona il <b>TIPO DI CAMPIONAMENTO</b> e il <b>DISTRETTO</b> per vedere il dettaglio delle attività")),
               wellPanel(
                 # radioButtons(
                 #   inputId = "anno2",
                 #   label = "Seleziona l'anno" ,
                 #   choices =c("2021", "2022"),
                 #   selected = "2022",
                 #   inline = TRUE,
                 #   width = NULL,
                 #   choiceNames = NULL,
                 #   choiceValues = NULL
                 # ),
                 selectInput(
                   inputId = "finalita2", 
                   label = "Seleziona il tipo di campionamento", 
                   choices = c("Tutte le finalità", levels(factor(confSicA$finalita)))
                 ),
                 # fluidRow(style = "margin-left: 0px;",
                 #          column(12, style = "width: auto;padding-left: 0px;padding-right: 0px; display: flex; align-items:center",
                 #                 tags$h4("Download", style = "font-weight: 700;font-size: 14px;padding-right:15px;"),
                 #                 downloadBttn(
                 #                   label = NULL,
                 #                   outputId = "tsalimdrill_download",
                 #                   style = "material-circle",
                 #                   color = "primary"
                 #                 )))
               )
        ),
        column(8, style = "padding-right:0px;",
               wellPanel(style ="height:80px;",
                         textOutput("finalita_text_2")),
               uiOutput("tsica")
        )
      ), # chiude la fluidRow
      # hr(),
      # textOutput("finalita_text_2"),
      # hr(),
      conditionalPanel(
        condition = "output.conditionSicA",
      fluidPage(style = "padding-left:0px;padding-right:0px;",
                fluidRow(
                  downloadButton("tsalimdrill_download", "", style = "visibility: hidden;"),
                  uiOutput("tsalimdrill")
                ))
      )
    )
  ), 
  
  
  # ALIMENTI ZOOTECNICI----
  tabPanel(
    title = "Alimenti Zootecnici",
    value = "uff",
    fluidPage( 
      fluidRow(
        column(4, style = "padding-left:0px;",
               wellPanel(
                 HTML("Seleziona il <b>TIPO DI CAMPIONAMENTO</b> e il <b>DISTRETTO</b> per vedere il dettaglio delle attività")),
               wellPanel(
                 # radioButtons(
                 #   inputId = "anno3",
                 #   label = "Seleziona l'anno" ,
                 #   choices =c("2021", "2022"),
                 #   selected = "2022",
                 #   inline = TRUE,
                 #   width = NULL,
                 #   choiceNames = NULL,
                 #   choiceValues = NULL
                 # ),
                 selectInput(
                   inputId = "finalita3", 
                   label = "Seleziona il tipo di campionamento", 
                   choices = c("Tutte le finalità", levels(factor(confAZ$finalita)))
                 )
               )
        ),
        column(8, style = "padding-right:0px;",
               wellPanel(style ="height:80px;",
                         textOutput("finalita_text_3")),
               uiOutput("tza")
        )
      ), # chiude la fluidRow
      # hr(),
      # textOutput("finalita_text_3"),
      # hr(),
      conditionalPanel(
        condition = "output.conditionAZ",
        fluidPage(style = "padding-left:0px;padding-right:0px;",
                fluidRow(
                  downloadButton("aslAZdrill_download", "", style = "visibility: hidden;"),
                  uiOutput("aslAZdrill")
                )
        )
      )
    )
  )
),
br(),
#FOOTER----
#https://stackoverflow.com/questions/67763901/footer-position-in-shiny
tags$footer(uiOutput("aggconf"), class = "footer")
)
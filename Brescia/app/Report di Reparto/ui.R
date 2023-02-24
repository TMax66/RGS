ui <- tagList(navbarPageWithInputs(
  "Report Gestionale Sanitario di Reparto - Sede Territoriale di Brescia",
  #position = c("fixed-top"),
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
  #theme = bslib::bs_theme(3),

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
      /*dt datatable style = bootstrap*/
      div.dataTables_wrapper div.dataTables_paginate li.paginate_button {
      padding: 1px 1px 1px 1px;
      }
      
      #selanno .btn-primary {
      background-color: #1f77b4;
      }
      
      .radio-btn-icon-yes{
      float:right;
      padding-left:5px;
      }
      
      .radio-btn-icon-no{
      float:right;
      padding-left:5px;
      }
      
      
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
             
    /*  div.form-group.shiny-input-container {*/
    /*  float: right;*/
    /*  text-align: right;*/
    /*  padding-bottom: 14px;*/
    /*  padding-top: 14px;*/
    /*  width: auto;*/
    /*  }*/
      
      .navbar-form {
    /* float: right;*/
       position:absolute;
       right:0;
    /* height: 50px;*/
    /* margin-top: 0px;*/
    /* margin-bottom: 0px;*/
    /* padding-right: 0px;*/
    /* padding-left: 0px;*/
    /* padding-top: 0px;*/
      }
      
      
      /* NAVBARPAGE */
      
      /*body {*/
      /*padding-top: 70px;*/ /*PER FISSARE LA NAVBARPAGE quando position = c("fixed-top")*/
      /*}*/
      
      /*.navbar-fixed-top {/*
      /*top: 0px;/*
      /*border-width: 0px 0px 0px;/*
      /*}/*
      /*.navbar-default {/*
      /*background-color: #1f77b4;/*
      /*}/*
      /*.navbar-default .navbar-brand {/*
      /*color: #FFF;/*
      /*}/*
      /*.nav-tabs {/*
      /*background-color: lightgrey;/*
      /*margin-left: -15px;/*
      /*margin-right: -15px;/*
      /*}/*
      /*-------------------------------*/
      
      .table>tbody>tr>td {
      vertical-align: middle;
      }

/*DIAGNOSTICA*/
      #table_diagno .dataTables_info,
      #drill_diagno .dataTables_info {
      float: left;
      padding-top: 2px;
      margin-top: 50px;
      }   

      #table_diagno .dataTables_paginate,
      #drill_diagno .dataTables_paginate {
      margin-top: 50px;
      }   
      
      #table_diagno th.sorting,
      #drill_diagno th.sorting {
      vertical-align: middle;
      }
      
      #diagnoT1 th.sorting {
      vertical-align: middle;
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #diagnoT1 td {
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #diagnoT1 .dataTables_paginate {
      float: left;
      }   
      
      
      #diagno_bttn1,
      #diagno_bttn2 {
      /*width:350px;*/
      height: 44px;
      border-radius: 50px;
      background-color: #1f77b4;
      }
      
      
      #diagno_downloadEsam_bttn,
      #diagno_downloadConf_bttn {
      /*width:350px;*/
      /*border-radius: 50%;*/
      background-color: orange;
      }
      
      #diagno_prove ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le prove"] {
      font-weight: bold }
/*------------*/      

/*SIEROLOGIA*/
      #table_siero .dataTables_info,
      #drill_siero .dataTables_info {
      float: left;
      padding-top: 2px;
      margin-top: 50px;
      }   

      #table_siero .dataTables_paginate,
      #drill_siero .dataTables_paginate {
      margin-top: 50px;
      }   
      
      #table_siero th.sorting,
      #drill_siero th.sorting {
      vertical-align: middle;
      }
      
      #sieroT1 th.sorting {
      vertical-align: middle;
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #sieroT1 td {
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #sieroT1 .dataTables_paginate {
      float: left;
      }   
      
      
      #siero_bttn1,
      #siero_bttn2 {
      /*width:350px;*/
      height: 44px;
      border-radius: 50px;
      background-color: #1f77b4;
      }
      
      
      #siero_downloadEsam_bttn,
      #siero_downloadConf_bttn {
      /*width:350px;*/
      /*border-radius: 50%;*/
      background-color: orange;
      }
      
      #siero_prove ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le prove"] {
      font-weight: bold }
/*------------*/  

/*FARMACOVIGILANZA*/
      #table_farma .dataTables_info,
      #drill_farma .dataTables_info {
      float: left;
      padding-top: 2px;
      margin-top: 50px;
      }   

      #table_farma .dataTables_paginate,
      #drill_farma .dataTables_paginate {
      margin-top: 50px;
      }   
      
      #table_farma th.sorting,
      #drill_farma th.sorting {
      vertical-align: middle;
      }
      
      #farmaT1 th.sorting {
      vertical-align: middle;
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #farmaT1 td {
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #farmaT1 .dataTables_paginate {
      float: left;
      }   
      
      
      #farma_bttn1,
      #farma_bttn2 {
      /*width:350px;*/
      height: 44px;
      border-radius: 50px;
      background-color: #1f77b4;
      }
      
      
      #farma_downloadEsam_bttn,
      #farma_downloadConf_bttn {
      /*width:350px;*/
      /*border-radius: 50%;*/
      background-color: orange;
      }
      
      #farma_prove ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le prove"] {
      font-weight: bold }
/*------------*/  

/*ITTIOPATOLOGIA*/
      #table_ittio .dataTables_info,
      #drill_ittio .dataTables_info {
      float: left;
      padding-top: 2px;
      margin-top: 50px;
      }   

      #table_ittio .dataTables_paginate,
      #drill_ittio .dataTables_paginate {
      margin-top: 50px;
      }   
      
      #table_ittio th.sorting,
      #drill_ittio th.sorting {
      vertical-align: middle;
      }
      
      #ittioT1 th.sorting {
      vertical-align: middle;
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #ittioT1 td {
      padding-left: 0px;
      padding-right: 0px;
      }
      
      #ittioT1 .dataTables_paginate {
      float: left;
      }   
      
      
      #ittio_bttn1,
      #ittio_bttn2 {
      /*width:350px;*/
      height: 44px;
      border-radius: 50px;
      background-color: #1f77b4;
      }
      
      
      #ittio_downloadEsam_bttn,
      #ittio_downloadConf_bttn {
      /*width:350px;*/
      /*border-radius: 50%;*/
      background-color: orange;
      }
      
      #ittio_prove ~ .selectize-control.single .selectize-dropdown [data-value="Tutte le prove"] {
      font-weight: bold }
/*------------*/  



      
     /*.recalculating { opacity: inherit !important; }*/
      
      /* ICONA FILTRO DATATABLE */
      input.form-control {
      font-family: Montserrat, Segoe UI Symbol;
      font-weight: 600;
      }
    
      body, table {
      font-family: Montserrat;
      }
      
      #thomeSA th.sorting_disabled.dt-left,
      #thomeAU th.sorting_disabled.dt-left,
      #thomeAZ th.sorting_disabled.dt-left {
      vertical-align: middle;
      padding-right: 0px !important;
      padding-left: 0px !important;
      }
      

      #settore-label,
      #visual-label {
      width: 200px;
      float: left;
      }
      
      /*#table .rt-align-center {*/
      /*text-align: left;*/
      /*}*/
      
      
      ')
    )
  ),
 
#HOME PAGE----
tabsetPanel(type = c("pills"),
 tabPanel(
   title = "Home",
   value = "home",
   #img(src="staff.jpg", style="width: 1500px ; align = center"), 
   #hr(),
   br(),
   fluidPage(
     tags$body(HTML("<div id='divLoading'> </div>")),
     style = "padding-right: 0px; padding-left: 0px;",
     grillade(gutter = "xl",
              knack(cols = 2,
              wellPanel(style = "margin-bottom: 0px; height:100%;",
                        h3("Sede Territoriale di Brescia", style = "margin-top: 10px; margin-bottom: 0px;"),
                br(),
                HTML("Opera nell'ambito della sanità animale e della sanità pubblica e si pone come riferimento 
                per il territorio che è caratterizzato da intense produzioni zootecniche e agro-alimentari. L'attività 
                è volta a fornire un servizio di assistenza tecnico-sanitaria agli operatori del settore e comprende 
                l'esecuzione di indagini di laboratorio per la diagnosi e la profilassi delle malattie degli animali 
                da reddito, selvatiche, da cortile e da compagnia. L'utenza è costituita da medici veterinari del servizio 
                veterinario ufficiale e liberi professionisti, da allevatori, da tecnici che operano nel settore agrozootecnico 
                particolarmente ricco in allevamenti suini, bovini, avi-cuniculi e ittici. La struttura fornisce un supporto 
                tecnico per la gestione delle emergenze, per la predisposizione di programmi di biosicurezza, 
                di profilassi e di terapia al fine del miglioramento delle produzioni zootecniche.")
                )),
              
              wellPanel(style = "margin-bottom: 0px; height:100%;",
                        #tags$div(style = "word-break: break-all;",
                        h4("Organizzazione interna"),
                        tags$div(
                          tags$ul(
                            tags$li("Diagnostica Generale"),
                            tags$li("Sierologia"),
                            tags$li("Farmacovigilanza e Antibioticoresistenza"),
                            tags$li("Ittiopatologia"))),
                        br(),
                        h4("Dirigente Responsabile", style="margin-top: 0px;"),
                        tags$div(
                          tags$ul(
                            tags$li(
                              HTML("Dr. Giovanni Loris Alborali"))))#)
                        )
              ),
     br(),
     br(),
     grillade(gutter = "xl",
              wellPanel(style = "margin-bottom: 0px; height:210px; width:100%",
                        h3("Sanità animale", style = "text-align: center;"),
         br(),
         dataTableOutput("thomeSA")#, type = 8, size = 0.5, proxy.height = "50px")
         ),
       wellPanel(style = "margin-bottom: 0px; height:210px; width:100%",
                 h3("Alimenti uomo", style = "text-align: center;"),
         br(),
         dataTableOutput("thomeAU")#, type = 8, size = 0.5, proxy.height = "50px")
         ),
       wellPanel(style = "margin-bottom: 0px; height:210px; width:100%",
                 h3("Alimenti zootecnici", style = "text-align: center;"),
         br(),
         dataTableOutput("thomeAZ")#, type = 8, size = 0.5, proxy.height = "50px")
         )
       )
     )
   ),
 

#ACCETTAZIONE----

tabPanel(
   title = "Accettazione", 
   value = "accettazione",
   br(),
   fluidPage( 
      # useShinyjs(),
     # conditionalPanel(
     #             "false", # always hide the download button, because we will trigger it 
     #             downloadButton("downloadData") # programmatically with shinyjs
     #        ),
   
   fluidRow( 
            wellPanel(
              fluidRow(style = "padding-left: 15px; margin-top: 10px;",
              radioButtons(
                inputId = "visual",
                label = "Visualizza andamento:",
                choices = c("Settimanale",
                            "Giornaliero"), 
                inline = TRUE
              )),
              fluidRow(style = "padding-left: 15px;",
              radioButtons(
                inputId = "settore",
                label = "Settore d'intervento:",
                choices = c( "Tutti",
                             "Sanità Animale",
                             "Alimenti Uomo",
                             "Alimenti Zootecnici",
                             "Controlli Interni Sistema Qualità"), 
                inline = TRUE)),
              br(),

                conditionalPanel(condition = "input.visual == 'Settimanale'",
                                 plotlyOutput("plotacc"),
                                 br(),
                                 #uiOutput("back"),
                                 #br(),
                                 conditionalPanel(
                                   condition = "output.condition",
                                   grillade(n_col = 1,
                                            br(),
                                            fluidRow(
                                              column(3, style = "width: auto;padding-left: 25px;padding-right: 0px;",
                                                     tags$h4("SCARICA TUTTI I CONFERIMENTI", style = "font-weight: 600;font-size: 20px;")),
                                              column(1, align = "left",
                                                     downloadBttn(
                                                       label = NULL,
                                                       outputId = "downloadConfAcc",
                                                       style = "material-circle",
                                                       color = "primary"
                                                       ))),
                                            br(),
                                            br(),
                                            reactableOutput("table")
                                            )
                                   )
                                 ),
                
                conditionalPanel(condition = "input.visual == 'Giornaliero'",
                                 plotlyOutput("plotacc2"),
                                 br(), 
                                 #uiOutput("back2"),
                                 #br(),
                                 conditionalPanel(
                                   condition = "output.condition",
                                   grillade(n_col = 1,
                                            br(),
                                            fluidRow(
                                              column(3, style = "width: auto;padding-left: 25px;padding-right: 0px;",
                                                     tags$h4("SCARICA TUTTI I CONFERIMENTI", style = "font-weight: 600;font-size: 20px;")),
                                              column(1, align = "left",
                                                     downloadBttn(
                                                       label = NULL,
                                                       outputId = "downloadConfAcc2",
                                                       style = "material-circle",
                                                       color = "primary"
                                                     ))),
                                            br(),
                                            br(),
                                            reactableOutput("table2")
                                            )
                                   )
                                 )
              )
            )
   )
   ),

#LABORATORI----
navbarMenu("Laboratori",
      
##DIAGNOSTICA----
tabPanel(
  title = "Laboratorio Diagnostica Generale",
  value = "labdiagnostica",
  br(),
  fluidPage(
    useShinyjs(),
    style = "padding-right: 0px; padding-left: 0px;",
    grillade(gutter = "xl",
             wellPanel(style = "margin-bottom: 0px; height:100%; line-height: 1.8;",#style = "margin-bottom: 50px;",
               h4("Laboratorio di Diagnostica Generale", style = "margin-top: 10px; margin-bottom: 0px;font-weight: 600;"),
               br(),
               br(),
               HTML("Esegue indagini su materiale patologico di animali ed alimenti zootecnici 
                    che comprendono esami anatomo-patologici, batteriologici, parassitologici, 
                    di biologia molecolare, esami specifici per la ricerca di micoplasmi, micobatteri 
                    e il virus della rabbia.<br>L’attività svolta riguarda principalmente la sanità pubblica 
                    e la sanità animale di tutte le specie con particolare riferimento alle specie da 
                    reddito (bovini, suini e specie avicole), agli animali da compagnia e selvatici.<br>Inoltre
                    fornisce un supporto diagnostico all’utenza nell’interpretazione dei risultati 
                    delle indagini di laboratorio e nella loro applicazione in materia di programmi di 
                    profilassi diretta ed indiretta.")),
             knack(cols = 2,
             wellPanel(style = "margin-bottom: 0px; height:100%;",
                       HTML("<div style='line-height:1.1;margin-top:10px;margin-bottom:0px;'>
                             <span style='font-size:18px;vertical-align:middle;font-weight:600;'>Numero di esami eseguiti</span>
                             <span style='font-size:14px;vertical-align:middle;'> (andamento settimanale)</span>
                             </div>"),
                       br(),
               plotlyOutput("diagnoP1")
             ))),
    br(),
    br(),
    grillade(gutter = 'xl',
             n_col = 7, cols_width = c(3, 4),

      wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                uiOutput("head_diagnoT1"),
                #br(),
        dataTableOutput("diagnoT1")
        ),
        wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                  tags$div(
                    tags$h4("Tempo medio di esecuzione per tipo di prova", style = "font-weight: 600;"), 
                    tags$h5("(andamento settimanale)")
                  ),
                  #br(),
                  div(style = "padding-top: 10px;",
          selectInput(inputId = "diagno_prova",
                      label = "Seleziona la prova",
                      choices = c("", levels(factor(diagnostica$prova))))),
          br(),
          plotlyOutput("diagnoP2"#, height='500px'
                       )
          )
        ),
    br(),
    br(),
    fluidRow(
      column(width = 3, offset = 1,# align = "center",
    actionBttn(
      inputId = "diagno_bttn1",
      label = "VEDI CONFERIMENTI",
      color = "primary",
      style = "material-flat",
      block = TRUE
      #icon = icon("sliders")
    )),
    column(1, align = "left",
           downloadBttn(
             label = NULL,
             outputId = "diagno_downloadConf",
             style = "material-circle",
             color = "primary"
             #block = TRUE
           )),
    column(width = 3, offset = 2,# align = "center",
           actionBttn(
      inputId = "diagno_bttn2",
      label = div("VEDI ESAMI"
                  #,icon("search")
                  ),
      color = "primary",
      style = "material-flat",
      block = TRUE
      #icon = icon("sliders")
    )),
    column(1, align = "left",
              downloadBttn(
                label = NULL,
                outputId = "diagno_downloadEsam",
                style = "material-circle",
                color = "primary"
                #block = TRUE
              ))),
    br(),
    br(),
    shinyjs::hidden(
      div(
        id = "diagno_cp1",
    conditionalPanel(
      condition = "input.diagno_bttn1",
    grillade(n_col = 1,
             wellPanel(style = "margin-bottom: 0px;",
               tags$div(
                 tags$h4("TUTTI I CONFERIMENTI", style = "font-weight: 600;"),
                 tags$h5("(seleziona il conferimento per vedere il dettaglio degli esami eseguiti)")
               ),
               br(),
             dataTableOutput("table_diagno", width = "100%"),
             br(),
             hr(style = "border-top: 1px solid black;"),
             br(),
             uiOutput("drillui_diagno")))
    ))),
    shinyjs::hidden(
      div(
        id = "diagno_cp2",
    conditionalPanel(
      condition = "input.diagno_bttn2",
      grillade(n_col = 1,
               wellPanel(style = "margin-bottom: 0px;",
                 div(HTML("<div style='display: inline-block;vertical-align: middle;'>
                             <h4 style='font-weight:600;'>TUTTI GLI ESAMI</h4>
                             <h5> (seleziona <i class='fa-solid fa-gear fa-xl'></i> per filtrare i campi e visualizzare gli esami)</h5>
                             </div>"),
                     div(style='display: inline-block;vertical-align: middle;margin-left: 30px;',
                     
                 dropdownButton(
                   
                   #tags$h3("List of Inputs"),
                   
                   selectInput(inputId = 'diagno_prove',label = "Seleziona la prova",
                               choices = c("","Tutte le prove", levels(factor(diagnostica$prova)))),
                   
                   dateRangeInput('dateRangeEsam_diagno',
                                  label = 'Seleziona la data di inizio analisi',
                                  format = "dd/mm/yyyy",
                                  language = "it-IT",
                                  separator = " a "),
                   br(),
                   div(style = "text-align: center;",
                       actionButton("doesami_diagno",
                                    label = div("Filtra esami", icon("play")),style = "color: white;background-color: #1f77b4;")
                       ),
                   #https://felixluginbuhl.com/scroller/
                   
                   circle = TRUE,
                   status = "primary",
                   icon = icon("gear"),
                   width = "300px",
                   up = TRUE,
                   right = FALSE
                   #tooltip = tooltipOptions(title = "Click to see inputs !")
                 ))),
                 br(),
                 dataTableOutput("diagno_esami"),
                 br(),
                 hr(style = "border-top: 1px solid black;")
     )
      )
    )))
    ) #chiude la fluidPage
  ), #chiude il tabPanel
    
   
           


##SIEROLOGIA----
tabPanel(
  title = "Laboratorio Sierologia",
  value = "labsierologia",
  br(),
  fluidPage(
    useShinyjs(),
    style = "padding-right: 0px; padding-left: 0px;",
    grillade(gutter = "xl",
             wellPanel(style = "margin-bottom: 0px; height:100%; line-height: 1.8;",#style = "margin-bottom: 50px;",
                       h4("Laboratorio di Sierologia", style = "margin-top: 10px; margin-bottom: 0px;font-weight: 600;"),
                       br(),
                       br(),
                       HTML("Esegue esami sierologici ufficiali previsti dai Piani nazionali e regionali di 
                            controllo ed eradicazione delle malattie del bestiame, gli esami richiesti per la 
                            compravendita, lo spostamento e l’importazione degli animali da reddito.<br>Esegue 
                            inoltre numerosi esami sierologici a carattere diagnostico su specifica richiesta dei 
                            medici veterinari che operano sul territorio.<br>Fornisce un supporto all’attività di altri 
                            Laboratori dell’Ente eseguendo esami sierologici ufficiali e/o a carattere diagnostico 
                            su campioni provenienti da altre province e inviati dalle Sezioni diagnostiche periferiche.")),
             knack(cols = 2,
                   wellPanel(style = "margin-bottom: 0px; height:100%;",
                             HTML("<div style='line-height:1.1;margin-top:10px;margin-bottom:0px;'>
                             <span style='font-size:18px;vertical-align:middle;font-weight:600;'>Numero di esami eseguiti</span>
                             <span style='font-size:14px;vertical-align:middle;'> (andamento settimanale)</span>
                             </div>"),
                             br(),
                             plotlyOutput("sieroP1")
                   ))),
    br(),
    br(),
    grillade(gutter = 'xl',
             n_col = 7, cols_width = c(3, 4),
             
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       uiOutput("head_sieroT1"),
                       #br(),
                       dataTableOutput("sieroT1")
             ),
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       tags$div(
                         tags$h4("Tempo medio di esecuzione per tipo di prova", style = "font-weight: 600;"), 
                         tags$h5("(andamento settimanale)")
                       ),
                       #br(),
                       div(style = "padding-top: 10px;",
                           selectInput(inputId = "siero_prova",
                                       label = "Seleziona la prova",
                                       choices = c("", levels(factor(sierologia$prova))))),
                       br(),
                       plotlyOutput("sieroP2"#, height='500px'
                       )
             )
    ),
    br(),
    br(),
    # grillade(
    #   wellPanel(
    #     radioButtons(
    #       inputId = "visual_diagn",
    #       label = "Visualizza andamento:",
    #       choices = c("Settimanale",
    #                   "Giornaliero"),
    #       inline = TRUE),
    #     br(),
    #     plotlyOutput("plot_diagn"))),
    #     br(),
    fluidRow(
      column(width = 3, offset = 1,# align = "center",
             actionBttn(
               inputId = "siero_bttn1",
               label = "VEDI CONFERIMENTI",
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "siero_downloadConf",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             )),
      column(width = 3, offset = 2,# align = "center",
             actionBttn(
               inputId = "siero_bttn2",
               label = div("VEDI ESAMI"
                           #,icon("search")
               ),
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "siero_downloadEsam",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             ))),
    br(),
    br(),
    shinyjs::hidden(
      div(
        id = "siero_cp1",
        conditionalPanel(
          condition = "input.siero_bttn1",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             tags$div(
                               tags$h4("TUTTI I CONFERIMENTI", style = "font-weight: 600;"),
                               tags$h5("(seleziona il conferimento per vedere il dettaglio degli esami eseguiti)")
                             ),
                             br(),
                             dataTableOutput("table_siero", width = "100%"),
                             br(),
                             hr(style = "border-top: 1px solid black;"),
                             br(),
                             uiOutput("drillui_siero")))
        ))),
    shinyjs::hidden(
      div(
        id = "siero_cp2",
        conditionalPanel(
          condition = "input.siero_bttn2",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             # tags$div(
                             #   tags$h4("TUTTI GLI ESAMI", style = "font-weight: 600;"),
                             #   tags$h5("(seleziona il conferimento per vedere il dettaglio degli esami eseguiti)")
                             # ),
                             div(HTML("<div style='display: inline-block;vertical-align: middle;'>
                             <h4 style='font-weight:600;'>TUTTI GLI ESAMI</h4>
                             <h5> (seleziona <i class='fa-solid fa-gear fa-xl'></i> per filtrare i campi e visualizzare gli esami)</h5>
                             </div>"),
                                 div(style='display: inline-block;vertical-align: middle;margin-left: 30px;',
                                     
                                     dropdownButton(
                                       
                                       #tags$h3("List of Inputs"),
                                       
                                       selectInput(inputId = 'siero_prove',label = "Seleziona la prova",
                                                   choices = c("","Tutte le prove", levels(factor(sierologia$prova)))),
                                       
                                       dateRangeInput('dateRangeEsam_siero',
                                                      label = 'Seleziona la data di inizio analisi',
                                                      format = "dd/mm/yyyy",
                                                      language = "it-IT",
                                                      separator = " a "),
                                       br(),
                                       div(style = "text-align: center;",
                                           actionButton("doesami_siero",
                                                        label = div("Filtra esami", icon("play")),style = "color: white;background-color: #1f77b4;")
                                       ),
                                       #https://felixluginbuhl.com/scroller/
                                       
                                       circle = TRUE,
                                       status = "primary",
                                       icon = icon("gear"),
                                       width = "300px",
                                       up = TRUE,
                                       right = FALSE
                                       #tooltip = tooltipOptions(title = "Click to see inputs !")
                                     ))),
                             br(),
                             dataTableOutput("siero_esami"),
                             br(),
                             hr(style = "border-top: 1px solid black;")
                             
                   )
          ))
        ))
  ) #chiude la fluidPage
), #chiude il tabPanel



##FARMACOVIGILANZA e ANTIBIOTICORESISTENZA----
tabPanel(
  title = "Laboratorio Farmacovigilanza e Antibioticoresistenza",
  value = "labfarmacovigilanza",
  br(),
  fluidPage(
    useShinyjs(),
    style = "padding-right: 0px; padding-left: 0px;",
    grillade(gutter = "xl",
             wellPanel(style = "margin-bottom: 0px; height:100%; line-height: 1.8;",#style = "margin-bottom: 50px;",
                       h4("Laboratorio di Farmacovigilanza e Antibioticoresistenza", style = "margin-top: 10px; margin-bottom: 0px;font-weight: 600;"),
                       br(),
                       br(),
                       HTML("Opera nell’ambito della Farmacovigilanza e farmacosorveglianza veterinaria con l’obiettivo
                            di monitorare l’utilizzo e il consumo dei farmaci con particolare riferimento agli 
                            antimicrobici destinati agli animali da reddito.<br>Esegue indagini di laboratorio 
                            microbiologiche e di biologia molecolare con l’obiettivo di valutare l’antibiotico 
                            resistenza di microrganismi patogeni e commensali al fine di una miglior programmazione 
                            degli interventi preventivi e di management in azienda.<br>L’attività viene svolta 
                            nell’ambito di Classyfarm, il sistema di categorizzazione del rischio degli allevamenti.")),
             knack(cols = 2,
                   wellPanel(style = "margin-bottom: 0px; height:100%;",
                             HTML("<div style='line-height:1.1;margin-top:10px;margin-bottom:0px;'>
                             <span style='font-size:18px;vertical-align:middle;font-weight:600;'>Numero di esami eseguiti</span>
                             <span style='font-size:14px;vertical-align:middle;'> (andamento settimanale)</span>
                             </div>"),
                             br(),
                             plotlyOutput("farmaP1")
                   ))),
    br(),
    br(),
    grillade(gutter = 'xl',
             n_col = 7, cols_width = c(3, 4),
             
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       uiOutput("head_farmaT1"),
                       #br(),
                       dataTableOutput("farmaT1")
             ),
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       tags$div(
                         tags$h4("Tempo medio di esecuzione per tipo di prova", style = "font-weight: 600;"), 
                         tags$h5("(andamento settimanale)")
                       ),
                       #br(),
                       div(style = "padding-top: 10px;",
                           selectInput(inputId = "farma_prova",
                                       label = "Seleziona la prova",
                                       choices = c("", levels(factor(farmacovigilanza$prova))))),
                       br(),
                       plotlyOutput("farmaP2"#, height='500px'
                       )
             )
    ),
    br(),
    br(),
    fluidRow(
      column(width = 3, offset = 1,# align = "center",
             actionBttn(
               inputId = "farma_bttn1",
               label = "VEDI CONFERIMENTI",
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "farma_downloadConf",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             )),
      column(width = 3, offset = 2,# align = "center",
             actionBttn(
               inputId = "farma_bttn2",
               label = div("VEDI ESAMI"
                           #,icon("search")
               ),
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "farma_downloadEsam",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             ))),
    br(),
    br(),
    shinyjs::hidden(
      div(
        id = "farma_cp1",
        conditionalPanel(
          condition = "input.farma_bttn1",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             tags$div(
                               tags$h4("TUTTI I CONFERIMENTI", style = "font-weight: 600;"),
                               tags$h5("(seleziona il conferimento per vedere il dettaglio degli esami eseguiti)")
                             ),
                             br(),
                             dataTableOutput("table_farma", width = "100%"),
                             br(),
                             hr(style = "border-top: 1px solid black;"),
                             br(),
                             uiOutput("drillui_farma")))
        ))),
    shinyjs::hidden(
      div(
        id = "farma_cp2",
        conditionalPanel(
          condition = "input.farma_bttn2",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             div(HTML("<div style='display: inline-block;vertical-align: middle;'>
                             <h4 style='font-weight:600;'>TUTTI GLI ESAMI</h4>
                             <h5> (seleziona <i class='fa-solid fa-gear fa-xl'></i> per filtrare i campi e visualizzare gli esami)</h5>
                             </div>"),
                                 div(style='display: inline-block;vertical-align: middle;margin-left: 30px;',
                                     
                                     dropdownButton(
                                       
                                       #tags$h3("List of Inputs"),
                                       
                                       selectInput(inputId = 'farma_prove',label = "Seleziona la prova",
                                                   choices = c("","Tutte le prove", levels(factor(farmacovigilanza$prova)))),
                                       
                                       dateRangeInput('dateRangeEsam_farma',
                                                      label = 'Seleziona la data di inizio analisi',
                                                      format = "dd/mm/yyyy",
                                                      language = "it-IT",
                                                      separator = " a "),
                                       br(),
                                       div(style = "text-align: center;",
                                           actionButton("doesami_farma",
                                                        label = div("Filtra esami", icon("play")),style = "color: white;background-color: #1f77b4;")
                                       ),
                                       #https://felixluginbuhl.com/scroller/
                                       
                                       circle = TRUE,
                                       status = "primary",
                                       icon = icon("gear"),
                                       width = "300px",
                                       up = TRUE,
                                       right = FALSE
                                       #tooltip = tooltipOptions(title = "Click to see inputs !")
                                     ))),
                             br(),
                             dataTableOutput("farma_esami"),
                             br(),
                             hr(style = "border-top: 1px solid black;")
                             
                   )
          ))
      ))
) #chiude la fluidPage
), #chiude il tabPanel

##ITTIOPATOLOGIA----
tabPanel(
  title = "Laboratorio Ittiopatologia",
  value = "labittiopatologia",
  br(),
  fluidPage(
    useShinyjs(),
    style = "padding-right: 0px; padding-left: 0px;",
    grillade(gutter = "xl",
             wellPanel(style = "margin-bottom: 0px; height:100%; line-height: 1.8;",#style = "margin-bottom: 50px;",
                       h4("Laboratorio di Ittiopatologia", style = "margin-top: 10px; margin-bottom: 0px;font-weight: 600;"),
                       br(),
                       br(),
                       HTML("Esegue indagini su materiale patologico di pesci d’acqua dolce e salata, allevati 
                       e presenti in corpi d’acqua naturale.<br>Le indagini comprendono esami anatomo-patologici, esami 
                       virologici, esami batteriologici, ed esami parassitologici.<br>L’attività viene svolta con 
                            due finalità principali: fornire un servizio diagnostico ai veterinari e agli operatori 
                            del settore dell’acquacoltura e garantire il supporto ai Servizi Veterinari delle ATS 
                            per il decreto legislativo 4 agosto 2008, n. 148, relativo alle condizioni di 
                            polizia sanitaria nonché alla prevenzione e alle misure di lotta contro di talune 
                            malattie degli animali acquatici.")),
             knack(cols = 2,
                   wellPanel(style = "margin-bottom: 0px; height:100%;",
                             HTML("<div style='line-height:1.1;margin-top:10px;margin-bottom:0px;'>
                             <span style='font-size:18px;vertical-align:middle;font-weight:600;'>Numero di esami eseguiti</span>
                             <span style='font-size:14px;vertical-align:middle;'> (andamento settimanale)</span>
                             </div>"),
                             br(),
                             plotlyOutput("ittioP1")
                   ))),
    br(),
    br(),
    grillade(gutter = 'xl',
             n_col = 7, cols_width = c(3, 4),
             
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       uiOutput("head_ittioT1"),
                       #br(),
                       dataTableOutput("ittioT1")
             ),
             wellPanel(style = "margin-bottom: 0px; height:650px;", #height:100%;
                       tags$div(
                         tags$h4("Tempo medio di esecuzione per tipo di prova", style = "font-weight: 600;"), 
                         tags$h5("(andamento settimanale)")
                       ),
                       #br(),
                       div(style = "padding-top: 10px;",
                           selectInput(inputId = "ittio_prova",
                                       label = "Seleziona la prova",
                                       choices = c("", levels(factor(ittiopatologia$prova))))),
                       br(),
                       plotlyOutput("ittioP2"#, height='500px'
                       )
             )
    ),
    br(),
    br(),
    fluidRow(
      column(width = 3, offset = 1,# align = "center",
             actionBttn(
               inputId = "ittio_bttn1",
               label = "VEDI CONFERIMENTI",
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "ittio_downloadConf",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             )),
      column(width = 3, offset = 2,# align = "center",
             actionBttn(
               inputId = "ittio_bttn2",
               label = div("VEDI ESAMI"
                           #,icon("search")
               ),
               color = "primary",
               style = "material-flat",
               block = TRUE
               #icon = icon("sliders")
             )),
      column(1, align = "left",
             downloadBttn(
               label = NULL,
               outputId = "ittio_downloadEsam",
               style = "material-circle",
               color = "primary"
               #block = TRUE
             ))),
    br(),
    br(),
    shinyjs::hidden(
      div(
        id = "ittio_cp1",
        conditionalPanel(
          condition = "input.ittio_bttn1",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             tags$div(
                               tags$h4("TUTTI I CONFERIMENTI", style = "font-weight: 600;"),
                               tags$h5("(seleziona il conferimento per vedere il dettaglio degli esami eseguiti)")
                             ),
                             br(),
                             dataTableOutput("table_ittio", width = "100%"),
                             br(),
                             hr(style = "border-top: 1px solid black;"),
                             br(),
                             uiOutput("drillui_ittio")))
        ))),
    shinyjs::hidden(
      div(
        id = "ittio_cp2",
        conditionalPanel(
          condition = "input.ittio_bttn2",
          grillade(n_col = 1,
                   wellPanel(style = "margin-bottom: 0px;",
                             div(HTML("<div style='display: inline-block;vertical-align: middle;'>
                             <h4 style='font-weight:600;'>TUTTI GLI ESAMI</h4>
                             <h5> (seleziona <i class='fa-solid fa-gear fa-xl'></i> per filtrare i campi e visualizzare gli esami)</h5>
                             </div>"),
                                 div(style='display: inline-block;vertical-align: middle;margin-left: 30px;',
                                     
                                     dropdownButton(
                                       
                                       #tags$h3("List of Inputs"),
                                       
                                       selectInput(inputId = 'ittio_prove',label = "Seleziona la prova",
                                                   choices = c("","Tutte le prove", levels(factor(ittiopatologia$prova)))),
                                       
                                       dateRangeInput('dateRangeEsam_ittio',
                                                      label = 'Seleziona la data di inizio analisi',
                                                      format = "dd/mm/yyyy",
                                                      language = "it-IT",
                                                      separator = " a "),
                                       br(),
                                       div(style = "text-align: center;",
                                           actionButton("doesami_ittio",
                                                        label = div("Filtra esami", icon("play")),style = "color: white;background-color: #1f77b4;")
                                       ),
                                       #https://felixluginbuhl.com/scroller/
                                       
                                       circle = TRUE,
                                       status = "primary",
                                       icon = icon("gear"),
                                       width = "300px",
                                       up = TRUE,
                                       right = FALSE
                                       #tooltip = tooltipOptions(title = "Click to see inputs !")
                                     ))),
                             br(),
                             dataTableOutput("ittio_esami"),
                             br(),
                             hr(style = "border-top: 1px solid black;")
                             
                   )
          ))
      ))
  ) #chiude la fluidPage
) #chiude il tabPanel
), #chiude il tabsetPanel




#PIVOT----
tabPanel(
  title = "Tabelle Pivot",
  value = "pivot",
  br(),
  fluidPage(style = "padding-right: 0px; padding-left: 0px;",
    fluidRow(
      
      column(12, align = "center",
             #wellPanel(
             shinycssloaders::withSpinner(
               proxy.height = "500px",
               rpivotTableOutput("pivot", height = "100%"), type = 8)
      #)
    ))
  )
  )
 
  # tabPanel(
  #   title = "Conferimenti",
  #   value = "conf",
  #   #h3(uiOutput("aggconf")),
  #   fluidRow(
  #     br(),
  #     downloadButton("downloadData", "Scarica i dati"),
  #     DTOutput("conferimenti")
  #   )
  # ),
  # tabPanel(
  #   title = "Prove",
  #   value = "prove",
  #   #h3(uiOutput("aggprove")),
  #   fluidRow(
  #     br(),
  #     downloadButton("downloadData2", "Scarica i dati"),
  #     DTOutput("esami")
  #   )
  # )
  # tabPanel(
  #   title = "Attività Ufficiale ",href = "http://rshiny.izsler.it/costiricavi",
  #   value = "uff",
  #   fluidRow(
  #   )
  # ),
  # tabPanel(
  #   title = "Attività in autocontrollo",
  #   value = "autocont",
  #   fluidRow(
  #   )
  # )
)
),
br(),
#FOOTER----
#https://stackoverflow.com/questions/67763901/footer-position-in-shiny
tags$footer(uiOutput("aggconf"), class = "footer")
)
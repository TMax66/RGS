ui <- navbarPage(
  title = div(
    HTML("Report Gestionale Sanitario di Reparto - Sede Territoriale di Brescia")
    ),
  #theme = bslib::bs_theme(3),

#CSS----

tags$head(
  # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
  # tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css", integrity="sha256-+N4/V/SbAFiW1MPBCXnfnP9QSN3+Keu+NlB+0ev/YKQ=", crossorigin="anonymous"),
  tags$style(
    HTML(
      '

      #table_diagn .dataTables_info,
      #drill_diagn .dataTables_info{
      float: left;
      margin-top: 50px;
      }   

      #table_diagn .dataTables_paginate,
      #drill_diagn .dataTables_paginate{
      margin-top: 50px;
      }    

      
     /*.recalculating { opacity: inherit !important; }*/
      
      /* ICONA FILTRO DATATABLE */
      input.form-control {
      font-family: Montserrat, Segoe UI Symbol;
      font-weight: 600;
      }
    
      body {
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
tabsetPanel( 
 tabPanel(
   title = "Home",
   value = "home",
   #img(src="staff.jpg", style="width: 1500px ; align = center"), 
   #hr(),
   br(),
   fluidPage(
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
              wellPanel(style = "margin-bottom: 0px; height:100%; width:100%",
                        h3("SANITÀ ANIMALE", style = "text-align: center;"),
         br(),
         dataTableOutput("thomeSA")
         ),
       wellPanel(style = "margin-bottom: 0px; height:100%; width:100%",
                 h3("ALIMENTI UOMO", style = "text-align: center;"),
         br(),
         dataTableOutput("thomeAU")
         ),
       wellPanel(style = "margin-bottom: 0px; height:100%; width:100%",
                 h3("ALIMENTI ZOOTECNICI", style = "text-align: center;"),
       br(),
       dataTableOutput("thomeAZ"))
       ),
     br(),
     br(),
     fluidRow(style = "margin-left:0px",
              div(style = "font-weight: 500;",
                  uiOutput("aggconf")
                  )
              )
     )
   ),
 

#Accettazione----

tabPanel(
   title = "Accettazione", 
   value = "accettaz",
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
              fluidRow(
                
                conditionalPanel(condition = "input.visual == 'Settimanale'",
                                 plotlyOutput("plotacc"),
                                 br(),
                                 # downloadButton("downloadData", "Download"),
                                 uiOutput("back"),
                                 br(),
                                 reactableOutput("table")#,
                                 # downloadButton("downloadData", "Scarica i dati")
                                 ),
                
                conditionalPanel(condition = "input.visual == 'Giornaliero'",
                                 plotlyOutput("plotacc2"),
                                 br(), 
                                 # downloadButton("downloadData", "Download"),
                                 uiOutput("back2"),
                                 br(),
                                 reactableOutput("table2")#,
                                 # downloadButton("downloadData", "Scarica i dati")
                                 )
                )
              )
            )
   )
   ),
#LABORATORI----
navbarMenu("Laboratori",

           
           # ##Lab Diagnostica Generale----
           # tabPanel(
           #   title = "Laboratorio Diagnostica Generale",
           #   value = "labdiagnostica",
           #   br(),
           #   fluidPage(
           #     fluidRow(style = "height:500px;",
           #              column(5, style = "padding-left:0px;",
           #                     wellPanel(style = "height:300px;    margin-bottom: 50px;",
           #                               h3("Laboratorio di Diagnostica Generale", style = "margin-top: 10px; margin-bottom: 0px;"),
           #                               br(),
           #                               HTML("Esegue indagini su materiale patologico di animali ed alimenti zootecnici 
           #          che comprendono esami anatomo-patologici, batteriologici, parassitologici, 
           #          di biologia molecolare, esami specifici per la ricerca di micoplasmi, micobatteri 
           #          e il virus della rabbia. L’attività svolta riguarda principalmente la sanità pubblica 
           #          e la sanità animale di tutte le specie con particolare riferimento alle specie da 
           #          reddito (bovini, suini e specie avicole), agli animali da compagnia e selvatici. 
           #          Inoltre fornisce un supporto diagnostico all’utenza nell’interpretazione dei risultati 
           #          delle indagini di laboratorio e nella loro applicazione in materia di programmi di 
           #          profilassi diretta ed indiretta.")),
           #                     #br(),
           #                     wellPanel(style = "height:100px;",
           #                               selectInput(inputId = "diagnos",
           #                                           label = "Seleziona la prova",
           #                                           choices = c("", levels(factor(diagnostica$prova))))
           #                     )
           #              ),
           #              column(7, style = "padding-right:0px;",
           #                     wellPanel(style = "height:450px;",
           #                               plotlyOutput("Dp1")))
           #     ),
           #     #br(),
           #     fluidRow(#style = "height:500px;",
           #       column(5,
           #              wellPanel(
           #                tableOutput("Dt1"))),
           #       column(7,
           #              wellPanel(plotOutput("Dp2")
           #              ))
           #     ),
           #     
           #     fluidRow(
           #       wellPanel(
           #         dataTableOutput("SumDiagn"),
           #         hr(),
           #         br(),
           #         dataTableOutput("ddrill")
           #       )
           #     )
           #   )
           #   
           #   # h3(uiOutput("aggconf")),
           #   # fluidRow(
           #   #   br(),
           #   #   downloadButton("downloadData", "Scarica i dati"),
           #   #   DTOutput("conferimenti")
           #   # )
           #   
           # ),
           # 
           # 
           # 
           #            
##Lab Diagnostica Generale----
tabPanel(
  title = "Laboratorio Diagnostica Generale",
  value = "labdiagnostica",
  br(),
  fluidPage(
    style = "padding-right: 0px; padding-left: 0px;",
    grillade(gutter = "xl",
             wellPanel(style = "margin-bottom: 0px; height:100%;",#style = "margin-bottom: 50px;",
               h4("Laboratorio di Diagnostica Generale", style = "margin-top: 10px; margin-bottom: 0px;font-weight: 600;"),
               br(),
               br(),
               HTML("Esegue indagini su materiale patologico di animali ed alimenti zootecnici 
                    che comprendono esami anatomo-patologici, batteriologici, parassitologici, 
                    di biologia molecolare, esami specifici per la ricerca di micoplasmi, micobatteri 
                    e il virus della rabbia. L’attività svolta riguarda principalmente la sanità pubblica 
                    e la sanità animale di tutte le specie con particolare riferimento alle specie da 
                    reddito (bovini, suini e specie avicole), agli animali da compagnia e selvatici. 
                    Inoltre fornisce un supporto diagnostico all’utenza nell’interpretazione dei risultati 
                    delle indagini di laboratorio e nella loro applicazione in materia di programmi di 
                    profilassi diretta ed indiretta.")),
             knack(cols = 2,
             wellPanel(style = "margin-bottom: 0px; height:100%;",
               plotlyOutput("Dp1")
             ))),
    br(),
    br(),
    grillade(gutter = 'xl',

      wellPanel(style = "margin-bottom: 0px; height:100%;",
                uiOutput("head_Dt1"),
                br(),
        dataTableOutput("Dt1")
        ),
        wellPanel(style = "margin-bottom: 0px; height:100%",
                  tags$div(
                    tags$h4("Tempi medi di esecuzione", style = "font-weight: 600;"), 
                    tags$h5("(andamento settimanale)")
                  ),
                  br(),
          selectInput(inputId = "diagnos",
                      label = "Seleziona la prova",
                      choices = c("", levels(factor(diagnostica$prova)))),
          br(),
          plotOutput("Dp2")
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
        br(),
        br(),
    grillade(n_col = 1,
             wellPanel(
               tags$div(
                 tags$h4("TUTTI I CONFERIMENTI", style = "text-align:center;font-weight: 600;")
               ),
             uiOutput("table_diagno"),
             br(),
             br(),
             uiOutput("drill_diagno")))
      
    ) #chiude la fluidPage
  ), #chiude il tabPanel
    
   
           
#Lab Sierologia----
tabPanel(
  title = "Laboratorio Sierologia",
  value = "labsiero",
  fluidPage(
    fluidRow(
      column(6,
             wellPanel(
               h2("Laboratorio di Sierologia"),
               br(),
               h4("Il Laboratorio di Sierologia della Sezione di Modena è il Laboratorio regionale di 
               riferimento per l’esecuzione degli esami previsti nei piani di profilassi della Brucellosi 
               e Leucosi bovina eseguendo le prove sierologiche in automazione su latte bovino nell’ambito 
                  dei piani di sorveglianza regionale per tutta la regione Emilia-Romagna."),
               hr(),
               br(),
               br(),
               br(),
               br(),
             # selectInput("prove", "Seleziona la prova", choices = c("", levels(factor(siero$prova)))),
             plotlyOutput("Sp1")
             )
             ),
      column(6,
             wellPanel(
               tableOutput("St1"),
               hr(),
               br(),
               plotOutput("Sp2")
               )
             )
      ),
    fluidRow(
      wellPanel(
        dataTableOutput("SumSiero"),
        hr(),
        br(),
        dataTableOutput("sdrill")
        )
      )
    )
  ),
 
#Lab Microbiologia Alimenti----
tabPanel(
  title = "Laboratorio Microbiologia Alimenti",
  value = "labalim",
  fluidPage(
    fluidRow(
      column(6,
             wellPanel(
               h2("Laboratorio di Microbiologia Alimenti"),
               br(),
               h4("Il Laboratorio di Microbiologia degli Alimenti esegue determinazioni analitiche 
                   su alimenti e matrici ambientali delle filiere dei prodotti di origine animale e 
                   vegetale, inclusi gli alimenti destinati all’alimentazione animale. Il Laboratorio 
                   e referente per i piani di controllo ufficiali eseguiti dalle Autorità Sanitarie 
                   competenti. Si propone, inoltre, quale partner erogatore di servizi per gli operatori 
                   del settore agro-alimentare (autocontrollo) e per la ricerca applicata al settore 
                   della produzione di alimenti."),
               hr(),
               br(),
               br(),
               br(),
               br(),
               plotlyOutput("MAp1")
               )
             ),
      column(6,
             wellPanel(
               tableOutput("MAt1"),
               hr(),
               br(),
               selectInput(inputId = "microalim",
                           label = "Seleziona la prova",
                           choices = c("", levels(factor(alimenti$prova)))),
               br(),
               hr(),
               plotOutput("MAp2")
               )
             )
      ),
    fluidRow(
      wellPanel(
        dataTableOutput("SumMA"),
        hr(),
        br(),
        dataTableOutput("madrill")
        )
      )
    )
)
  
  # h3(uiOutput("aggconf")),
  # fluidRow(
  #   br(),
  #   downloadButton("downloadData", "Scarica i dati"),
  #   DTOutput("conferimenti")
  # )
  
  ),

#Lab Biologia Molecolare-----

 # tabPanel(
 #   title = "Laboratorio di biologia molecolare e TSE",
 #   value = "biologmolec",
 #   fluidPage(
 #     fluidRow(
 #       column(6, 
 #              wellPanel(
 #                h2("Laboratorio di biologia molecolare e TSE"), br(),
 #                
 #                h4(""), 
 #                
 #                hr(), br(), br(), br(), br(),
 #                
 #                plotlyOutput("bmp1")
 #              )), 
 #       column(6, 
 #              wellPanel(
 #                tableOutput("bmt1"), 
 #                hr(),br(),
 #                selectInput(inputId = "biomolec",
 #                            label = "Seleziona la prova",
 #                            choices = c( "", levels(factor(biomol$prova)))),
 #                br(), hr(),
 #                plotOutput("bmp2")
 #                
 #              ))
 #     ), 
 #     fluidRow(
 #       wellPanel(
 #         dataTableOutput("Sumbm"), hr(), br(), 
 #         dataTableOutput("bmdrill")
 #       )
 #     )
 #   )
 #   # h3(uiOutput("aggconf")),
 #   # fluidRow(
 #   #   br(),
 #   #   downloadButton("downloadData", "Scarica i dati"),
 #   #   DTOutput("conferimenti")
 #   # )
 # ),
 
 #laboratorio covid19----
 # tabPanel(
 #   title = "Laboratorio di diagnostica COVID-19",
 #   value = "labcovid",
 #   fluidPage(
 #     fluidRow(
 #       column(6, 
 #              wellPanel(
 #                h2("Laboratorio di diagnostica COVID-19"), br(),
 #                
 #                h4(""), 
 #                
 #                hr(), br(), br(), br(), br(),
 #                
 #                plotOutput("covp1")
 #              )), 
 #       column(6, 
 #              wellPanel(
 #                tableOutput("covt1"), 
 #                hr(),br(),
 #                selectInput(inputId = "cov",
 #                            label = "Seleziona la prova",
 #                            choices = c( "", levels(factor(biomol$prova)))),
 #                br(), hr(),
 #                plotOutput("bmp2")
 #                
 #              ))
 #     ), 
 #     fluidRow(
 #       wellPanel(
 #         dataTableOutput("Sumbm"), hr(), br(), 
 #         dataTableOutput("bmdrill")
 #       )
 #     )
 #   )
   # h3(uiOutput("aggconf")),
   # fluidRow(
   #   br(),
   #   downloadButton("downloadData", "Scarica i dati"),
   #   DTOutput("conferimenti")
   # )
# ),
 
#laboratorio qualità----
 
 tabPanel(
   title = "Qualità",
   value = "qualità",
   # h3(uiOutput("aggconf")),
   # fluidRow(
   #   br(),
   #   downloadButton("downloadData", "Scarica i dati"),
   #   DTOutput("conferimenti")
   # )
 ),
   
   
   # Attività Ufficiale
   
   tabPanel(
     title = "Attività Ufficiale",
     value = "attuff"
     # h3(uiOutput("aggconf")),
     # fluidRow(
     #   br(),
     #   downloadButton("downloadData", "Scarica i dati"),
     #   DTOutput("conferimenti")
     # )
   ),
   
   
   
 
 #laboratorio autocontrollo-----
 tabPanel(
   title = "Attività per Autocontrollo",
   value = "autocontrollo"
   # h3(uiOutput("aggconf")),
   # fluidRow(
   #   br(),
   #   downloadButton("downloadData", "Scarica i dati"),
   #   DTOutput("conferimenti")
   # )
 ),
 

 #Attività di ricerca-----
 tabPanel(
   title = "Attività  di ricerca",
   value = "autocontrollo"
 ),

#Tabella pivot----
tabPanel(
  title = "Tabelle Pivot",
  fluidPage(
    fluidRow(
      column(6,div(style="height:10px"),rpivotTableOutput("pivot") ))

)),
 
  tabPanel(
    title = "Conferimenti",
    value = "conf",
    #h3(uiOutput("aggconf")),
    fluidRow(
      br(),
      downloadButton("downloadData", "Scarica i dati"),
      DTOutput("conferimenti")
    )
  ),
  tabPanel(
    title = "Prove",
    value = "prove",
    #h3(uiOutput("aggprove")),
    fluidRow(
      br(),
      downloadButton("downloadData2", "Scarica i dati"),
      DTOutput("esami")
    )
  )
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
)
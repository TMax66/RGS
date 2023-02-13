ui <- tagList(navbarPageWithInputs(
  "IZSLER Sede Territoriale di Modena",
  #position = c("fixed-top"),
  inputs = radioGroupButtons(
    inputId = "selanno",
    label = NULL,
    status = "primary",
    choices = c("2022", "2023"),
    individual = TRUE,
    justified = FALSE,
    selected = 2023
  ),
 # theme = bslib::bs_theme(4),
 tags$head(
   # tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
   # tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css", integrity="sha256-+N4/V/SbAFiW1MPBCXnfnP9QSN3+Keu+NlB+0ev/YKQ=", crossorigin="anonymous"),
   tags$style(
     HTML(
       '
     .navbar-nav{display:flex}
     .nav>li>a {
    position: relative;
    display: block;
    padding: 10px 10px;
}
     
     .navbar-nav a {padding-left:0px}
      .navbar-form {
        position: absolute;
    
    right: 0;
      }

      ')
   )
 ),
 #home----
 
 tabPanel(
   title = "Home",
   value = "home",
   
     img(src="staff.jpg", style="width: 1500px ; align = center"), 
   hr(), br(),
   fluidPage(
    fluidRow( 
     column(6, 
            wellPanel(
              h2("Sede Territoriale di Modena"), br(),
              
              h4("Il lavoro svolto dalla Sede Territoriale di Modena è orientato, in linea generale, prevalentemente verso il controllo degli Alimenti per l’uomo, essendo particolarmente forte la vocazione provinciale nel comparto agroalimentare, caratterizzata da una consolidata e sviluppata industria conserviera della carne, che si distribuisce uniformemente sul territorio provinciale con ben 205 imprese e molte di esse grandi esportatrici, ma anche dell’attività diagnostica in Sanità Animale, essendo la provincia ancora vocata all’allevamento da reddito, in particolare bovini e suini. Tali attività vengono espletate fornendo servizi sia di tipo tecnico (analisi) che professionale (consulenze, diagnosi), allo scopo di soddisfare la domanda di consulenza/attività analitica dei settori sopraccitati, in perenne evoluzione. 
                 Inoltre la Sede territoriale di Modena e a tutt’oggi il laboratorio di riferimento Regionale per la diagnosi e prevenzione delle TSE.")
            )), 
     column (6, 
               column(6,
               img(src="SEZ-MO.gif", style="align = center")
               ), 
               column(6, 
              h3("Direttore: Dr. Gianluca Rugna"),
             )
             )
    ), 
    
    fluidRow(
      column(4, 
      wellPanel(
        h3("SANITA' ANIMALE"), 
        tableOutput("thomeSA")
      )), 
      column(4, 
      wellPanel(
        h3("ALIMENTI UOMO"), 
        tableOutput("thomeAU")
      )), 
      column(4,
       wellPanel(
         h3("ALIMENTI ZOOTECNICI"), 
         tableOutput("thomeAZ")
       ))
             ), 
    fluidRow(
      uiOutput("aggconf")
    )

      )
    ),
 
 
 
 
#Accettazione----

 
 tabPanel(
   title = "Accettazione", 
   fluidPage( 
     # useShinyjs(),
     # conditionalPanel(
     #             "false", # always hide the download button, because we will trigger it 
     #             downloadButton("downloadData") # programmatically with shinyjs
     #        ),
   
   fluidRow( 
            wellPanel(
              radioButtons(
                inputId = "settore",
                label = "Settore d'intervento",
                choices = c( "Tutti", levels(factor(conf$settore))), 
                inline = TRUE) , br(),
              
              plotlyOutput("plotacc"), br(), 
              
              # downloadButton("downloadData", "Download"),
              

              uiOutput("back"), br(),
             
             reactableOutput("table")#, 
             
             
             
             #downloadButton("downloadData", "Scarica i dati")
            )
  
 ))),
 
#Laboratorio Sierologia----
 
tabPanel(
   title = "Laboratorio Sierologia",
   value = "labsiero",
   fluidPage(
     fluidRow(
       column(6, 
       wellPanel(
         h2("Laboratorio di Sierologia"), br(),
         
         h4("Il Laboratorio di Sierologia della Sezione di Modena è il Laboratorio regionale di riferimento per l’esecuzione degli esami previsti nei piani di profilassi della Brucellosi e Leucosi bovina eseguendo le prove sierologiche in automazione su latte bovino
            nell’ambito dei piani di sorveglianza regionale per tutta la regione Emilia-Romagna."), 
         
         hr(), br(), br(), br(), br(),
         
        # selectInput("prove", "Seleziona la prova", choices = c("", levels(factor(siero$prova)))), 
         
         plotlyOutput("Sp1")
        
       )), 
      column(6, 
             wellPanel( 
            tableOutput("St1"), 
            hr(), br(),
            plotOutput("Sp2")
             )
             
             )
     ), 
     fluidRow(
       wellPanel(
         dataTableOutput("SumSiero"), hr(), br(),
         dataTableOutput("sdrill")
       )
     )
   )
  
 ),
 
 #Laboratorio Diagnostica Generale----
 
 tabPanel(
   title = "Laboratorio Diagnostica Generale",
   value = "labdiagnostica",
   fluidPage(
     fluidRow(
       column(6, 
              wellPanel(
                h2("Laboratorio di Diagnostica Generale"), br(),
                
                h4("Il Laboratorio di Diagnostica Generale si propone quale erogatore di
                Servizi diagnostici in Sanita animale eseguendo indagini su materiale 
                patologico prelevato da animali da reddito e da affezione, 
                conferiti da veterinari e da privati, al fine di fornire 
                un servizio diagnostico specializzato. Si avvale di attività 
                tipicamente professionali, quali gli esami necroscopici 
                e gli esami anatomo-patologici, ed attività analitiche, 
                quali esami batteriologici e parassitologici, 
                nonché esami di altra natura (virologici, istologici, tossicologici ecc.), 
                in collaborazione con altri laboratori della Sede Centrale e di altre 
                Sezioni Diagnostiche. Come attività peculiare di questa Sezione si segnala 
                l’attività diagnostica in patologia apistica."), 
                
                hr(), br(), br(), br(), br(),
                
                plotlyOutput("Dp1")
              )), 
       column(6, 
              wellPanel(
                tableOutput("Dt1"), 
                hr(),br(),
                selectInput(inputId = "diagnos",
                            label = "Seleziona la prova",
                            choices = c( "", levels(factor(diagnostica$prova)))),
                br(), hr(),
                plotOutput("Dp2")
                
              ))
     ), 
     fluidRow(
       wellPanel(
         dataTableOutput("SumDiagn"), hr(), br(), 
         dataTableOutput("ddrill")
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
 
 #Laboratorio Microbiologia Alimenti----
 tabPanel(
   title = "Laboratorio Microbiologia Alimenti",
   value = "labalim",
   fluidPage(
     fluidRow(
       column(6, 
              wellPanel(
                h2("Laboratorio di Microbiologia Alimenti"), br(),
                
                h4("Il Laboratorio di Microbiologia degli Alimenti esegue determinazioni analitiche 
                   su alimenti e matrici ambientali delle filiere dei prodotti di origine animale e 
                   vegetale, inclusi gli alimenti destinati all’alimentazione animale. Il Laboratorio 
                   e referente per i piani di controllo ufficiali eseguiti dalle Autorità Sanitarie 
                   competenti. Si propone, inoltre, quale partner erogatore di servizi per gli operatori 
                   del settore agro-alimentare (autocontrollo) e per la ricerca applicata al settore 
                   della produzione di alimenti."), 
                
                hr(), br(), br(), br(), br(),
                
                plotlyOutput("MAp1")
              )), 
       column(6, 
              wellPanel(
                tableOutput("MAt1"), 
                hr(),br(),
                selectInput(inputId = "microalim",
                            label = "Seleziona la prova",
                            choices = c( "", levels(factor(alimenti$prova)))),
                br(), hr(),
                plotOutput("MAp2")
                
              ))
     ), 
     fluidRow(
       wellPanel(
         dataTableOutput("SumMA"), hr(), br(), 
         dataTableOutput("madrill")
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
 
 #Laboratorio biologia Molecolare-----
 tabPanel(
   title = "Laboratorio di biologia molecolare e TSE",
   value = "biologmolec",
   fluidPage(
     fluidRow(
       column(6, 
              wellPanel(
                h2("Laboratorio di biologia molecolare e TSE"), br(),
                
                h4(""), 
                
                hr(), br(), br(), br(), br(),
                
                plotlyOutput("bmp1")
              )), 
       column(6, 
              wellPanel(
                tableOutput("bmt1"), 
                hr(),br(),
                selectInput(inputId = "biomolec",
                            label = "Seleziona la prova",
                            choices = c( "", levels(factor(biomol$prova)))),
                br(), hr(),
                plotOutput("bmp2")
                
              ))
     ), 
     fluidRow(
       wellPanel(
         dataTableOutput("Sumbm"), hr(), br(), 
         dataTableOutput("bmdrill")
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
 
 # #laboratorio qualità----
 # 
 # tabPanel(
 #   title = "Qualità",
 #   value = "qualità",
 #   # h3(uiOutput("aggconf")),
 #   # fluidRow(
 #   #   br(),
 #   #   downloadButton("downloadData", "Scarica i dati"),
 #   #   DTOutput("conferimenti")
 #   # )
 # ),
 #   
 #   
 #   # Attività Ufficiale
 #   
 #   tabPanel(
 #     title = "Attività Ufficiale",
 #     value = "attuff"
 #     # h3(uiOutput("aggconf")),
 #     # fluidRow(
 #     #   br(),
 #     #   downloadButton("downloadData", "Scarica i dati"),
 #     #   DTOutput("conferimenti")
 #     # )
 #   ),
 #   
 #   
 #   
 # 
 # #laboratorio autocontrollo-----
 # tabPanel(
 #   title = "Attività per Autocontrollo",
 #   value = "autocontrollo"
 #   # h3(uiOutput("aggconf")),
 #   # fluidRow(
 #   #   br(),
 #   #   downloadButton("downloadData", "Scarica i dati"),
 #   #   DTOutput("conferimenti")
 #   # )
 # ),
 # 
 # 
 # #Attività di ricerca-----
 # tabPanel(
 #   title = "Attività  di ricerca",
 #   value = "autocontrollo"
 # ),

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

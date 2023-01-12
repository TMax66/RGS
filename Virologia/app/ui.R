ui <- navbarPage(
  title = "IZSLER Reparto di Virologia",
 # theme = bslib::bs_theme(4),
 
 #home----
 
 tabPanel(
   title = "Home",
   value = "home",
   
    img(src="staff.jpg", style="width: 1600px ; height: 200; align = center"), 
   hr(), br(),
   fluidPage(
    fluidRow( 
     column(6, 
            wellPanel(
              h2("Reparto di Virologia"), br(),
              
              h4("Il reparto eroga prestazioni diagnostiche, di supporto all’attività IZSLER, ai servizi veterinari regionali e territoriali (ATS e AUSL) ed agli utenti privati, fornendo servizi specialistici multidisciplinari dedicati alla diagnostica di malattie ad eziologia virale degli animali domestici e selvatici.
L’attività svolta prevede principalmente l’esecuzione di analisi su materiali patologici e su sangue di animali, atte a ricercare rispettivamente i virus o loro componenti in modo diretto oppure gli anticorpi da essi indotti con metodi indiretti sierologici.")
            )), 
     column (6, 
               column(6,
              # img(src="SEZ-MO.gif", style="align = center")
               ), 
               column(6, 
              h3("Direttore: Dr. Antonio Lavazza"),
             )
             )
    ), 
    
    # fluidRow(
    #   column(4, 
    #   wellPanel(
    #     h3("SANITA' ANIMALE"), 
    #     tableOutput("thomeSA")
    #   )), 
    #   column(4, 
    #   wellPanel(
    #     h3("ALIMENTI UOMO"), 
    #     tableOutput("thomeAU")
    #   )), 
    #   column(4,
    #    wellPanel(
    #      h3("ALIMENTI ZOOTECNICI"), 
    #      tableOutput("thomeAZ")
    #    ))
    #          ), 
    fluidRow(
      uiOutput("aggconf")
    )

      )
    ),
 
 
 
 
#Accettazione----

 
 tabPanel(
   title = "Accettazione", 
   fluidPage( 
   
   fluidRow( 
            wellPanel(
              radioButtons(
                inputId = "settore",
                label = "Settore d'intervento",
                choices = c( "Tutti", levels(factor(conf$settore))), 
                inline = TRUE) , br(),
              
              plotlyOutput("plotacc"), br(),
            )
  
 ))),
 
#Laboratorio di Assistenza ai Piani di Risanamento, Contenimento ed Eradicazione----
 
tabPanel(
   title = "Laboratorio di Assistenza ai Piani di Risanamento, Contenimento ed Eradicazione",
   #value = "labsiero",
   fluidPage(
     fluidRow(
       column(6, 
       wellPanel(
         h2("Laboratorio di Assistenza ai Piani di Risanamento, Contenimento ed Eradicazione"), br(),
         
         h4("Analisi finalizzate all’esecuzione di piani di controllo e sorveglianza nazionali e regionali (IBR, Malattia di Aujeszky, Blue Tongue, Influenza Aviaria, Pestivirus, West Nile Disease).
             Supporto diagnostico ai Centri di miglioramento genetico e Centri tori.
             Supporto tecnico e normativo al Ministero della Salute per malattie oggetto di piani di risanamento, delle api e dei selvatici; per le malattie e il benessere dei lagomorfi domestici e selvatici."), 
         
         hr(), br(), br(), br(), br(),
         
        # selectInput("prove", "Seleziona la prova", choices = c("", levels(factor(siero$prova)))), 
         
         #plotlyOutput("Sp1")
        
       ))#, 
   #    column(6, 
   #           wellPanel( 
   #          tableOutput("St1"), 
   #          hr(), br(),
   #          plotOutput("Sp2")
   #           )
   #           
   #           )
   #   ), 
   #   fluidRow(
   #     wellPanel(
   #       dataTableOutput("SumSiero"), hr(), br(),
   #       dataTableOutput("sdrill")
   #     )
   #   )
   # )
  
 ))),
 
 #Laboratorio di Virologia e Sierologia Specializzata e Microscopia Elettronica----
 
 tabPanel(
   title = "Laboratorio di Virologia e Sierologia Specializzata e Microscopia Elettronica",
  # value = "labdiagnostica",
   fluidPage(
     fluidRow(
       img(src="me.tiff", style="align = center"), 
       column(6, 
              wellPanel(
                h2("Laboratorio di Virologia e Sierologia Specializzata e Microscopia Elettronica"), br(),
                
                h4("Unità diagnostica virologica: esami virologici e sierologici per malattie virali dei mammiferi domestici, di interesse zootecnico, selvatici e d’affezione con tecniche di virologia classica e molecolare (isolamento, metodi immunoenzimatici e biomolecolari) e/o mediante tecniche sierologiche (ELISA, HI, SN, IPMA, CIA test, …). Diagnosi, tipizzazione, caratterizzazione genomica e analisi filogenetiche di virus aviari.
                   Unità di malattie emergenti e trasmesse da vettori: diagnostica di malattie emergenti a carattere zoonosico e trasmesse da vettori ed indagini entomologiche. Sviluppo e, messa a punto e validazione di test immunoenzimatici virologici e sierologici.
                  Unità di Microscopia Elettronica: diagnostica specialistica al microscopio elettronico e supporto analitico per altre strutture di ricerca a carattere locale (Ospedali e Università) e nazionale (altri IIZZSS)."), 
                
                hr(), br(), br(), br(), br(),
                
               # plotlyOutput("Dp1")
              ))#, 
   #     column(6, 
   #            wellPanel(
   #              tableOutput("Dt1"), 
   #              hr(),br(),
   #              selectInput(inputId = "diagnos",
   #                          label = "Seleziona la prova",
   #                          choices = c( "", levels(factor(diagnostica$prova)))),
   #              br(), hr(),
   #              plotOutput("Dp2")
   #              
   #            ))
   #   ), 
   #   fluidRow(
   #     wellPanel(
   #       dataTableOutput("SumDiagn"), hr(), br(), 
   #       dataTableOutput("ddrill")
   #     )
   #   )
   # )
   
   
   
   
   
   
   # h3(uiOutput("aggconf")),
   # fluidRow(
   #   br(),
   #   downloadButton("downloadData", "Scarica i dati"),
   #   DTOutput("conferimenti")
   # )
 ))),
 
 ##Laboratorio di Proteomica e Diagnostica TSE----
 tabPanel(
   title = "Laboratorio di Proteomica e Diagnostica TSE",
  # value = "labalim",
   fluidPage(
     fluidRow(
       column(6, 
              wellPanel(
                h2("Laboratorio di Proteomica e Diagnostica TSE"), br(),
                
                h4("Unità di Immunobiochimica delle proteine e malattie virali dei lagomorfi: attività di servizio e consulenza per: I) purificazione e analisi proteine (anticorpi, virus e proteine virali); II) produzione di reagenti d’uso in metodi immunologici, anticorpi marcati e non, kits diagnostici. Diagnostica e sviluppo di metodiche immunologiche e di biologia molecolare delle malattie virali dei lagomorfi (RHDV, EBHSV, Myxomavirus).
                   Unità di diagnostica TSE: sorveglianza attiva per le Encefalopatie Spongiformi Trasmissibili (BSE nei bovini e Scrapie negli ovi-caprini) mediante diagnosi e supporto alle Autorità competenti."), 
                
                hr(), br(), br(), br(), br(),
                
               # plotlyOutput("MAp1")
              ))#, 
   #     column(6, 
   #            wellPanel(
   #              tableOutput("MAt1"), 
   #              hr(),br(),
   #              selectInput(inputId = "microalim",
   #                          label = "Seleziona la prova",
   #                          choices = c( "", levels(factor(alimenti$prova)))),
   #              br(), hr(),
   #              plotOutput("MAp2")
   #              
   #            ))
   #   ), 
   #   fluidRow(
   #     wellPanel(
   #       dataTableOutput("SumMA"), hr(), br(), 
   #       dataTableOutput("madrill")
   #     )
   #   )
   # )
   # 
   
   
   
   
   
   
   
   
   # h3(uiOutput("aggconf")),
   # fluidRow(
   #   br(),
   #   downloadButton("downloadData", "Scarica i dati"),
   #   DTOutput("conferimenti")
   # )
 ))),
 
 
 ##laboratorio qualità----
 
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
   
   
   
 
 #laboratorio autocontrollo 
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
   
   h4("L’attività di ricerca si estrinseca spesso nello sviluppo, messa a punto e validazione di metodi diagnostici anche mediante produzione di reagenti innovativi e nella caratterizzazione di ceppi virali identificati sul territorio nazionale sia a livello molecolare che di profilo antigenico mediante anticorpi monoclonali.
Ampi e vari sono i settori zootecnici interessati da tale attività di ricerca, che è sia correlata alla tipicità dei centri di referenza, sia frequentemente ‘indotta’ da eventi morbosi contingenti.
Le principali linee di ricerca riguardano virus trasmessi da vettori (flavivirus, bynyavirus, flebovirus), pestivirus e herpesvirus bovini e suini, influenza aviare e suina, coronavirus animali, reovirus dei mammiferi, enterovirus suini, virus aviari (LTI, BI, IBD, AI), virus dei conigli (RHD, Myxomatosi), malattie virali delle api, virus dei pipistrelli e di altri animali selvatici (uccelli, ungulati e lagomorfi).")
 ),

#Tabella pivot----
tabPanel(
  title = "Tabelle Pivot",
  fluidPage(
    fluidRow(
      column(6,div(style="height:10px"),rpivotTableOutput("pivot") ))
  ))

# )),
#  
#   tabPanel(
#     title = "Conferimenti",
#     value = "conf",
#     #h3(uiOutput("aggconf")),
#     fluidRow(
#       br(),
#       downloadButton("downloadData", "Scarica i dati"),
#       DTOutput("conferimenti")
#     )
#   ),
#   tabPanel(
#     title = "Prove",
#     value = "prove",
#     #h3(uiOutput("aggprove")),
#     fluidRow(
#       br(),
#       downloadButton("downloadData2", "Scarica i dati"),
#       DTOutput("esami")
#     )
#   )
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
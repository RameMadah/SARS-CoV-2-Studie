library(graphics)
library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(base64enc)
library(scales)

server <- function(input, output, session) {
  
#Summarize Data and then Plot 
################################ Erstes Plot
 
  
data <- reactive({
req(input$sel)
df <- germany_vaccinations_k %>%  group_by(code) %>% summarise( output = get(input$sel))
})



datao <- reactive({
req(input$sel)
df <- germany_vaccinations_k %>%  group_by(code) %>% summarise( output = get(input$selo))

})

########################## Zweites Plot

nf <- reactive({
  filter(germany_vaccinations_timeseries_v2, between(date, input$date[1], input$date[2]))  
}) 

data2 <- reactive({
req(input$sel2)
lf <- nf() %>%  group_by(date ) %>% summarise( output = get(input$sel2))
})

datao2 <- reactive({
req(input$sel22)
lf <- nf() %>%  group_by(date ) %>% summarise( output = get(input$sel22))
})



########################## Drittes Plot
kf <- reactive({
  filter(new3, between(date, input$date2[1], input$date2[2]))  
})


data3 <- reactive({
req(input$sel3)
df <- kf() %>%  group_by(date) %>% summarise( output = get(input$sel3))
})

datao3 <- reactive({
req(input$sel3)
df <- kf() %>%  group_by(date) %>% summarise( output = get(input$sel33))
})

####################################################################################################

output$plotselb <-  renderText({  
paste("<B>  histogramm (1) : &nbsp;&nbsp;&nbsp;&nbsp;     </B>",
"<B>  min = </B>", min(germany_vaccinations_k[input$sel])," &nbsp;&nbsp;   ",
"<B>      max = </B>", max(germany_vaccinations_k[input$sel])," &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(germany_vaccinations_k[[input$sel]]))," &nbsp;&nbsp;  ",
"<B>     median = </B>", median(germany_vaccinations_k[[input$sel]]))

})

output$plotselb1 <-  renderText({  
paste("<B>    histogramm (2) :  &nbsp;&nbsp;&nbsp;&nbsp; </B>",
"<B> min = </B>", min(germany_vaccinations_k[input$selo])," &nbsp;&nbsp;   ",
"<B>      max = </B>", max(germany_vaccinations_k[input$selo])," &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(germany_vaccinations_k[[input$selo]]))," &nbsp;&nbsp;  ",
"<B>     median = </B>", median(germany_vaccinations_k[[input$selo]]))

})

output$plotselbb <-  renderText({  
paste("<B>    histogramm (1) :  &nbsp;&nbsp;&nbsp;&nbsp; </B>",
"<B> min = </B>", min(germany_vaccinations_timeseries_v2[input$sel2])," &nbsp;&nbsp;   ",
"<B>      max = </B>", max(germany_vaccinations_timeseries_v2[input$sel2])," &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(germany_vaccinations_timeseries_v2[[input$sel2]]))," &nbsp;&nbsp;  ",
"<B>     median = </B>", median(germany_vaccinations_timeseries_v2[[input$sel2]]))
})

output$plotselbb2 <-  renderText({  
paste("<B>    histogramm (2) :  &nbsp;&nbsp;&nbsp;&nbsp; </B>",
"<B> min = </B>", min(germany_vaccinations_timeseries_v2[input$sel22])," &nbsp;&nbsp;   ",
"<B>      max = </B>", max(germany_vaccinations_timeseries_v2[input$sel22])," &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(germany_vaccinations_timeseries_v2[[input$sel22]]))," &nbsp;&nbsp;  ",
"<B>     median = </B>", median(germany_vaccinations_timeseries_v2[[input$sel22]]))
})


output$plotselbc <-  renderText({  
paste("<B>    histogramm (1) :  &nbsp;&nbsp;&nbsp;&nbsp; </B>",
"<B> min = </B>", min(new3[input$sel3]),"€ &nbsp;&nbsp;   ",
"<B>      max = </B>", max(new3[input$sel3]),"€ &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(new3[[input$sel3]])),"€ &nbsp;&nbsp;  ",
"<B>     median = </B>", round(median(new3[[input$sel3]])),"€ ")
})

output$plotselbc2 <-  renderText({  
paste("<B>    histogramm (2) :  &nbsp;&nbsp;&nbsp;&nbsp; </B>",
"<B> min = </B>", min(new3[input$sel33]),"€ &nbsp;&nbsp;   ",
"<B>      max = </B>", max(new3[input$sel33]),"€ &nbsp;&nbsp;  ",
"<B>     mean = </B>", round(mean(new3[[input$sel33]])),"€ &nbsp;&nbsp;  ",
"<B>     median = </B>", round(median(new3[[input$sel33]])),"€ ")
})

###########################################################################################################




#PLOT1
output$ploto <- renderPlot({  
ggplot()+
geom_bar(data = data(), aes(y = output, x = code ), stat = "sum", alpha = input$alpha_sel) + 
geom_bar(data = datao(), aes(y = output, x = code), stat = "sum", alpha = input$alpha_sel2, fill = "lightblue")+ 
theme(legend.position = "right",axis.text.x = element_text(angle = 90 ,face = "bold", color = "black") ) +  labs(ho = "Code des Bundeslandes" ,y= "Anzahl der Geimpften", x = "Code der Bundesländer")+ 
    scale_y_continuous( labels = number_format(), breaks = pretty_breaks(7))

})


#Plot 2
output$plot2 <- renderPlot({  

ggplot() +
geom_bar(data = data2(), aes(y = output, x = date), stat = "sum",alpha = input$alpha_selo)+
geom_bar(data = datao2(), aes( y = output,x = date), stat = "sum",alpha = input$alpha_selo2 , fill = "lightblue")+
theme(legend.position = "none" ) +  labs(y= "Anzahl der Dosen", x = "Datum")+ 
    scale_y_continuous( labels = number_format(), breaks = pretty_breaks(7))

})

#Plot 3
output$plot3 <- renderPlot({  
g <- ggplot(  ) 
g + geom_bar( data = data3(), aes( y = output   ,x = date ), stat = "sum",alpha = input$alpha_selom, fill = "darkgreen") +
geom_bar( data = datao3(), aes( y = output   ,x = date ), stat = "sum",alpha = input$alpha_selom2, fill = "green")+
   labs(y= "Kumulative Umsatz in Euro ", x = "Zeitspanne") + theme( legend.position = "none"  )+ 
  scale_y_continuous( labels = number_format(), breaks = pretty_breaks(7))

})
}



######################################################################  +
######################################################################
ui <- basicPage(   

  


fluidRow(column(3," "),column(6,h1(tags$strong("SARS-CoV-2-Pandemie eine interaktives visualisiertes App ",fill = "Blue")))),
fluidRow(column(3," "),column(6,h6(tags$strong("Web-App / Visualisierung ist Optimal auf PC")))),
column(3," "),column(9,tags$h3("Die Geimpften aufgeschlüsselt nach Bundesland")),
p("Zunächst einmal, gibt es drei Grafen, die verschiedene Studien und Sortierungen darstellen.
Durch das Auswählen von der verfügbaren Möglichkeiten wird deutlich, 
die Visualisierung des Grafen geändert werden.."),

tags$ul(
tags$li("Summe der verabreichten Dosen,
Erstgeimpften und vollständig Geimpften, aufgeschlüsselt nach Bundesland."),

tags$li("Summe die Anzahl der als Erstimpfungen, zwetimpfungen,drittimpfungen der
verabreichten Dosen der Impfstoffe von BioNTech/Pfizer, Moderna 
AstraZeneca und Johnson & Johnson an."),

tags$li("kumilative Umsätze der
verabreichten Dosen der Impfstoffe von BioNTech/Pfizer, Moderna 
AstraZeneca und Johnson & Johnson an.")),
br(),

hr(),
###################################
#INPUT 1
fluidRow(
column(3,selectInput(inputId = "sel",  label = "Möglichkeit für Histogramm (1) auswählen",
list("vaccinationsTotal","peopleFirstTotal","peopleFullTotal","peopleBoosterTotal"))),

column(6,h6("Vergleichen Sie Ergebnisse aus Histogramm (1) mit der Ergebnisse vom Histogramm (2)")),
#  mainPanel( ) tags$img( src = "App/www/H1-Kopie.png", width= "179px", height = "70px", alt ="img")
column(3,selectInput(inputId = "selo",  label = "Möglichkeitfür Histogramm (2)  auswählen",
list("peopleFirstTotal","peopleFullTotal","peopleBoosterTotal"))),),


fluidRow(
  column(3,""),
  
  column(6,""),
  #  mainPanel( ) tags$img( src = "App/www/H1-Kopie.png", width= "179px", height = "70px", alt ="img")
  column(3,"")),


br(),
fluidRow(
column(3,sliderInput("alpha_sel", "Erste Alpha auswählen", min = .04, max = 1, value = 0.5)),
column(6,""),
column(3,sliderInput("alpha_sel2", "Zweite Alpha auswählen", min = .04, max = 1, value = 0.5)),),
br(),
#OUTput (1)
fluidRow(column(3,img(src = "H1-Kopie.png", width= "75%", height = "75%")),column(9,tags$blockquote("Die Geimpften aufgeschlüsselt nach Bundesland"))),
br(),
plotOutput("ploto"),
br(),
column(2," "),htmlOutput("plotselb"),
column(2," "),htmlOutput("plotselb1"),
br(),
br(),
hr(),
###################################
###################################

fluidRow(column(3," "),column(9,tags$h3("Die Impfdosen aufgeschlüsselt nach Datum")),),
tags$ul(
tags$li("Dosen_erst_kumulativ gibt die Anzahl der als Erstimpfungen 
verabreichten Dosen der Impfstoffe von BioNTech/Pfizer, Moderna und 
AstraZeneca an."),
tags$li( "Dosen_zweit_kumulativ gibt die
Anzahl der als Zweitimpfungen verarbeichten Dosen der Impfstoffe von BioNTech/Pfizer, 
Moderna und AstraZeneca sowie die verarbreichten Dosen von Johnson & Johnson an."),
tags$li("Dosen_dritt_kumulativ gibt die Anzahl der als Auffrischungsimpfungen verabreichten 
Dosen der Impfstoffe von BioNTech/Pfizer, Moderna und AstraZeneca an."),
br(),
tags$li("Die täglichen Datenupdates der Impfungen basieren nun auf den vom Robert 
Koch-Institut bereitgestellten Datensätzen.")),
hr(),


#INPUT (2)
fluidRow(column(3,selectInput(inputId = "sel2",  label = "Möglichkeit für Histogramm (1) auswählen",
       list("dosen_kumulativ","dosen_biontech_kumulativ","dosen_biontech_erst_kumulativ"
,"dosen_biontech_zweit_kumulativ","dosen_biontech_dritt_kumulativ",
"dosen_moderna_kumulativ","dosen_moderna_erst_kumulativ","dosen_moderna_zweit_kumulativ",
"dosen_moderna_dritt_kumulativ","dosen_astra_kumulativ","dosen_astra_erst_kumulativ",
"dosen_astra_zweit_kumulativ","dosen_astra_dritt_kumulativ","dosen_johnson_kumulativ",
"dosen_johnson_erst_kumulativ","dosen_johnson_zweit_kumulativ",
"dosen_johnson_dritt_kumulativ","dosen_erst_kumulativ","dosen_zweit_kumulativ",
"dosen_dritt_kumulativ","dosen_differenz_zum_vortag","dosen_erst_differenz_zum_vortag",
" dosen_zweit_differenz_zum_vortag","dosen_dritt_differenz_zum_vortag","dosen_vollstaendig_differenz_zum_vortag",
"dosen_erst_unvollstaendig_differenz_zum_vortag","personen_erst_kumulativ","personen_voll_kumulativ",
"personen_auffrisch_kumulativ","impf_quote_erst","impf_quote_voll"))),

column(6,h6("Vergleichen Sie Ergebnisse aus Histogramm (1) mit der Ergebnisse vom Histogramm (2)")),
column(3,selectInput(inputId = "sel22",  label = "Möglichkeitfür Histogramm (2)  auswählen",
       list("dosen_biontech_kumulativ","dosen_kumulativ","dosen_biontech_erst_kumulativ"
            ,"dosen_biontech_zweit_kumulativ","dosen_biontech_dritt_kumulativ",
            "dosen_moderna_kumulativ","dosen_moderna_erst_kumulativ","dosen_moderna_zweit_kumulativ",
            "dosen_moderna_dritt_kumulativ","dosen_astra_kumulativ","dosen_astra_erst_kumulativ",
            "dosen_astra_zweit_kumulativ","dosen_astra_dritt_kumulativ","dosen_johnson_kumulativ",
            "dosen_johnson_erst_kumulativ","dosen_johnson_zweit_kumulativ",
            "dosen_johnson_dritt_kumulativ","dosen_erst_kumulativ","dosen_zweit_kumulativ",
            "dosen_dritt_kumulativ","dosen_differenz_zum_vortag","dosen_erst_differenz_zum_vortag",
            " dosen_zweit_differenz_zum_vortag","dosen_dritt_differenz_zum_vortag","dosen_vollstaendig_differenz_zum_vortag",
            "dosen_erst_unvollstaendig_differenz_zum_vortag","personen_erst_kumulativ","personen_voll_kumulativ",
            "personen_auffrisch_kumulativ","impf_quote_erst","impf_quote_voll"))),),
br(),
fluidRow(column(3,sliderInput("alpha_selo", "Erste Alpha auswählen", min = .04, max = 1, value = 0.5)),
         
         
         #Date Range
         column(6,align="center",dateRangeInput("date", "Die Zeitspanne auswählen:",
                                                start  = "2020-12-29",
                                                end    = "2021-12-08",
                                                min    = "2020-12-28",
                                                max    = "2021-12-08",
                                                format = "mm/dd/yy",
                                                separator = " - " )),
         
column(3,sliderInput("alpha_selo2", "Zweite Alpha auswählen", min = .04, max = 1, value = 0.5))),
br(),

fluidRow(column(3,img(src = "H1-Kopie.png", width= "75%", height = "75%")),column(9,tags$blockquote("Die Impfdosen aufgeschlüsselt nach Datum"))),
br(),
plotOutput("plot2"),

br(),
fluidRow(column(2," "),htmlOutput("plotselbb"),
column(2," "),htmlOutput("plotselbb2")),
br(),
hr(),
br(),
br(),
#################################
fluidRow(column(3," "),column(9,tags$h3("Der kumilative Umsatz der Firmen aufgeschlüsselt nach Datum"))),
tags$ul(
  tags$li(" Hier wurde die kumilative Umsatz jeder Impfstoff-Marka dargestellt, man kann hier vergleichen zwichen 
         die Folgende Impfstoffmarken biontech, morderna, astra und johnson  ")),
hr(),
br(),
br(),
br(),
#Input(3)
fluidRow(column(3,selectInput(inputId = "sel3",  label = "Möglichkeit  für Histogramm (1) auswählen",
list("biontech","morderna","astra","johnson"))),

column(6,h6(" Vergleichen Sie Ergebnisse aus Histogramm (1) mit der Ergebnisse vom Histogramm (2)")),

column(3,selectInput(inputId = "sel33",  label = "Möglichkeit  für Histogramm (2) auswählen",
list("morderna","biontech","astra","johnson")))),
#Date Range
fluidRow(column(3," "),column(9,)),
br(),
fluidRow(column(3,sliderInput("alpha_selom", "Erste Alpha auswählen", min = .04, max = 1, value = 0.5)),
         
         #Date Range
         column(6,align="center",dateRangeInput("date2", "Die Zeitspanne auswählen:",
                                 start  = "2020-12-29",
                                 end    = "2021-12-08",
                                 min    = "2020-12-28",
                                 max    = "2021-12-08",
                                 format = "mm/dd/yy",
                                 separator = " - " )),
column(3,sliderInput("alpha_selom2", "Zweite Alpha auswählen", min = .04, max = 1, value = 0.5))),
br(),
#OUTput (3)
fluidRow(column(3,img(src = "H3 - Kopie.png", width= "75%", height = "75%")),column(9,tags$blockquote("Der kumilative Umsatz der Firmen aufgeschlüsselt nach Datum")))
,br(),
plotOutput("plot3"),
br(),
fluidRow(column(2 ,""),htmlOutput("plotselbc"),
column(2 ,""),htmlOutput("plotselbc2")),
br(),
br(),
hr(),
br(),
##################################

tags$ul(
h3("Quellen:"),
tags$li("1 #Die Geimpften aufgeschlüsselt nach Bundesland /", tags$a(href="https://impfdashboard.de/",
"Stand: 16.12.2021 (Impfungen) und die Quelle impfdashboard.de, RKI, BMG. ( hier klicken )")
),

tags$li( "2 #Die Impfdosen aufgeschlüsselt nach Datum /",
tags$a(href="https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland", "basieren Robert Koch-Institut ( hier klicken )")),


tags$li( "3 #Der kumilative Umsatz der Firmen aufgeschlüsselt nach Datum /",
tags$a(href="https://github.com/robert-koch-institut/COVID-19-Impfungen_in_Deutschland", "basieren Robert Koch-Institut ( hier klicken )")),

tags$li( "3 #Der kumilative Umsatz der Firmen aufgeschlüsselt nach Datum /",
tags$a(href="https://www.welt.de/politik/ausland/article222810856/EU-Von-1-78-bis-14-70-Euro-so-viel-kosten-die-Impfstoffe.html",
  "Von 1,78 bis 14,70 Euro – so viel kosten die Impfstoffe Von Christoph B. Schiltz Veröffentlicht am 18.12.2020( hier klicken )"))),
hr(),
br()
)
shinyApp(ui = ui, server = server)

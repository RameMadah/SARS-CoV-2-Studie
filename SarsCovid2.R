
library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)

server <- function(input, output, session) {

#Summarize Data and then Plot 
data <- reactive({
req(input$sel)
df <- germany_vaccinations_k %>%  group_by(code) %>% summarise( output = get(input$sel))
print(df)
})



dataop <- reactive({
 ## req(input$sel)
  set.seed(seed = 90000)
  selom <-  germany_vaccinations_k[["code"]]
  group <- c(germany_vaccinations_k$peopleFirstTotal ,germany_vaccinations_k$vaccinationsTotal )
  df <- data.frame(selom, group = group)
})


data2 <- reactive({
req(input$sel2)
lf <- germany_vaccinations_timeseries_v2 %>%  group_by(date) %>% summarise( output = get(input$sel2))
})

data3 <- reactive({
req(input$sel3)
df <- new3 %>%  group_by(date) %>% summarise( output = get(input$sel3))
})


####################################################################################################

output$plotselb <-  renderText({  
  paste("<B> min = </B>", min(germany_vaccinations_k[input$sel])," &nbsp;&nbsp;   ",
        "<B>      max = </B>", max(germany_vaccinations_k[input$sel])," &nbsp;&nbsp;  ",
        "<B>     mean = </B>", round(mean(germany_vaccinations_k[[input$sel]]))," &nbsp;&nbsp;  ",
        "<B>     median = </B>", median(germany_vaccinations_k[[input$sel]]))
  
})

output$plotselbb <-  renderText({  
  paste("<B> min = </B>", min(germany_vaccinations_timeseries_v2[input$sel2])," &nbsp;&nbsp;   ",
        "<B>      max = </B>", max(germany_vaccinations_timeseries_v2[input$sel2])," &nbsp;&nbsp;  ",
        "<B>     mean = </B>", round(mean(germany_vaccinations_timeseries_v2[[input$sel2]]))," &nbsp;&nbsp;  ",
        "<B>     median = </B>", median(germany_vaccinations_timeseries_v2[[input$sel2]]))
  
})

output$plotselbc <-  renderText({  
  paste("<B> min = </B>", min(new3[input$sel3]),"€ &nbsp;&nbsp;   ",
        "<B>      max = </B>", max(new3[input$sel3]),"€ &nbsp;&nbsp;  ",
        "<B>     mean = </B>", mean(new3[[input$sel3]]),"€ &nbsp;&nbsp;  ",
        "<B>     median = </B>", median(new3[[input$sel3]]),"€ ")
})

###########################################################################################################



#Plot 1
output$plot <- renderPlot({  
g <- ggplot( data(), aes( y = output   ,x = code ,ho = factor(code) )  ) 
g + geom_bar( stat = "sum",fill =
c("brown1", "red", "green3", "darkgoldenrod4","deepskyblue3",
"brown2", "coral", "chartreuse4", "hotpink2","orange",
"darkolivegreen", "darkgoldenrod4", "Darkblue", "hotpink2","darkgreen",
"blue", "brown"),size=2, alpha=0.2 )+
theme(legend.position = "left" ) +  labs(ho = "Code des Bundeslandes" ,y= "Anzahl der Geimpften", x = "Code der Bundesländer")
})








output$ploto <- renderPlot({  
   f <- ggplot( dataop(), aes(  x = selom  ,  fill = group) ) +
    geom_histogram(alpha = 0.2, stat = "count" )
 f+ylim(0, 50)
  
  
   })
#Plot 1.5
#output$ploto <- renderPlot({  
#g <- ggplot( data(), aes( y = output   ,x = code ,ho = factor(code) )  ) 
#g + geom_bar( stat = "sum",fill =
#                c("brown1", "red", "green3", "darkgoldenrod4","deepskyblue3",
#                  "brown2", "coral", "chartreuse4", "hotpink2","orange",
#                  "darkolivegreen", "darkgoldenrod4", "Darkblue", "hotpink2","darkgreen",
#                  "blue", "brown"),size=2, alpha=0.4 )+
#  theme(legend.position = "left",  panel.grid.minor = element_blank(), 
#        panel.grid.major = element_blank(),
#        plot.background = element_blank() )+  labs(ho = "Code des Bundeslandes" ,y= "Anzahl der Geimpften", x = "Code der Bundesländer")+
#  geom_density(stat = "sum")
##,rect = element_rect(fill = "transparent"),panel.border = element_blank()
    ##,plot.margin = unit(c(1,1,20,1), "cm")

####,axis.title.y=element_text(angle=90, vjust=-0.5),axis.title.x=element_text(vjust=-2),
#plot.title=element_text(size=15, vjust=3),plot.margin = unit(c(1,1,80,1), "cm")
##
#
#})

#Plot 2
output$plot2 <- renderPlot({  

g <- ggplot( data2(), aes( y = output   ,x = date  ),  ) 
g + geom_bar( stat = "sum")+
  theme(legend.position = "left" ) +  labs(y= "Anzahl der Dosen", x = "Datum")

})

#Plot 3
output$plot3 <- renderPlot({  

g <- ggplot( data3(), aes( y = output   ,x = date ),  ) 
g + geom_bar( stat = "sum", fill = "darkgreen") +
theme(legend.position = "left" ) +  labs(y= "Kumulative Umsatz in Euro ", x = "Zeitspanne")

})
}



######################################################################
######################################################################
ui <- basicPage(   

  
  

h1("SARS-CoV-2-Pandemie eine interaktive Visualisiertes App "),
hr(),
column(3," "),column(9,tags$h3("Die Geimpften aufgeschlüsselt nach Bundesland")),
p("Zunächst einmal, gibt es zwei Grafen, die verschiedene Studien und Sortierungen darstellen.
Durch das Auswählen von der verfügbaren Möglichkeiten wird deutlich, 
die Visualisierung des Grafen geändert werden.."),

tags$ul(
tags$li("Summe der verabreichten Dosen,
Erstgeimpften und vollständig Geimpften, aufgeschlüsselt nach Bundesland."),

tags$li("Summe die Anzahl der als Erstimpfungen, zwetimpfungen,drittimpfungen der
verabreichten Dosen der Impfstoffe von BioNTech/Pfizer, Moderna 
AstraZeneca und Johnson & Johnson an.")),


hr(),
###################################
#INPUT
selectInput(inputId = "sel",  label = "Möglichkeit auswählen",
list("vaccinationsTotal","peopleFirstTotal","peopleFullTotal","peopleBoosterTotal")),

#selectInput(inputId = "selo",  label = "Möglichkeit auswählen",
     #       list("vaccinationsTotal","peopleFirstTotal","peopleFullTotal","peopleBoosterTotal")),
#OUTput
column(3," "),column(9,tags$blockquote("Die Geimpften aufgeschlüsselt nach Bundesland")),


plotOutput("plot"),
#plotOutput("ploto"),
br(),
br(),
br(),
br(),
br(),
column(3," "),htmlOutput("plotselb"),
br(),
br(),
hr(),

###################################

column(3," "),column(9,tags$h3("Die Impfdosen aufgeschlüsselt nach Datum")),
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


#INPUT
selectInput(inputId = "sel2",  label = "Möglichkeit auswählen",

#Möglichkeiten
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
"personen_auffrisch_kumulativ","impf_quote_erst","impf_quote_voll")),

#OUTput
column(3," "),column(9,tags$blockquote("Die Impfdosen aufgeschlüsselt nach Datum")),
plotOutput("plot2"),
br(),
br(),
br(),
br(),
column(3," "),htmlOutput("plotselbb"),
hr(),
br(),
br(),
#################################


selectInput(inputId = "sel3",  label = "Möglichkeit auswählen",
      list("biontech","morderna","astra","johnson")),
#OUTput
column(3," "),column(9,tags$blockquote("Der kumilative Umsatz der Firmen aufgeschlüsselt nach Datum")),

plotOutput("plot3"),
br(),
br(),
br(),
br(),
br(),
column(3," "),htmlOutput("plotselbc"),
br(),
br(),
hr(),
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
                "Von 1,78 bis 14,70 Euro – so viel kosten die Impfstoffe Von Christoph B. Schiltz Veröffentlicht am 18.12.2020( hier klicken )"))

),
hr(),
br()
)
shinyApp(ui = ui, server = server)

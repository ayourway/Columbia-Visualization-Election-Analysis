library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(readr)
state_data <- read_csv("~/Documents/Spring 2016/Visualization/projectn/R code/finaldraft/EDAV State Data.csv")

variables <- c("Gun Owner Percentage" = "Gun_Owner_Pct",
               "State Population" = "Population",
               "Poverty Rate" = "Poverty_Rate",
               "Gini Index"="Gini",
               "Benefits_Vs_Tax"="Benefits_Vs_Tax",
               "Percent of Caucasian"="Pct_White",
               "Percent of White Evangelical"="Pct_White_Evangelical",
               "Percent of Nonreligious"="Pct_Nonreligious", 
               "Percent of Urban Population"="Pct_Urban",
               "Percent of Obese"="Pct_Obese",
               "Life Expectancy"="Life_Exp",
               "Percent of Union Worker"="Pct_Union",
               #"Right to work"="RTW",
               "Percent of Business Owner"="Pct_Biz_Owner",
               "Smoker Percentage"="Pct_Smoker",
               "% High School or lower"="Pct_Highschoolbelow",
               "% Some College Education"="Pct_Somecollege",
               "% College Graduate"="Pct_College_Graduate",
               "% Post Graduate"="Pct_Post_Graduate",
               "No_money_for_healthcare"="No_money_for_healthcare",
               "No_money_for_food"="No_money_for_food",
               "Democratic Vote Lean"="Democratic_Vote")

shinyUI(fluidPage(
  #Basic framework of webpage
  #titlePanel(title = "Election Analysis"),
  navbarPage("Election Analysis",
        
        tabPanel("Data",tableOutput("str")),
        
        navbarMenu("Report",
                   tabPanel("Barchart Plot",plotlyOutput("hist1",height = 600),
                            plotlyOutput("hist2",height = 600)),
                   tabPanel("Correlation Plot",plotOutput("cor1",height = 600),
                            h5(textOutput("com_c2"))),
                   tabPanel("Scatter Plot", plotlyOutput("scatter3"),
                            h5(textOutput("com_s3")), 
                            plotlyOutput("scatter4"),
                            h5(textOutput("com_s4")),
                            plotlyOutput("scatter5"),
                            h5(textOutput("com_s5")),
                            plotlyOutput("scatter6"),
                            h5(textOutput("com_s6"))),
                   tabPanel("Heatmap", plotlyOutput("heat1",height = 900),
                            h5(textOutput("com_h1")),
                            plotlyOutput("heat2", height = 900),
                            h5(textOutput("com_h2")))
                   
        ),
        tabPanel("Map",div(class="outer",
                           leafletOutput("map",height = 700),
                           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                         draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                         width = 340, height = "auto",
                                         plotlyOutput("small"),
                                         h2("Election Result Map"),
                                         selectInput("vars", "Variable", variables, selected = "Democratic_Vote"),
                                         checkboxInput("BR","Political Map",FALSE)
                           ))),

        tabPanel("Comment", ({
          pageWithSidebar(
            headerPanel('Please leave your feedback about our work'),
            sidebarPanel(
              textInput("comments","Comments Here",""),
              actionButton("save","Save it")
            ),
            mainPanel(
              tableOutput("fileiIn")
            )
          )
        })),
        navbarMenu("More",
                   tabPanel("Background",h2("References"),tableOutput("com_b1")),
                   tabPanel("Team",h2("Team members"),tableOutput("team1"))
      )
    )
  )
)

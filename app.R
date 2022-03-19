# load packages
library(shiny)
library(plotly)
library(tidyr)
library(dplyr)
library(tidyverse)
library(EpiModel)
library(cli)


vaxWidget <- numericInput(
  inputId = 'vaxRate',
  label = "Select Vaccination Percentage",
  value = 0,
  min = 0,
  max = 100,
  step = 1
)

DistancingList <- c("Yes", "No")
distancingWidget <- radioButtons(
  inputId = 'distancing',
  label = 'Is social distancing in effect?',
  choices = DistancingList,
  selected = 'No'
)

conclusion <- "My conclusion based on the above analysis is that mitigation efforts,
especially when used in conjunction, have a significant impact on the spread of infectious
diseases such as COVID-19. Using the values from this cdc study:
https://www.cdc.gov/mmwr/volumes/70/wr/mm7036a3.htm , I found that each mitigation effort
on its own has a noticeable impact. When used together, the effect multiplies and
the spread across the timeframe in question is significantly slowed. The deterministic
model, especially when used with a large population, shows a clear effect of even
minor changes to rates of infection.

While infection rates may seem like trivially small numbers, with minor fluctuations,
this analysis has shown how important they are on a large scale. With models such as
these that build each step of infection off of the last one, these small rate differences
have a compounding effect and make a huge difference overall."
  
  
maskWidget <- radioButtons(
  inputId = 'mask',
  label = 'Is masking in effect?',
  choices = DistancingList,
  selected = 'No'
)
shinyApp(
  ui = fluidPage(h2(strong("Assignment 3")),
                 h3("Kyle Sorstokke"),
                  sidebarLayout(
                  sidebarPanel(
                    vaxWidget,
                    distancingWidget,
                    maskWidget
                  ),
                  mainPanel(
                    plotOutput("plot1"),
                    br(),
                    plotOutput("plot2"),
                    paste0("Using CDC studies, I found that the probability of",
                           "infection has a strong inverse relationship to the vaccination",
                           "rate. I also found that the masked infection rate is 7.7%",
                           "while unmasked is 32.4%. Using these values, as well as distancing",
                           "to modify the number of exposures, users can modify the visualization."),
                    paste0(conclusion)
                  )
                )
  ),
  server = function(input, output) {
    
    param2 <- param.icm(inf.prob = 0.2, act.rate = 0.25)
    init2 <- init.icm(s.num = 500, i.num = 1)
    control2 <- control.icm(type = "SI", nsims = 10, nsteps = 300)
    mod2 <- icm(param2, init2, control2)
    output$plot2 <- renderPlot({
      plot(mod2)
    })
    
      output$plot1 <- renderPlot({
        reactive({
          if(input$distancing == "Yes") {
            ar = 0.1
          } else {
            ar = 0.5
          }
          if(input$mask == "Yes") {
            a = 0.077
          } else {
            a = 0.324
          }
          param <- param.dcm(inf.prob = a*(1.02-input$vaxRate/100),
                             act.rate = ar,
                             rec.rate = 0.2)
          init <- init.dcm(s.num = 76000000, i.num = 1000)
          control <- control.dcm(type = "SIS", nsteps = 365)
          mod <- dcm(param, init, control)
        })
        plot(mod)
      })
    
  }
)
  
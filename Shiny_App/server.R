#server.R

#

#library(shinydashboard)
#library(shiny)    
#library(rintrojs)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
  

  # Respiratory 
  output$box1 <- renderPlot({
    evi.graphs(EVI_output=EVI_Res_Inf, graph="EVI", ln=F)
  })

  output$box2 <- renderPlot({
    evi.graphs(EVI_output=EVI_Res_Inf, graph="EVI", ln=T)
  })
  
  output$box3 <- renderPlot({
    evi.graphs(EVI_output=EVI_Res_Inf, graph="PPV", ln=T)
  })
  
  output$box4 <- renderPlot({
    evi.graphs(EVI_output=EVI_Res_Inf, graph="NPV", ln=T)
  })
  
  # Pneumonia
  output$box5 <- renderPlot({
    evi.graphs(EVI_output=EVI_Pneum, graph="EVI", ln=F)
  })
  
  output$box6 <- renderPlot({
    evi.graphs(EVI_output=EVI_Pneum, graph="EVI", ln=T)
  })
  
  output$box7 <- renderPlot({
    evi.graphs(EVI_output=EVI_Pneum, graph="PPV", ln=T)
  })
  
  output$box8 <- renderPlot({
    evi.graphs(EVI_output=EVI_Pneum, graph="NPV", ln=T)
  })
  
  # COVID
  
  output$box9 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F)
  })
  
  output$box9 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F)
  })
  
  output$box10 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=T)
  })
  
  output$box11 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="PPV", ln=T)
  })
  
  output$box12 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="NPV", ln=T)
  })
  
  # FLU
  
  output$box13 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=F)
  })

  output$box14 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=T)
  })
  
  
  output$box15 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="PPV", ln=T)
  })
  
  output$box16 <- renderPlot({
        evi.graphs(EVI_output=EVI_Flu, graph="NPV", ln=T)
  })
  

}
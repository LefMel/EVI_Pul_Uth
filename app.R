# UI_EVI_UTH
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library(shiny)    
library(shinydashboard)
library(EVI)

ui <- dashboardPage(
  
  skin = "black",
  
  # HEADER
  
  dashboardHeader(
    title = "Epidemic Volatility Index | Pulmonary Clinic - University of Thessaly",
    titleWidth = 900
    
  ),
  
  # SIDEBAR
  dashboardSidebar(
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    
    sidebarUserPanel("Version 1.0"),
    
    tabPanel("Info",
             h3("Respiratory Infections"),
             h5("Time period : 02/10/2021 until 2023"),
             h5("Days: the serial number for each time point."),
             h6("01: 02/10/2021 | 225: 20/12/2022"),
             h3("Pneumonia"),
             h5("Time period : 02/10/2021 until 2023"),
             h5("Days: the serial number for each time point."),
             h6("01: 02/10/2021 | 225: 20/12/2022"),
             h3("Flu"),
             h5("Time period : 16/11/2022 until 2023"),
             h5("Days: the serial number for each time point."),
             h6("01: 16/11/2022 | 25: 05/01/2023"),
             h3("COVID"),
             h5("Time period : 01/02/2022 until 2023"),
             h5("Days: the serial number for each time point."),
             h6("01: 01/02/2022 | 170: 20/12/2022"),
             
             h4("Figures Explained"),
             h5("Upper left:"),
             h6("Observations (updated every two days), presented on the original scale, with red dots corresponding to dates that, according to EVI, an early warning was issued."),
             h5("Upper right:"),
             h6("Observations (updated every two days), presented on the logarithmic scale, which facilitates the comparison of the steepness of the epidemic curve between the different waves."),
             h5("Bottom left:"),
             h6("Positive predictive value (PPV) for the days that an early warning was issued. Higher color intensity corresponds to PPV closer to the value of 1."),
             h5("Bottom right:"),
             h6("Negative predictive values (NPV) for the days that an early warning was not issued. Higher color intensity corresponds to NPV closer to the value of 1.")
             
             
    )
  ),
  
  
  dashboardBody(
    tabsetPanel(
      tabPanel("Respiratory Infections",
               fluidRow(
                 box(plotOutput("box1")),
                 box(plotOutput("box2")),
                 box(plotOutput("box3")),
                 box(plotOutput("box4"))
               )
      )
    ),
    tabsetPanel(
      tabPanel("Pneumonia",
               fluidRow(
                 box(plotOutput("box5")),
                 box(plotOutput("box6")),
                 box(plotOutput("box7")),
                 box(plotOutput("box8"))
               )
      )
    ),
    tabsetPanel(
      tabPanel("Flu",
               fluidRow(
                 box(plotOutput("box9")),
                 box(plotOutput("box10")),
                 box(plotOutput("box11")),
                 box(plotOutput("box12"))
               )
      )
    ),
    tabsetPanel(
      tabPanel("COVID",
               fluidRow(
                 box(plotOutput("box13")),
                 box(plotOutput("box14")),
                 box(plotOutput("box15")),
                 box(plotOutput("box16"))
               )
      )
    )
  )
  
  
  
)      

# MAIN BODY
#  fluidRow(
#      column(        width = 12,
#         bsButton("Respiratory Infections", 
#                 label = "res_inf", 
#icon = icon("user"), 
#                 style = "success"),
#   bsButton("Pneumonia", 
###            label = "pneumonia", 
#         # icon = icon("spinner", class = "spinner-box"), 
#            style = "success"),
#   bsButton("COVID-19", 
#           label = "covid", 
#icon = icon("flask", class = "flask-box"), 
#           style = "success"),
#  bsButton("Flu", 
#           label = "flu", 
#           #icon = icon("thumbs-up"), 
#           style = "success"),
# )
#)



#  fluid_design("res_inf_panel", "box1", "box2", "box3", "box4"),
# fluid_design("pneumonia_panel", "box5", "box6", "box7", "box8"),
# fluid_design("covid_panel", "box9", "box10", "box11", "box12"),
# fluid_design("flu_panel", "box13", "box14", "box15", "box16")



#server.R

#

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  library(EVI)
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
  
  # Flu
  output$box9 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=F)
  })
  
  output$box10 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=T)
  })
  
  output$box11 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="PPV", ln=T)
  })
  
  output$box12 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="NPV", ln=T)
  })
  
  # COVID
  
  output$box13 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F)
  })
  
  output$box14 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F)
  })
  
  output$box15 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=T)
  })
  
  output$box16 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="PPV", ln=T)
  })
  
  output$box12 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="NPV", ln=T)
  })
  

  
  
}

shinyApp(ui=ui, server = server)


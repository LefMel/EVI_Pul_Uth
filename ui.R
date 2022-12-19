# UI_EVI_UTH
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#library(shiny)

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
   div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;")
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
      tabPanel("COVID",
               fluidRow(
                 box(plotOutput("box9")),
                 box(plotOutput("box10")),
                 box(plotOutput("box11")),
                 box(plotOutput("box12"))
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
 
 
  

  

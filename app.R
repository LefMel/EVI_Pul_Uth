# UI_EVI_UTH
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.


library(shiny)    
library(shinydashboard)
# library(EVI)
library(ggplot2)

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
    
    sidebarUserPanel("Version 1.2"),
    
    sidebarMenu(id ="tabs",
      menuItem("Time Period", tabName = "Time_Period", icon = icon("bar-chart")),
              dateRangeInput("rdates_Res_Inf", "Range of dates for Respiratory Infections:", start = min(EVI_Res_Inf$Days), end = max(EVI_Res_Inf$Days)), # input$rdates_Res_Inf
              dateRangeInput("rdates_COVID", "Range of dates for COVID-19 cases:", start = min(EVI_COVID$Days), end = max(EVI_COVID$Days)), # input$rdates_COVID
              dateRangeInput("rdates_Flu", "Range of dates for Flu cases:", start = min(EVI_Flu$Days), end = max(EVI_Flu$Days)), # input$rdates_Flu
      #dateRangeInput("rdates_Pneum", "Range of dates for Pneumonia:", start = min(EVI_Pneum$Days), end = max(EVI_Pneum$Days)), # input$rdates_Pneum

              
      
            
      menuItem("Model Predictive Value", tabName = "PV", icon = icon("line-chart"),
               menuItem("Positive Predictive Value", icon = icon("angle-right"),
                           checkboxInput("PPV", "Plot model's positive predictive values", value = FALSE) # input$PPV
                        ),
               menuItem("Negative Predictive Value", tabName = "NPV", icon = icon("angle-right"),
                          checkboxInput("NPV", "Plot model's negative predictive values", value = FALSE) # input$NPV
                        )
               ),
      #menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
      #menuItem("About", tabName = "about", icon = icon("question")),
      hr(),
      
      menuItem("Plot Features", tabName = "dashboard", icon = icon("line-chart"),
               
               checkboxInput("rlines", "Draw lines instead of points (EVI plots)", value = FALSE)#, # input$rlines==FALSE
               
         #      numericInput("rsize", "Size of points", value = 1.5) # input$rsize
               
               )

      
      
    ),
    
    hr(),
    h4("References"),
    helpText(tags$b(""),"Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021)",
             tags$a(href="https://www.nature.com/articles/s41598-021-02622-3", 
                    "Read it here")),
    helpText(tags$b(""),"Meletis E. et al. EVI R-package: Epidemic Volatility Index as an Early-Warning Tool",
             tags$a(href="https://cran.r-project.org/web/packages/EVI/EVI.pdf", 
                    "R-package Info")),
    
    
    tabPanel("Info", tabName = "Widgets", icon = icon("th"),
             
             h4("Figures Explained"),
             h5("Upper left:"),
             h6("Observations (updated every two days), presented on the original scale, with red dots corresponding to dates that, according to EVI, an early warning was issued."),
             h5("Upper right:"),
             h6("Observations (updated every two days), presented on the logarithmic scale, which facilitates the comparison of the steepness of the epidemic curve between the different waves."),
             hr(),
             h5("Model Predictive value"),
             h6("Positive predictive value (PPV) for the days that an early warning was issued. Higher color intensity corresponds to PPV closer to the value of 1."),
             h6("Negative predictive values (NPV) for the days that an early warning was not issued. Higher color intensity corresponds to NPV closer to the value of 1.")
             
    )
  ),
  
  
  dashboardBody(
    
    tabBox(width=NULL,
    
      tabPanel(h5("Respiratory Infections"),
               fluidRow(
                 box(plotOutput("boxR1")),
                 box(plotOutput("boxR2")),
                 box(plotOutput("boxR3")),
                 box(plotOutput("boxR4")),
                 box(plotOutput("boxR5")),
                 box(plotOutput("boxR6"))
               )
      ),
      tabPanel("COVID-19",
               fluidRow(
                 box(plotOutput("boxC1")),
                 box(plotOutput("boxC2")),
                 box(plotOutput("boxC3")),
                 box(plotOutput("boxC4")),
                 box(plotOutput("boxC5")),
                 box(plotOutput("boxC6"))
               )
      ),
  
      tabPanel("Flu",
               fluidRow(
                 box(plotOutput("boxF1")),
                 box(plotOutput("boxF2")),
                 box(plotOutput("boxF3")),
                 box(plotOutput("boxF4")),
                 box(plotOutput("boxF5")),
                 box(plotOutput("boxF6"))
               )
      )#,

      #tabPanel("Pneumonia",
      #         fluidRow(
      #           box(plotOutput("boxP1")),
      #           box(plotOutput("boxP2")),
      #           box(plotOutput("boxP3")),
      #           box(plotOutput("boxP4")),
      #           box(plotOutput("boxP5")),
      #           box(plotOutput("boxP6"))
      #         )
      #)

    )
  )
)


#server.R


server <- function(input, output) {
  
 evi.graphs <- function (EVI_output, graph = c("EVI"), ln = TRUE, type = "p") 
 {
   if (!exists("EVI_output")) 
     stop("Please run the deviant function first")
   EVI_output$cases_1 = as.numeric(EVI_output$Cases * EVI_output$Index)
   EVI_output$cases_1[EVI_output$cases_1 == 0] <- NA
   EVI_output$cases_0 = as.numeric(EVI_output$Cases * (1 - EVI_output$Index))
   EVI_output$cases_0[EVI_output$cases_0 == 0] <- NA
   EVI_output$npv = as.numeric(EVI_output$npv * (1 - EVI_output$Index))
   EVI_output$npv[EVI_output$npv == 0] <- NA
   EVI_output$ppv = as.numeric(EVI_output$ppv * EVI_output$Index)
   EVI_output$ppv[EVI_output$ppv == 0] <- NA
   EVI_output$variable <- "x"
   if (graph == "EVI" && ln == FALSE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = ("Cases"), 
                           color = "Index>0"), size = 0.5), scale_color_manual(values = c("grey69", "red3")), theme(legend.position = "none"), 
                           labs(y = "Cases", x = "Days"), if (type =="l") geom_path(aes_string(y = "Cases",colour = "factor(Index>0)")))
   }
   if (graph == "EVI" && ln == TRUE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = "log(Cases)", 
                           color = "Index>0"), size = 0.5), scale_color_manual(values = c("grey69", "red3")), theme(legend.position = "none"), 
                           labs(y = "ln(Cases)", x = "Days"), if (type =="l") geom_path(aes_string(y = "log(Cases)", colour = "factor(Index>0)")))
   }
   if (graph == "PPV" && ln == FALSE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = "(cases_1)", col = "ppv"), size = 0.5), geom_point(aes_string(y = "(cases_0)"), 
                           col = "grey69", size = 0.5), labs(y = "Cases", x = ""), scale_color_gradient(low = "green", high = "red", limits = c(0, 1)), labs(color = "PPV"), 
                           theme(legend.position = c(0.95, 0.3), legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.key.height = unit(0.5, "cm")))
   }
   if (graph == "PPV" && ln == TRUE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = "log(cases_1)", col = "ppv"), size = 0.5), geom_point(aes_string(y = "log(cases_0)"), 
                           col = "grey69", size = 0.5), labs(y = "ln(Cases)", x = ""), scale_color_gradient(low = "green", high = "red", limits = c(0, 1)), labs(color = "PPV"), 
                           theme(legend.position = c(0.95, 0.3), legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.key.height = unit(0.5,"cm")))
   }
   if (graph == "NPV" && ln == FALSE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = "(cases_0)", col = "npv"), size = 0.5), geom_point(aes_string(y = "(cases_1)"), 
                           col = "grey69", size = 0.5), labs(y = "Cases"), scale_color_gradient(low = "green", high = "red", limits = c(0, 1)), labs(color = "NPV"), 
                           theme(legend.position = c(0.95, 0.3), legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.key.height = unit(0.5, "cm")))
   }
   if (graph == "NPV" && ln == TRUE) {
     sp3 <- ggplot(EVI_output, aes_string(x = "Days", group = "variable")) + list(geom_point(aes_string(y = "log(cases_0)", col = "npv"), size = 0.5), geom_point(aes_string(y = "log(cases_1)"), 
                           col = "grey69", size = 0.5), labs(y = "ln(Cases)"), scale_color_gradient(low = "green", high = "red", limits = c(0, 1)), labs(color = "NPV"), 
                           theme(legend.position = c(0.95, 0.3), legend.title = element_text(size = 10), legend.text = element_text(size = 8), legend.key.height = unit(0.5, "cm")))
   }
   print(sp3)
 }
 
# Respiratory 
 
# KP: Here I have added a text output. If the date selected by the user does not exist gives a suggestion to move onto the next one.
#  output$daterange1_valid_Resp <- renderText({
#    is_valid <- any(input$rdates_Res_Inf == EVI_Res_Inf$Days)
#    ifelse(is_valid, "OK", "No available respiratory data for the chosen date. Choose the next available.")
#  })

#  output$daterange1_valid_Pneum <- renderText({
#    is_valid <- any(input$rdates_Pneum == EVI_Pneum$Days)
#    ifelse(is_valid, "OK", "No available pneumonia data for the chosen date. Choose the next available.")
#  })
 
#  output$daterange1_valid_Flu <- renderText({
#    is_valid <- any(input$rdates_Flu == EVI_Flu$Days)
#    ifelse(is_valid, "OK", "There is no available flu data for the chosen date. Choose the next available.")
#  })
 
#  output$daterange1_valid_COVID <- renderText({
#    is_valid <- any(input$rdates_COVID == EVI_COVID$Days)
#    ifelse(is_valid, "OK", "There is no available COVID data for the chosen date. Choose the next available.")
#  })
 
  
  output$boxR1 <- renderPlot({
      LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
      UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
      evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxR2 <- renderPlot({
    LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
    UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
    evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxR3 <- renderPlot({
    LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
    UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="PPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")


  })

  output$boxR4 <- renderPlot({
    LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
    UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")

    
  })
  
  output$boxR5 <- renderPlot({
    LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
    UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
    
  ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="NPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  output$boxR6 <- renderPlot({
    LL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1])) 
    UL=ifelse(identical(which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]),integer(0)), yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]-1))), no = which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2])) 
    ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  # COVID
  
  output$boxC1 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxC2 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxC3 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="PPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")

  })
  
  output$boxC4 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
     
    
  })
  
  output$boxC5 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    
    ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="NPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  output$boxC6 <- renderPlot({
    LL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[1]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[1]+1))), no = which(EVI_COVID$Days==input$rdates_COVID[1])) 
    UL=ifelse(identical(which(EVI_COVID$Days==input$rdates_COVID[2]),integer(0)), yes = which(EVI_COVID$Days==(as.Date(input$rdates_COVID[2]-1))), no = which(EVI_COVID$Days==input$rdates_COVID[2])) 
    
    ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_COVID[LL:UL,], graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  # Flu
  output$boxF1 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 
    evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxF2 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 
    
    evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$boxF3 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="PPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")

  })
  
  output$boxF4 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 
    
    ifelse(test = input$PPV, evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  output$boxF5 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 
    
    ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="NPV", ln=F,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  output$boxF6 <- renderPlot({
    LL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[1]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[1]+1))), no = which(EVI_Flu$Days==input$rdates_Flu[1])) 
    UL=ifelse(identical(which(EVI_Flu$Days==input$rdates_Flu[2]),integer(0)), yes = which(EVI_Flu$Days==(as.Date(input$rdates_Flu[2]-1))), no = which(EVI_Flu$Days==input$rdates_Flu[2])) 

    ifelse(test = input$NPV, evi.graphs(EVI_output=EVI_Flu[LL:UL,], graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p")), "Check Model predictive value")
    
  })
  
  # Pneumonia
  #output$box13 <- renderPlot({
  #  evi.graphs(EVI_output=EVI_Pneum[input$rdates_Pneum[1]:input$rdates_Pneum[2],], graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  #})
  
  #output$box14 <- renderPlot({
  #  evi.graphs(EVI_output=EVI_Pneum[input$rdates_Pneum[1]:input$rdates_Pneum[2],], graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  #})
  
  #output$box15 <- renderPlot({
  #  evi.graphs(EVI_output=EVI_Pneum[input$rdates_Pneum[1]:input$rdates_Pneum[2],], graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  #})
  
  #output$box16 <- renderPlot({
  #  evi.graphs(EVI_output=EVI_Pneum[input$rdates_Pneum[1]:input$rdates_Pneum[2],], graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  #})
  
  
  
}

shinyApp(ui=ui, server = server)


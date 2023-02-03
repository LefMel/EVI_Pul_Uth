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
    
    sidebarMenu(
      menuItem("Options", tabName = "dashboard", icon = icon("dashboard"),
               dateRangeInput("rdates_Res_Inf", "Range of dates for Respiratory Infections:", start = min(EVI_Res_Inf$Days), end = max(EVI_Res_Inf$Days)), # input$rdates_Res_Inf
        #       dateRangeInput("rdates_Pneum", "Range of dates for Pneumonia:", start = min(EVI_Pneum$Days), end = max(EVI_Pneum$Days)), # input$rdates_Pneum
        #       dateRangeInput("rdates_Flu", "Range of dates for Flu cases:", start = min(EVI_Flu$Days), end = max(EVI_Flu$Days)), # input$rdates_Flu
        #       dateRangeInput("rdates_COVID", "Range of dates for COVID-19 cases:", start = min(EVI_COVID$Days), end = max(EVI_COVID$Days)), # input$rdates_COVID
          
                 
               
               checkboxInput("rlines", "Draw lines instead of points (EVI plots)", value = FALSE)#, # input$rlines==FALSE
               
         #      numericInput("rsize", "Size of points", value = 1.5) # input$rsize
               
               )
      
      
      
    ),
    
    
    tabPanel("Info", tabName = "Widgets", icon = icon("th"),
             h3("Respiratory Infections"),
             h5("Time period:",  paste(min(EVI_Res_Inf$Days), "-", max(EVI_Res_Inf$Days))),
             textOutput("daterange1_valid_Resp"), #KP:  Error message for Respiratory infections selection
             h3("--"),
             
             h3("Pneumonia"),
             h5("Time period:",  paste(min(EVI_Pneum$Days), "-", max(EVI_Pneum$Days))),
             #textOutput("daterange1_valid_Pneum"), #KP:  Uncomment if added in server(), see Resp example.
             h3("--"),
             
             h3("Flu"),
             h5("Time period:",  paste(min(EVI_Flu$Days), "-", max(EVI_Flu$Days))),
             #textOutput("daterange1_valid_Flu"), #KP:  Uncomment if added in server()
             h3("--"),
             
             h3("COVID"),
             h5("Time period:",  paste(min(EVI_COVID$Days), "-", max(EVI_COVID$Days))),
             #textOutput("daterange1_valid_COVID"), #KP:  Uncomment if added in server()
             
             h4("Figures Explained"),
             h5("Upper left:"),
             h6("Observations (updated every two days), presented on the original scale, with red dots corresponding to dates that, according to EVI, an early warning was issued."),
             h5("Upper right:"),
             h6("Observations (updated every two days), presented on the logarithmic scale, which facilitates the comparison of the steepness of the epidemic curve between the different waves."),
             h5("Bottom left:"),
             h6("Positive predictive value (PPV) for the days that an early warning was issued. Higher color intensity corresponds to PPV closer to the value of 1."),
             h5("Bottom right:"),
             h6("Negative predictive values (NPV) for the days that an early warning was not issued. Higher color intensity corresponds to NPV closer to the value of 1."),
             h4("References"),
             helpText(tags$b(""),"Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021)",
                      tags$a(href="https://www.nature.com/articles/s41598-021-02622-3", 
                             "Read it here")),
             helpText(tags$b(""),"Meletis E. et al. EVI R-package: Epidemic Volatility Index as an Early-Warning Tool",
                      tags$a(href="https://cran.r-project.org/web/packages/EVI/EVI.pdf", 
                             "R-package Info"))
             
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
      tabPanel("COVID",
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
      tabPanel("Pneumonia",
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
 output$daterange1_valid_Resp <- renderText({
   is_valid <- any(input$rdates_Res_Inf == EVI_Res_Inf$Days)
   ifelse(is_valid, "OK", "There is no available respiratory data for the chosen date. Choose the next available.")
 })
 # Similarly for the other 3 if you find it useful.
 
  output$box1 <- renderPlot({
    LL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]);UL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]) #KP:  These choose the number of line based on the input date resulting in a a vector LL:UL 
    # KP: The next two lines may be useful to automate the wrong choice of date by the user. 
    #  LL=ifelse(identical(LL1, numeric(0)),yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[1]+1))),no = LL1)    
    #  UL=ifelse(identical(UL1, numeric(0)),yes = which(EVI_Res_Inf$Days==(as.Date(input$rdates_Res_Inf[2]+1))),no = UL1)
    evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box2 <- renderPlot({
    LL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]);UL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]) #KP: Same 
    evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box3 <- renderPlot({
    LL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]);UL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]) #KP: Same 
    evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box4 <- renderPlot({
    LL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[1]);UL=which(EVI_Res_Inf$Days==input$rdates_Res_Inf[2]) #KP: Same 
    evi.graphs(EVI_output=EVI_Res_Inf[LL:UL,], graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  
  # COVID
  
  output$box5 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box6 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box7 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box8 <- renderPlot({
    evi.graphs(EVI_output=EVI_COVID, graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  # Flu
  output$box9 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=F,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box10 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="EVI", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box11 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="PPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
  })
  
  output$box12 <- renderPlot({
    evi.graphs(EVI_output=EVI_Flu, graph="NPV", ln=T,type = ifelse(test = input$rlines,"l","p"))
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


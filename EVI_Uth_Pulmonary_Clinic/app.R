
library(shiny)
library(readxl)
library(openxlsx)
library(ggplot2)
library(cowplot)
library(readr)
library(EVI)

getwd()
setwd("C:/Users/LefMel/Documents/EVI Uth")
getwd()

data_dir <- list.files(path="Data/", full.names = TRUE)

Res_Inf <- do.call(rbind, lapply(data_dir, function(x)
                    cbind(read.xlsx(x, cols=c(1,2,3)),
                          name=tools::file_path_sans_ext(basename(x)))))
Pneum <- do.call(rbind, lapply(data_dir, function(x)
  cbind(read.xlsx(x, cols=c(1,4,5)),
        name=tools::file_path_sans_ext(basename(x)))))
Att_Emerg <- do.call(rbind, lapply(data_dir, function(x)
  cbind(read.xlsx(x, cols=c(1,6)),
        name=tools::file_path_sans_ext(basename(x)))))
  
Total <- do.call(rbind, lapply(data_dir, function(x)
  cbind(read.xlsx(x, cols=c(1,9,10)),
        name=tools::file_path_sans_ext(basename(x)))))

COVID_19 <- do.call(rbind, lapply(data_dir, function(x)
  cbind(read.xlsx(x, cols=c(1,7,8)),
        name=tools::file_path_sans_ext(basename(x)))))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("EVI | Pulmonary Clinic, University of Thessaly"),
  
  img(src="uth_logo", height = 100, width = 100),


  
  

## Tab for different monitoring category
  tabsetPanel(
    tabPanel("Respiratory Infections", fluid = TRUE,
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
              helpText(tags$b("Cite as"),": Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021).", 
                        tags$a(href="https://doi.org/10.1038/s41598-021-02622-3", 
                               "DOI: 10.1038/s41598-021-02622-3")) 
             ),
             
             # Create a spot for the barplot
             mainPanel(
               plotOutput("ResPlot1"),
               plotOutput("ResPlot2"),
               plotOutput("ResPlot3"),
               plotOutput("ResPlot4")
               
             )
             
           )
    ),
    tabPanel("Pneumonia", fluid = TRUE,
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               helpText(tags$b("Cite as"),": Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021).", 
                        tags$a(href="https://doi.org/10.1038/s41598-021-02622-3", 
                               "DOI: 10.1038/s41598-021-02622-3")) 
             ),
             
             # Create a spot for the barplot
             mainPanel(
               plotOutput("PneumPlot1"),
               plotOutput("PneumPlot2"),
               plotOutput("PneumPlot3"),
               plotOutput("PneumPlot4")
               
             )
             
           )
    ),
    tabPanel("Total", fluid = TRUE,
           sidebarLayout(      
             
             # Define the sidebar with one input
             sidebarPanel(
               helpText(tags$b("Cite as"),": Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021).", 
                        tags$a(href="https://doi.org/10.1038/s41598-021-02622-3", 
                               "DOI: 10.1038/s41598-021-02622-3")) 
             ),
             
             # Create a spot for the barplot
             mainPanel(
               plotOutput("TotalPlot1"),
               plotOutput("TotalPlot2"),
               plotOutput("TotalPlot3"),
               plotOutput("TotalPlot4")
               
             )
             
           )
    ),
#   tabPanel("COVID-19", fluid = TRUE,
#         sidebarLayout(      
           
           # Define the sidebar with one input
#           sidebarPanel(
#             helpText(tags$b("Cite as"),": Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021).", 
#                      tags$a(href="https://doi.org/10.1038/s41598-021-02622-3", 
#                             "DOI: 10.1038/s41598-021-02622-3")) 
#           ),
           
           # Create a spot for the barplot
#           mainPanel(
#             plotOutput("COVPlot1"),
#             plotOutput("COVPlot2"),
#             plotOutput("COVPlot3"),
#             plotOutput("COVPlot4")
             
#           )
           
#         )
#   ),
    tabPanel("Attendance", fluid = TRUE,
         sidebarLayout(      
           
           # Define the sidebar with one input
           sidebarPanel(
             helpText(tags$b("Cite as"),": Kostoulas, P., Meletis, E., Pateras, K. et al. The epidemic volatility index, a novel early warning tool for identifying new waves in an epidemic. Sci Rep 11, 23775 (2021).", 
                      tags$a(href="https://doi.org/10.1038/s41598-021-02622-3", 
                             "DOI: 10.1038/s41598-021-02622-3")) 
           ),
           
           # Create a spot for the barplot
           mainPanel(
             plotOutput("AttPlot1"),
             plotOutput("AttPlot2"),
             plotOutput("AttPlot3"),
             plotOutput("AttPlot4")
             
           )
           
         )
      )
  )
)



# Define server 
server <- function(input, output) {
  
  EVI_Res_Inf = deviant(as.numeric(Res_Inf$Respiratory.Infections[2:60]))
  EVI_Pneum = deviant(as.numeric(Pneum$Pneumonia[2:60]))
  EVI_Total = deviant(as.numeric(Total$`Total.(Respiratory.inf.+.Pneumonia)`[2:60]))
  EVI_Att =   deviant(Att_Emerg$Attendance.at.Emergency.Department[2:60])
  
  # Respiratory 
  output$ResPlot1 <- renderPlot({
    
#    deviant(as.numeric(Res_Inf$Respiratory.Infections[2:63]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
     evi.graphs(EVI_output=EVI_Res_Inf, graph="EVI", ln=F)
    
  })
  
  output$ResPlot2 <- renderPlot({
    
#    deviant(as.numeric(Res_Inf$Respiratory.Infections[2:63]))
    
    evi.graphs(EVI_output=EVI_Res_Inf, graph="EVI", ln=T)
    
  })
  
  output$ResPlot3 <- renderPlot({
    
#    deviant(as.numeric(Res_Inf$Respiratory.Infections[2:63]))
    
    evi.graphs(EVI_output=EVI_Res_Inf, graph="PPV", ln=T)
    
  })
  
  output$ResPlot4 <- renderPlot({
    
#    deviant(as.numeric(Res_Inf$Respiratory.Infections[2:63]))
    
    evi.graphs(EVI_output=EVI_Res_Inf, graph="NPV", ln=T)
    
  })
  
  # Pneumonia
  output$PneumPlot1 <- renderPlot({
    
#    deviant(as.numeric(Pneum$Pneumonia[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
     evi.graphs(EVI_output=EVI_Pneum, graph="EVI", ln=F)
    
    
  })
  
  output$PneumPlot2 <- renderPlot({
    
#    deviant(as.numeric(Pneum$Pneumonia[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=T)
     evi.graphs(EVI_output=EVI_Pneum, graph="EVI", ln=T)
    
  })
  
  output$PneumPlot3 <- renderPlot({
    
#    deviant(as.numeric(Pneum$Pneumonia[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="PPV", ln=T)
     evi.graphs(EVI_output=EVI_Pneum, graph="PPV", ln=T)
    
  })
  
  output$PneumPlot4 <- renderPlot({
    
#    deviant(as.numeric(Pneum$Pneumonia[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="NPV", ln=T)
     evi.graphs(EVI_output=EVI_Pneum, graph="NPV", ln=T)   
    
  })
  
  # Total
  output$TotalPlot1 <- renderPlot({
    
#    deviant(as.numeric(Total$`Total.(Respiratory.inf.+.Pneumonia)`[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
     evi.graphs(EVI_output=EVI_Total, graph="EVI", ln=F)
    
  })
  
  output$TotalPlot2 <- renderPlot({
    
#    deviant(as.numeric(Total$`Total.(Respiratory.inf.+.Pneumonia)`[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=T)
    evi.graphs(EVI_output=EVI_Total, graph="EVI", ln=T)
    
  })
  
  output$TotalPlot3 <- renderPlot({
    
#    deviant(as.numeric(Total$`Total.(Respiratory.inf.+.Pneumonia)`[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="PPV", ln=T)
    evi.graphs(EVI_output=EVI_Total, graph="PPV", ln=T)
    
  })
  
  output$TotalPlot4 <- renderPlot({
    
#    deviant(as.numeric(Total$`Total.(Respiratory.inf.+.Pneumonia)`[2:65]))
    
#    evi.graphs(EVI_output=EVI_output, graph="NPV", ln=T)
    evi.graphs(EVI_output=EVI_Total, graph="NPV", ln=T)
    
  })
  
  # Attendance
  output$AttPlot1 <- renderPlot({
    
#    deviant(Att_Emerg$Attendance.at.Emergency.Department[2:63])
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
     evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
    
    
  })
  
  output$AttPlot2 <- renderPlot({
    
#    deviant(Att_Emerg$Attendance.at.Emergency.Department[2:63])
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=T)
    evi.graphs(EVI_output=EVI_Att, graph="EVI", ln=T)
    
  })
  
  output$AttPlot3 <- renderPlot({
    
#    deviant(Att_Emerg$Attendance.at.Emergency.Department[2:63])
    
#    evi.graphs(EVI_output=EVI_output, graph="PPV", ln=T)
    evi.graphs(EVI_output=EVI_Att, graph="PPV", ln=T)
    
  })
  
  output$AttPlot4 <- renderPlot({
    
#    deviant(Att_Emerg$Attendance.at.Emergency.Department[2:63])
    
#    evi.graphs(EVI_output=EVI_output, graph="NPV", ln=T)
    evi.graphs(EVI_output=EVI_Att, graph="NPV", ln=T)
    
  })
  
  # COVID-19 Cases
#  output$COVPlot1 <- renderPlot({
    
#    deviant(as.numeric(COVID_19$`COVID-19.Cases`[2:63]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=F)
    
#  })
  
#  output$COVPlot2 <- renderPlot({
    
#    deviant(as.numeric(COVID_19$`COVID-19.Cases`[2:63]))
    
#    evi.graphs(EVI_output=EVI_output, graph="EVI", ln=T)
    
#  })
  
#  output$COVPlot3 <- renderPlot({
    
#    deviant(as.numeric(COVID_19$`COVID-19.Cases`[2:63])))
    
#    evi.graphs(EVI_output=EVI_output, graph="PPV", ln=T)
    
#  })
  
#  output$COVPlot4 <- renderPlot({
    
#    deviant(as.numeric(COVID_19$`COVID-19.Cases`[2:63]))
    
#    evi.graphs(EVI_output=EVI_output, graph="NPV", ln=T)
  
#  })


}


# Run the application 
shinyApp(ui = ui, server = server)

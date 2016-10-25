############## CSE 674 Project 1 - Bayesian Networks ##############
## Authors: 
## 1. KARTHIK CHAGANTI :  kchagant :50169441
## 2. VAIBHAV LELLA    :  vaibhavl :50169859
###################################################################


# source("http://bioconductor.org/biocLite.R")
# biocLite("RBGL")
# biocLite("Rgraphviz")
# install.packages("gRain")
# install.packages("bnlearn")
# install.packages("caTools")
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
#install.packages("shinythemes")
library("shinythemes")
library(Rgraphviz)
library(gRain)
library(bnlearn)
library(caTools)
library(DiagrammeR)
library(ggplot2)
library(googleVis)
library(markdown)
shinyUI(navbarPage("ESTIMATION OF RISK POSED BY CAR INSURANCE SEEKER",theme="bootstrap.css",
                tabPanel("Application",                
                 sidebarLayout(
                      sidebarPanel(br(),wellPanel(
                        
                        selectInput("evidence_input", "Evidence Variables:",selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )),
                        
                        
                        # This outputs the dynamic UI component
                        uiOutput("ui"),
                        actionButton("gobutton","Add")
                      ),
                      wellPanel(
                        selectInput("target_input","Target Variable:",selectize = TRUE,
                                    c("Choose One"="","GoodStudent", "Age", "SocioEcon","RiskAversion","VehicleYear","ThisCarDam","RuggedAuto","Accident","MakeModel",
                                      "DrivQuality","Mileage","Antilock","DrivingSkill","SeniorTrain","ThisCarCost","Theft","CarValue","HomeBase",
                                      "AntiTheft","PropCost","OtherCarCost","OtherCar","MedCost","Cushioning","Airbag","ILiCost","DrivHist"
                                    )
                                    
                        )
                      ),
                      actionButton("reset", "Reset")
                     
                     
                      
                      ),
                      mainPanel(
                        tags$style(type="text/css",
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        ),tags$p("Evidences:"),
                                verbatimTextOutput("evidence_input_text"),
                                tags$p("States:"),
                                verbatimTextOutput("dynamic_value"),
                                br(),
                                tabsetPanel(
                                  tabPanel("Bayesian Network",
                                #plotOutput("bayesnet",width="100%", height = "1200px")),
                                uiOutput("bayesnet_main")),
                                tabPanel("Inference Analysis",
                                         br(),
                                         #splitLayout(cellWidths = c("30%","70%"), uiOutput("bayes_extra"),htmlOutput("pie_extraaa")),
                                         verbatimTextOutput("withoutEV"),
                                         uiOutput("bayes_extra"),
                                          
                                         br(),
                                         br(),
                                         verbatimTextOutput("withEV"),
                                         splitLayout(cellWidths = c("30%","70%"), uiOutput("bayes"),htmlOutput("pie"))
                                         
                                          
                                         #htmlOutput("pie"),
                                         # gvisMerge("pie_extraa", "pie", horizontal = FALSE,
                                         #           tableOptions = "border=\"0\"", chartid)
                                        )
                                  
                                )
                      )
                 
                 
                    )
                  ),
                tabPanel("About",
                         br(),
                         verbatimTextOutput("Kchagant")
                         
                )
                
                ))
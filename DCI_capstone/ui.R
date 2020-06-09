#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dbHeader <- dashboardHeader()
dbHeader$children[[3]]$children <-tags$p( tags$h2(tags$b("Predictive Analytics For Dialysis Insurance Claims"),align = "middle"),style = "font-size:40%;")
dbHeader$children[[2]]$children <-tags$a(href='http://www.dciinc.org',
                                           tags$img(src='DCI_logo.png',height='40',width='190'))


# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        skin = "black",
        dbHeader,
        
        #****************************************************Side Bar**###############################################
        dashboardSidebar(
            sidebarMenu(id = "sidebarmenu",
                        menuItem("Welcome", tabName = "welcome",icon =icon("money-bill-alt ",class= NULL, lib="font-awesome")),
                       
                         menuItem("Payment Summary", tabName = "summary",icon = icon("file-invoice-dollar", class="fas fa-file-invoice", lib="font-awesome")),
                             conditionalPanel("input.sidebarmenu=='summary'",
                                         selectInput("summary_year","Pick Year",choices = c(2017,2018)) ),
                        
                        menuItem("Treatment Types", tabName = "payors", icon = icon("clinic-medical", class="fas fa-briefcase-medical" , lib="font-awesome")),
                            # conditionalPanel("input.sidebarmenu=='payors'",
                              #   selectInput("summary_payor","Select a Payor",choices = payors) ),
                       # conditionalPanel("input.sidebarmenu=='payors'",
                        #                 selectInput("summary_year_payor","Pick Year",choices = c( 2017,2018)) ),
                       conditionalPanel("input.sidebarmenu=='payors'",
                                        uiOutput("treatment_payors_sel_year")),
                       conditionalPanel("input.sidebarmenu=='payors'",
                                         uiOutput("treatment_payors")),
                       
                        
                                
                        
                        menuItem("Payors", tabName = "pcn", icon = icon("id-card", class="far fa-id-card",lib="font-awesome")),
                        
                        conditionalPanel("input.sidebarmenu=='pcn'",
                                         #selectInput("summary_year_pcn","Pick Year",choices = c( 2017,2018))
                                         uiOutput("summary_year_pcn_ou")),
                        
                        conditionalPanel("input.sidebarmenu=='pcn'",
                                         #selectInput("summary_payor_pcn","Select a Payor",choices = payors)
                                         uiOutput("summary_payor_pcn_ou")
                                         ),
                        
                        

                        menuItem("Revenue Categories",tabName = "medications",icon = icon("capsules")),
                        conditionalPanel("input.sidebarmenu=='medications'",
                                         selectInput("meds","Select Service Type",choices = meds_data$description)),
                        
                        menuItem("Payment Predictions",tabName = "paymentpredictions",icon = icon("trophy")),
                        
                        conditionalPanel("input.sidebarmenu=='paymentpredictions'",
                                         #selectInput("algo_payors","Select Payor Code",choices = pcn_payor_code_shiny)
                                         uiOutput("algo_payors_ou")
                                         ),
                        
                        conditionalPanel("input.sidebarmenu=='paymentpredictions'",
                                         #selectInput("algo_modality","Select Treatment Types",choices = modality_cost_codes_shiny)
                                         uiOutput("algo_modality_ou")
                                         ),
        
                                         
                        menuItem("Raw Data",tabName = "data",icon = icon("database"))
                 
                         
            )
            
        ),#****************************************************End of Side Bar**###############################################
        
        #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@**Dashboard Body**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
        dashboardBody(
            
            tabItems(
                #welcome page tab
                tabItem(tabName = "welcome",
                        fluidRow(
                            
                                   column(12,h2("Can We Forecast Future Insurance Payments?"),
                                   
                                   tags$p(tags$b("Project Details:")),
                                   tags$p("This application explores a dataset from Dialysis Clinic Incorporation's patient cliams. Dialysis claims data spans from 2017 to 2018."),
                                   tags$p("The goal of the exploration is to understand the distribution of payments and forecast future payments from Payors for different Treatment types and Revenue Categories. This project uses Regression Analysis to Forecast future payments for a Payor and Treatment type."),
                                          
                                   tags$p("Disclaimer: Data has been de identified for privacy protection."),
                        ),
                            
                                    column(12,img(src='doc2.jpeg', width='700',height='350' ),div(style="height:3px"))
                        ),
                        
                        fluidRow(
                            column(6,img(src='kidneys2.jpeg', width='600',height='300' )),
                            column(6,img(src='Mix_pic.png', width='600',height='300' ))
                            
                        )
                        ),
                
              ###############################################**Summary Tab**###############################################
                #summary tab
                tabItem(tabName = "summary",
                        
                        #Dist Histograms
                        fluidRow(column (12,height='10px',box(width=100, solidHeader = TRUE, plotlyOutput("distPlot")))),
                        
                        #box plots by modality
                        fluidRow(column (12,box(width=100, solidHeader = TRUE, plotlyOutput("distBoxPlot"))))
                    ),########################################** End of Summary Tab**######################################### 
              
              ###############################################**Treatments Tab**###############################################
                
               tabItem(tabName ="payors",
                        
                       #barplot of total charges by  treatment type
                        fluidRow(column (12,height='10px',box(width=100, solidHeader = TRUE, plotlyOutput("distpayorBarPlot")))),
                       
                       #barplot of percentage of charge by treatment type
                       fluidRow(column (12,height='10px',box(width=100, solidHeader = TRUE, plotlyOutput("distpayorBarPlotper"))))
                    ),##########################################** End of Treatments Tab**#########################################
              
              ###############################################**Payors Tab**##################################################
              
               #pcn
               tabItem(tabName = "pcn",
                       #barplot
                       fluidRow(column(6,height = '10px',box(width = 100, solidHeader = TRUE,plotlyOutput("distPcnBarPlot"))),
                       
                       #pie chart 1
                       fluidRow(column (6,height='1px',box(width=200, solidHeader = TRUE, plotOutput("distPcnplot")))
                                ),
                       
                       
                       #bar chart 2            
                          fluidRow( column (12,height='1px',box(width=100, solidHeader = TRUE, plotlyOutput("distPcnServicesPlot")))) 
                       )
                      
                   ),###############################################**End of Payors Tab**##################################################
              
              ##############################################**Medications Tab**##################################################

               tabItem(tabName = "medications",
              
                       fluidRow(column(12,height = '10px',box(width = 100, solidHeader = TRUE,plotlyOutput("distMedicationsplt")))),
                       fluidRow(
                           column(4,h2("Percentage of Charges: ")),
                           column(4,offset=0.5,valueBoxOutput("vbox1",width=500))
                       )
           
              ),###############################################**End of Medications Tab**###########################################
              
              ##############################################**Payment Predictions**##################################################
              tabItem(tabName = "paymentpredictions",
                      
                      fluidRow(
                          column(6,h2("Predictions using Linear Regression: ")),
                          column(8,height = '10px',box(width = 100, solidHeader = TRUE,plotlyOutput("ComparisionPlot")))
                            ),
                        fluidRow(
                          # column(6,height = '10px',box(width = 100, solidHeader = TRUE,textInput("inputcharge","Input Charge Amount",value=0)
                          #                               verbatimTextOutput( "predictedvalue")))
                          
                          column(4,h5("Input Charge Amount"),box(width = 20, solidHeader = TRUE,textInput("inputcharge","",value=0))),
                          column(4,h5("Predicted Value"),height = '10px',box(height = 85,width = 20, solidHeader = TRUE,verbatimTextOutput( "predictedvalue")))
                                                        
                                )
             ), ##############################################**End of Payment Predictions**##################################################
             
             tabItem(tabName = "data",
                     h1("Raw Data"),
                     fluidRow(
                         box(width = 12, status='primary',
                             'Click on the column to sort.',
                             DT::dataTableOutput("raw_data_table"))
                     ))
              
              
            ) ###end of Tab items###
            
            
        ) #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@**End of Dashboard Body**@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#
    )
)

source("server.R")

library(shiny)
library(shinyjs)
library(DataEditR)
dataset<-c()
shinyUI(
    fluidPage(
    useShinyjs(),
    extendShinyjs(text = HTML("shinyjs.ref = 
    function(){ 
        $('#trigger').click();
    }"), functions = c("ref")),
    titlePanel("Dashboard for data cleaning"),
    sidebarLayout(
        sidebarPanel(
            actionButton("trigger", "Refresh"),
            tags$div(
                class="button-container",
                h2("Data Lodaing", icon("file-csv")),
            ),
            fileInput("file1", 
                      "Choose CSV File",
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            selectInput("separator", "Separator", 
                        c("comma","semicolon","tab","space")),
            selectInput("decimal", "Decimal Values", 
                        c("comma","dot")),
            checkboxInput("header", "Header", TRUE),
            actionButton("upload", "Upload"),
            h2("Data Celaning"),
            tags$div(
                class="button-container",
                h3("Filtering",icon("filter")),
                
            ),
            varSelectInput("variableToFilterSelect",
                           "Variable to filter", dataset , multiple = F),
            selectInput("filter", "Condition", 
                        c("equal to","less than", "greater than")),
            textInput("filtervalue", "Value"),
            actionButton("applyFilter", "Apply filter"),
            tags$div(
                class="button-container",
                h3("N/A clean",icon("broom")),
                
            ),
            selectInput("fillMissing", "Fill by", 
                        c("mean","median")),
            actionButton("naClean", "Clean"),
            actionButton("naFill", "Fill"),
            tags$div(
                class="button-container",
                h3("Type casting", icon("font")),
            ),
            varSelectInput("variableToCastSelect",
                           "Variable to cast", dataset , multiple = F),
            selectInput("type", "Cast to type", 
                        c("character","numeric")),
            actionButton("cast", "Cast"),
            h3("Outlier testing"),
            varSelectInput("variableToCleanSelect", "Variable to test:", dataset , multiple = F),
            checkboxInput("highestLowest", "Highest/lowest value considered outlier", TRUE),
            h3("Outlier clean"),
            actionButton("oClean", "Clean"),
            actionButton("oFill", "Fill"),
            selectInput("ofillMissing", "Fill by", 
                        c("mean","median")),
            h2("Export data"),
            downloadButton('download',"Download the data"),
            
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Data Preview", 
                                 icon = icon("circle"), 
                                 uiOutput("dataPreviewUI"),
                                 verbatimTextOutput("dataPreviewTypes"),
                                 verbatimTextOutput("summary"),
                                 h4("Missing Data Chart"),
                                 plotOutput("missingDataChart")),
                        tabPanel("Lineplot", 
                                 icon = icon("chart-line"), 
                                 uiOutput("lineUI"),
                                 varSelectInput("dateVariable",
                                                "Date Variable",
                                                dataset,
                                                multiple = F),
                                 checkboxInput("datevariableSelect",
                                      "Cast date variable?",
                                      FALSE),
                                 conditionalPanel("input.datevariableSelect",
                                                  textInput("dateFormat", "Date Format")),
                                 varSelectInput("variablesLine",
                                                "Variables to be shown:",
                                                dataset,
                                                multiple = T)),
                        tabPanel("Histogram", 
                                 icon = icon("chart-area"), 
                                 uiOutput("histUI"),
                                 varSelectInput("variableHist", 
                                                "Variable to be shown:", 
                                                dataset, 
                                                multiple = F),
                                 sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 10)),
                        tabPanel("Panels", 
                                 icon = icon("grip-vertical"), 
                                 uiOutput("panelsUI"),
                                 varSelectInput("variablesPanels",
                                                "Variables to be shown:",
                                                dataset,
                                                multiple = T)),
                        tabPanel("Table",
                                 icon = icon("table"), 
                                 uiOutput("dataUI")),
                        tabPanel("Boxplot", 
                                 icon = icon("box"), 
                                 uiOutput("boxplotUI"),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable1boxSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable1box",
                                                           "Variable 1:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable2boxSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable2box",
                                                           "Variable 2:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable3boxSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable3box",
                                                           "Group by (categorical only):",
                                                           dataset,
                                                           multiple = F))
                                 )),
                        tabPanel("Barplot", 
                                 uiOutput("barplotUI"),
                                 icon = icon("chart-bar"), 
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable1barSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable1bar",
                                                           "Variable 1:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable2barSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable2bar",
                                                           "Variable 2:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable3barSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable3bar",
                                                           "Group by (categorical only):",
                                                           dataset,
                                                           multiple = F))
                                 )),
                        tabPanel("Scatterplot", 
                                 uiOutput("scatterUI"),
                                 icon = icon("braille"), 
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable1scatterSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable1scatter",
                                                           "Variable 1:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable2scatterSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable2scatter",
                                                           "Variable 2:",
                                                           dataset,
                                                           multiple = F))
                                 ),
                                 fluidRow(
                                     column(2,
                                            checkboxInput("variable3scatterSelect",
                                                          "Use Varialbe",
                                                          FALSE)),
                                     column(10,
                                            varSelectInput("variable3scatter",
                                                           "Group by (categorical only):",
                                                           dataset,
                                                           multiple = F))
                                 )),
                        tabPanel("Outliers",
                                 h3("Tests"),
                                 verbatimTextOutput("normTest"),
                                 verbatimTextOutput("normTest2"),
                                 verbatimTextOutput("GrubbsTest"),
                                 verbatimTextOutput("GrubbsTest1"),
                                 verbatimTextOutput("GrubbsTest2"),
                                 verbatimTextOutput("GrubbsTest21"),
                                 verbatimTextOutput("GrubbsTest3"),
                                 verbatimTextOutput("GrubbsTest31"),
                                 verbatimTextOutput("DixonTest"),
                                 verbatimTextOutput("DixonTest2"),
                                 h3("1.5 IQR method outliers"),
                                 verbatimTextOutput("IQRoutliers"))
                                 
                        
            )
        )
    ),
    
    
    
    ###Styling
    tags$head(tags$style("
    #summary {
        color:red; 
        font-size:12px; 
        font-style:italic; 
        overflow-y:scroll; 
        max-height: 200px;
        background: ghostwhite;
    }
    #normTest {
        color:red; 
        font-size: 16px; 
        font-weight: 700; 
        background: ghostwhite;
    }
    #normTest2 {
        color:red; 
        font-size: 16px; 
        font-weight: 700; 
        background: ghostwhite;
    }
    .button-container {
        display: flex;
        flex-direction: row;
        align-items: center;
        h3 {
        margin: 0px !important; 
        }
        
        i {
        margin: 4px !important;
        }
    }
    #trigger {
    display: none;
    }
                         "))
    
    
))

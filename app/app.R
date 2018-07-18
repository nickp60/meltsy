#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# look a global variable cause I'm a lazy duck
X <- NA
ORIGINALX <- X

COLS <- NA
X <-  read.csv("./testdata.csv",
               header = T,
               sep = ",",
               quote = "\"")

updateX <- function(input=input){
  # This is a terrible function to update global variables X and cols
  COLS <<- colnames(X)
  # print(COLS)
  # try avoid setting columns that are not there, should something change
  # for (col in COLS){
  #   if (col in input$factorcols){
  #     if (col %in%  COLS){
  #       X[, col] <- as.factor(X[, col])
  #     }
  #   }
  # }
  X <<- X
  # str(X)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Analysis of Process Data"),
   # Input: Select a file ----
   # Sidebar with a slider input for number of bins 
   fluidRow(
     column( 3, 
   # sidebarLayout(
      #sidebarPanel(
        fileInput("file1", "Choose CSV File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        actionButton("add", "Add UI") ,  
        tags$hr()
        
      ),
      
      # Show a plot of the generated distribution
      column(4,
         dataTableOutput("contents")
      ),
   column(4, offset = 1,
          plotOutput("exploreplot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$exploreplot <- renderPlot({
     req(input$xcol)
     try(
      ggplot(X, aes_string(y=input$ycol, x=input$xcol, color=input$groupcol, fill=input$groupcol, linetype=input$groupcol, shape=input$groupcol))+
       geom_smooth(alpha=.2)+ 
       geom_jitter(width = .1)
   )
   })
   output$contents <- renderDataTable({
     # input$file1 will be NULL initially. After the user selects
     # and uploads a file, head of that data file by default,
     # or all rows if selected, will be shown.
     
     #req(input$file1)
     if (is.null(input$file)){
       updateX(input)
       return(X)
     }
     # when reading semicolon separated files,
     # having a comma separator causes `read.csv` to error
     tryCatch(
       {
         df <- read.csv(input$file1$datapath,
                        header = T,
                        sep = ",",
                        quote = "\"")
       },
       error = function(e) {
         # return a safeError if a parsing error occurs
         stop(safeError(e))
       }
     )
     X <- df
     updateX(input)
     return(X)

   },   options = list(
     pageLength = 5)
   )
   observeEvent(input$add, {
     # for (col in COLS)
     # insertUI(
     #   selector = "#add",
     #   where = "afterEnd",
     #   ui = checkboxGroupInput("factorcols", "Columns that should be treated as factors (ie, non-numeric)", choices = COLS)
     # )
     insertUI(
       selector = "#add",
       where = "afterEnd",
       ui =tags$hr()
     )
     insertUI(
       selector = "#add",
       where = "afterEnd",
       ui = selectInput("groupcol", "group column", choices=colnames(X), selected =colnames(X)[3])
     )
     insertUI(
       selector = "#add",
       where = "afterEnd",
       ui = selectInput("ycol", "y column", choices=colnames(X), selected = colnames(X)[2])
     )
     insertUI(
       selector = "#add",
       where = "afterEnd",
       ui = selectInput("xcol", "x column", choices=colnames(X), selected=colnames(X)[1])
     )
   })
   
   #output$ui <- renderUI(checkboxInput('test', 'checkboxes', colnames(input$file1)))
   
}

# Run the application 
shinyApp(ui = ui, server = server)


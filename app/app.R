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
sPAIRS <- NA
PAIRS <- NA
X <-  read.csv("./data/testdata.csv",
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
  sPAIRS <<- paste(expand.grid(COLS, COLS)$Var1, expand.grid(COLS, COLS)$Var2)
  PAIRS <<- strsplit(sPAIRS, " ")
  
  # str(X)
}
updateX()

run_model<- function(data, x, y, random=c(), fixed=c(), interacting=list(), nested=list()){
  # make a model given a few parameters
  # interacting parameters should be a list of vector pairs, like list(c("A", "B"), c("B", "C"))[
  randoms = ""
  fixeds = ""
  interactings = ""
  nesteds = ""
  fixed = fixed[x != fixed]  # remove x from fixed effects, if its in there
  
  mx <- paste(y, "~", x)
  
  if (length(fixed) != 0){
    fixeds <- paste(fixed, collapse = " + ")
    mx <- paste(mx, fixeds, sep = " + " )
  }
  if (length(random) != 0){
    randoms <- paste("(1", "|", random, ")", collapse = " + ")
    mx <- paste(mx, randoms, sep = " + " )
  }
  if (length(interacting) != 0){
    interaction_strings = c()
    for (i in 1:length(interacting)){
      splitint <- strsplit(interacting[i], " ")[[1]]
      interaction_strings <- c(
        interaction_strings, 
        paste("(", splitint[1], "*",  splitint[2], ")" )
      )
    }
    interactings <- paste(interaction_strings, collapse = " + ")
    mx <- paste(mx, interactings, sep = " + " )
  }
  if (length(nested) != 0){
    nested_strings = c()
    for (i in 1:length(nested)){
      nested_strings <- c(
        nested_strings, 
        paste("(", nested[[i]][1], "/",  nested[[i]][2], ")" )
      )
    }
    nesteds <- paste(nested_strings, collapse = " + ")
    mx <- paste(mx, nested_strings, sep = " + " )
  }
  return(lmer(mx, data=data))
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  titlePanel("Mixed Effects Linear model Timecourses with Shiny"),
  # Input: Select a file ----
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1", "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      tags$hr(),
      selectInput("xcol", "X column/Independent Variable", choices=COLS, selected=COLS[1]),
      selectInput("ycol", "y column/Dependent Variable", choices=COLS, selected = COLS[5]),
      selectInput("groupcol", "group column", choices=COLS, selected = COLS[2]),
      checkboxGroupInput("fixedcols", "Fixed Effects", choices = COLS),
      checkboxGroupInput("randomcols", "Random Effects", choices = COLS), #, multiple = T),
      selectInput("interactingcols", "Interacting Effects", choices = sPAIRS, multiple=T),
      #      checkboxGroupInput("Nestedcols", "Interating Effects", choices = COLS)
      actionButton("add", "Update Model")
    ),
    
      # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Plot", 
          dataTableOutput("contents"),
          plotOutput("exploreplot")
        ),
        tabPanel(
          "model",
          verbatimTextOutput("model")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$exploreplot <- renderPlot({
     req(input$xcol)
     print(input$fixedcols)
     print(length(input$fixedcols))
     print(input$fixedcols[1])
     try(
      if(is.null(input$fixedcols)){
        ggplot(X, aes_string(y=input$ycol, x=input$xcol))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 1){
      ggplot(X, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], shape=input$fixedcols[1]))+
       geom_smooth(alpha=.2)+ 
       geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 2){
        ggplot(X, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], linetype=input$fixedcols[2], shape=input$fixedcols[1]))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 3){
        ggplot(X, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], fill=input$fixedcols[2], linetype=input$fixedcols[3], shape=input$fixedcols[1]))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      }
      
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
   # observeEvent(input$add, {
     # for (col in COLS)
     # insertUI(
     #   selector = "#add",
     #   where = "afterEnd",
     #   ui = checkboxGroupInput("factorcols", "Columns that should be treated as factors (ie, non-numeric)", choices = COLS)
     # )
     #shinyjs::disable("add")
   #   insertUI(
   #     selector = "#add",
   #     where = "afterEnd",
   #     ui = checkboxGroupInput("fixedcols", "Fixed Effects", choices = COLS, selected = input$ycol)
   #   )
   #   insertUI(
   #     selector = "#add",
   #     where = "afterEnd",
   #     ui =tags$hr()
   #   )
   #   insertUI(
   #     selector = "#add",
   #     where = "afterEnd",
   #     ui = selectInput("groupcol", "group column", choices=colnames(X), selected =colnames(X)[3])
   #   )
   #   insertUI(
   #     selector = "#add",
   #     where = "afterEnd",
   #     ui = selectInput("ycol", "y column/Dependent Variable", choices=colnames(X), selected = colnames(X)[2])
   #   )
   #   insertUI(
   #     selector = "#add",
   #     where = "afterEnd",
   #     ui = selectInput("xcol", "X column/Independent Variable", choices=colnames(X), selected=colnames(X)[1])
   #   )
   # })
   ###################################################################
   observeEvent(input$add, {
     output$model <- renderPrint(
       run_model(data=data, x=input$xcol,  y=input$ycol, random=input$randomcols, 
                            fixed=input$fixedcols, interacting=input$interactingcols, nested=list())
     )
     
   #output$ui <- renderUI(checkboxInput('test', 'checkboxes', colnames(input$file1)))
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)


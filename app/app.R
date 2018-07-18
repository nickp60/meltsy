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
library(lme4)
# look a global variable cause I'm a lazy duck
#RESULTS_COUNTER = 0
#RESULTS<-data.frame()
X <- NA
ORIGINALX <- X
MODEL = NA
MODEL_STRING = NA
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

make_model<- function(x, y, random=c(), fixed=c(), interacting=list(), nested=list()){
  # make a model given a few parameters
  # interacting parameters should be a list of vector pairs, like list(c("A", "B"), c("B", "C"))[
  # print(str(df))
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
  return(mx)
}
run_model <- function( model_string, df){
  print(df)
  print(model_string)
  # return(lm(model_string, data=df))
  return(lmer(model_string, data=df, REML=FALSE))
}
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mixed Effects Linear model Time Series"),
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
      # tags$hr(),
      selectInput("xcol", "X column/Independent Variable", choices=COLS, selected=COLS[1]),
      selectInput("ycol", "y column/Dependent Variable", choices=COLS, selected = COLS[5]),
      # selectInput("groupcol", "group column", choices=COLS, selected = COLS[2]),
      checkboxGroupInput("fixedcols", "Fixed Effects", choices = COLS),
      checkboxGroupInput("randomcols", "Random Effects", choices = COLS), #, multiple = T),
      selectInput("interactingcols", "Interacting Effects", choices = sPAIRS, multiple=T)
      #      checkboxGroupInput("Nestedcols", "Interating Effects", choices = COLS)
      # actionButton("add", "Update Model")
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
          verbatimTextOutput("model"),
          actionButton("A_execute_model", "Execute Model"),
          verbatimTextOutput("model_output"),
          dataTableOutput("modelsdf"),
          actionButton("save_model", "Save Model")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(RESULTS = data.frame(), RESULTS_COUNTER=0, MODEL=NA, MODEL_STRING="methane ~ day + treatment + (1|day)")
  output$exploreplot <- renderPlot({
    req(input$xcol)
    # print(input$fixedcols)
    # print(length(input$fixedcols))
    # print(input$fixedcols[1])
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
  
  ###################################################################
  # observeEvent(input$add, {
  #   values$MODEL_STRING <- make_model(x=input$xcol,  y=input$ycol, random=input$randomcols,
  #                                     fixed=input$fixedcols, interacting=input$interactingcols,
  #                                     nested=list())
  #   output$model <- renderText(
  #     values$MODEL_STRING
  #   )
  #   #output$ui <- renderUI(checkboxInput('test', 'checkboxes', colnames(input$file1)))
  # })
 output$model <- renderText(
    make_model(x=input$xcol,  y=input$ycol, random=input$randomcols,
               fixed=input$fixedcols, interacting=input$interactingcols,
               nested=list())
  )

  observeEvent(input$A_execute_model, {
    values$MODEL_STRING <- make_model(x=input$xcol,  y=input$ycol, random=input$randomcols,
               fixed=input$fixedcols, interacting=input$interactingcols,
               nested=list())
    print("hi hop")
    thismod <- values$MODEL_STRING
    values$MODEL <- lmer(values$MODEL_STRING, 
                         data = data.frame(X))
    
    output$model_output <- renderPrint({
      return(values$MODEL)
    })
  })
  
  # observeEvent(input$execute_model, {
  #   print("hi hop")
  #   print(values$MODEL_STRING)
  #   MODEL_STRING <<- values$MODEL_STRING
  #   print(MODEL_STRING)
  #   print(values$MODEL)
  #   values$MODEL <- lmer(MODEL_STRING, data = X)
  #   
  #   output$model_output <- renderPrint({
  #     return(values$MODEL)
  #   })
  #   #output$ui <- renderUI(checkboxInput('test', 'checkboxes', colnames(input$file1)))
  # })
  observeEvent(input$save_model, {
    values$RESULTS_COUNTER <- values$RESULTS_COUNTER + 1
    row <- data.frame(
      id = values$RESULTS_COUNTER,
      xcol = input$xcol,
      ycol =  input$ycol,
      fixedcols =  paste(input$fixedcols, collapse = ","),
      randomcols = paste(input$randomcols, collapse = ","),
      interactingcols = paste(input$interactingcols, collapse = ","),
      nestedcols = paste(input$nestedcols, collapse = ","),
      model_string = values$MODEL_STRING,
      model = "run_model(df = X, model_string = MODEL_STRING)",
      stringsAsFactors = F)
    values$RESULTS <- rbind(values$RESULTS, row)
    # output$model <- renderPrint(
    #   run_model(data=X, x=input$xcol,  y=input$ycol, random=input$randomcols, 
    #             fixed=input$fixedcols, interacting=input$interactingcols, nested=list())
    # )
    print("postupdated")
  })
  output$modelsdf <- renderDataTable({
    values$RESULTS
  })
  # output$model_output <- renderDataTable({
  #   values$MODEL
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)


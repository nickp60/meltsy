library(shiny)
library(ggplot2)
library(lme4)

# look global variables cause I'm a lazy duck
X <- NA
ORIGINALX <- X
MODEL = NA
COLS <- NA
sPAIRS <- NA
PAIRS <- NA
X <-  read.csv("./data/testdata.csv",
               header = T,
               sep = ",",
               quote = "\"")

updateX <- function(){
  # This is a terrible function to update global variables X and cols
  COLS <<- colnames(X)
  sPAIRS <<- paste(expand.grid(COLS, COLS)$Var1, expand.grid(COLS, COLS)$Var2)
  PAIRS <<- strsplit(sPAIRS, " ")
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
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
#  titlePanel("Mixed Effects Linear model Time Series"),
  # Input: Select a file ----
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "userfile", "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      # tags$hr(),
      selectInput("xcol", "X column/Independent Variable", choices=COLS, selected=COLS[1]),
      selectInput("ycol", "Y column/Dependent Variable", choices=COLS, selected = COLS[5]),
      checkboxGroupInput("fixedcols", "Fixed Effects", choices = COLS),
      checkboxGroupInput("randomcols", "Random Effects", choices = COLS), #, multiple = T),
      selectInput("interactingcols", "Interacting Effects", choices = sPAIRS, multiple=T)
      #      checkboxGroupInput("Nestedcols", "Interating Effects", choices = COLS)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel(
          "?. Help", 
          includeMarkdown("../README.md")
          
        ),
        tabPanel(
          "1. Plot", 
          dataTableOutput("contents")
#          plotOutput("exploreplot")
        ),
        tabPanel(
          "2. Model",
          verbatimTextOutput("model"),
          actionButton("A_execute_model", "Execute Model"),
          verbatimTextOutput("model_output"),
          actionButton("save_model", "Save Model"),
          dataTableOutput("modelsdf")
        ),
        tabPanel(
          "3. Compare",
          dataTableOutput("modelsdf2"),
          selectInput("model1", "A: ID Model ID from first column of list above ", choices = c(1:100), multiple=F, selected = 1),
          selectInput("model2", "B: ID Model ID from first column of list above ", choices = c(1:100), multiple=F, selected = 2),
          actionButton("run_anova", "Compare models"),
          verbatimTextOutput("model_compare_output")
#          htmlOutput("model_compare_output")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues(DF=NA, PAIRS=NA, RESULTS = data.frame(), RESULTS_COUNTER=0, MODEL=NA, MODEL_STRING=NA, MODEL_LIST=list())
  output$exploreplot <- renderPlot({
    req(values$DF)
    # print(input$fixedcols)
    # print(length(input$fixedcols))
    # print(input$fixedcols[1])
    try(
      if(is.null(input$fixedcols)){
        ggplot(values$DF, aes_string(y=input$ycol, x=input$xcol))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 1){
        ggplot(values$DF, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], shape=input$fixedcols[1]))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 2){
        ggplot(values$DF, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], linetype=input$fixedcols[2], shape=input$fixedcols[1]))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      } else if(length(input$fixedcols) == 3){
        ggplot(values$DF, aes_string(y=input$ycol, x=input$xcol, color=input$fixedcols[1], fill=input$fixedcols[2], linetype=input$fixedcols[3], shape=input$fixedcols[1]))+
          geom_smooth(alpha=.2)+ 
          geom_jitter(width = .1)
      }
      
    )
    
  })
  dataframe <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    if (is.null(input$userfile)){
      print("loading test data")
      values$DF <- X
      updateX()
      return(values$DF)
    } else{
      df <- read.csv(input$userfile$datapath,
                     header = T,
                     sep = ",",
                     quote = "\"")
      
      print("loading user data!")
      values$DF <- df
      updateX()
      return(df)
      
    }
  })
  
  
  
  output$contents <- renderDataTable({
    dataframe()
  }, options = list(
    pageLength = 5)
  )
  # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
  # #req(input$file1)
  # print(input$file)
  # if (is.null(input$file)){
  #   print("loading boring")
  #   values$DF <- X
  #   updateX(input)
  #   return(values$DF)
  # } else {
  #   tryCatch(
  #     {
  #       df <- read.csv(input$file1$datapath,
  #                      header = T,
  #                      sep = ",",
  #                      quote = "\"")
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   print("uploaded!")
  #   values$DF <- df
  #   updateX()
  #   return(values$DF)
  #   
  # }
  # when reading semicolon separated files,
    # # having a comma separator causes `read.csv` to error
    # tryCatch(
    #   {
    #     df <- read.csv(input$file1$datapath,
    #                    header = T,
    #                    sep = ",",
    #                    quote = "\"")
    #   },
    #   error = function(e) {
    #     # return a safeError if a parsing error occurs
    #     stop(safeError(e))
    #   }
    # )
    # print("uploaded!")
    # values$DF <- df
    # updateX()
    # return(values$DF)
    
  
 output$model <- renderText(
    make_model(x=input$xcol,  y=input$ycol, random=input$randomcols,
               fixed=input$fixedcols, interacting=input$interactingcols,
               nested=list())
  )

  observeEvent(input$A_execute_model, {
    values$MODEL_STRING <- make_model(x=input$xcol,  y=input$ycol, random=input$randomcols,
                                      fixed=input$fixedcols, interacting=input$interactingcols,
                                      nested=list())
    thismod <- values$MODEL_STRING
    if (is.null(input$randomcols) && is.null(input$interactingcols) && is.null(input$randomcols)){
      values$MODEL <- lm(values$MODEL_STRING, 
                         data = data.frame(X))
    } else{
      values$MODEL <- lmer(values$MODEL_STRING, 
                           data = data.frame(X), REML=FALSE)
    }
    output$model_output <- renderPrint({
      return(values$MODEL)
    })
  })
  
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
      stringsAsFactors = F)
    values$MODEL_LIST[values$RESULTS_COUNTER] <- values$MODEL
    values$RESULTS <- rbind(values$RESULTS, row)
  })
  output$modelsdf <- renderDataTable({
    values$RESULTS
  }) 
  # cant have this displayed twice?
  output$modelsdf2 <- renderDataTable({
    values$RESULTS
  })
  observeEvent(input$run_anova, {
    model1 <-  values$MODEL_LIST[[as.numeric(input$model1)]]
    model2 <-  values$MODEL_LIST[[as.numeric(input$model2)]]
    aov <- anova(model1, model2)
    print(aov)
    
    output$model_compare_output <- renderPrint({
      return(aov)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


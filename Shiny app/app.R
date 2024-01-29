#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ISLR)
library(datasets)
#install.packages("plotly")
library(plotly)

setwd("E:/CS/R/STAT6210/FinalProject/R_DataSciences_Logistics_Regresion")


if (!require(Logistic.Regression)) install.packages("E:/CS/R/STAT6210/FinalProject/Logistic.Regression_0.1.1.tar.gz",
                                          repos = NULL, type = "source")
library(Logistic.Regression)


if (!require(rsconnect)) install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name='leshanzhao', token='BBAEE0CC92FD2C6C5AC0D51BFA61F740', secret='2T8+uoUKKHzmh73qzsxS6FuSuYDuL3joSfpph7A8')









# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Metrics with different cut-off values"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("step", "Step Length:",
                  min = 0.01,
                  max = 0.1,
                  value = 0.1,),
      sliderInput("cutoff", "Current Cut-off value:",
                  min = 0.2,
                  max = 0.8,
                  value = 0.5,),
      sliderInput("bootstrap", "Number of bootstraps:",
                  min = 5,
                  max = 35,
                  value = 5),

      checkboxGroupInput("Metrics", "Metrics to show:",
                         c("Prevalence" = "Prevalence",
                           "Accuracy" = "Accuracy",
                           "Sensitivity" = "Sensitivity",
                           "Specificity" = "Specificity",
                           "False Discovery Rate" = "False.Discovery.Rate",
                           "Diagnostic Odds Ratio" = "Diagnostic.Odds.Ratio"),
                         selected="Prevalence"),
      checkboxGroupInput("Predictor", "Predictor to fit:",
                         c("Lag1" = "Lag1",
                           "Lag2" = "Lag2",
                           "Lag3" = "Lag3",
                           "Lag4" = "Lag4",
                           "Lag5" = "Lag5",
                           "Volume" = "Volume"),
                         selected=c("Lag1" = "Lag1",
                                    "Lag2" = "Lag2",
                                    "Lag3" = "Lag3")),
      tableOutput("data"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      p("Note: This dataset contains a large amount of data, adjusting the number of bootstraps and predictor will cause a logistic regression to be re-fitted to the dataset, this may take a long time, please be patient."),
      tabsetPanel(
        tabPanel("Metrics list", tableOutput("Metrics.list")),
        tabPanel("Metrics Plot", plotOutput("plot")),
        tabPanel("Confusion Matrix", tableOutput("Confusion.Matrix"), tableOutput("Current.Metrics")),
        tabPanel("Logistic Regression model",tableOutput("Beta"),textOutput("Note"))
      )
    )
  )
)

Metrics.list <- function(predictor,metrics,step.1) {
  table <- metrics.table(X=Weekly[,predictor],Y=Weekly$Direction,step=step.1)
  table.2 <- table[,c("Cut-off value",metrics)]
  table.2 <- as.matrix(table.2)
  colnames(table.2) <- c("Cut-off value",metrics)
  return(table.2)
}

Metrics.plot <- function(predictor,metrics,step.1) {
  metrics <- as.matrix(metrics)
  table <- metrics.table(X=Weekly[,predictor],Y=Weekly$Direction,step=step.1)
  n=length(metrics)
  table.2 <- table[,c("Cut-off value",metrics)]
  table.2 <- as.data.frame(table.2)
  row <- nrow(table.2)
  colnames(table.2) <- c("Cut-off value",metrics)

  image <- data.frame("cutoff"=NA,"value"=NA)[-1,]
  if(ncol(table.2)>=2){
    for(i in 2:(n+1)){
      temp <- table.2[,c(1,i)]
      colnames(temp) <- c("cutoff","value")
      image=rbind(image,temp)
    }
    image=cbind(image,data.frame(group=rep(metrics,each=row)))
  }else{
    image=cbind(table.2[,1],rep(0,row),data.frame(group=rep("NA",row)))
  }
  plot <- ggplot(data=image,aes(x=cutoff,y=value,color=group))+geom_line(size=1)
  return(plot)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Confusion.Matrix <- renderTable({
    predict <- logistic_pred(Beta.hat(Weekly[,input$Predictor],Weekly$Direction), Weekly[,input$Predictor])
    actual.value <- as.numeric(Weekly$Direction)-1
    Analysis <- confusion.matrix(predict, actual.value, cutoff=input$cutoff)
    Yi <- as.numeric(Weekly$Direction)-1
    level <- as.character(unique(Weekly$Direction))
    names(level) <- unique(Yi)
    matrix <- matrix(Analysis$matrix,nrow=2,ncol=2)
    rownames(matrix) <- c(paste("Actual.", level["1"],sep=""),
                          paste("Actual.", level["0"],sep=""))
    colnames(matrix) <- c(paste("Predicted.", level["1"],sep=""),
                          paste("Predicted.", level["0"],sep=""))
    matrix
  },rownames = TRUE)

  output$Current.Metrics <- renderTable({
    predict <- logistic_pred(Beta.hat(Weekly[,input$Predictor],Weekly$Direction), Weekly[,input$Predictor])
    actual.value <- as.numeric(Weekly$Direction)-1
    Analysis <- confusion.matrix(predict, actual.value, cutoff=input$cutoff)
    output <- t(Analysis$metrics)
    output
  })

  output$Beta <- renderTable({
    Boot <- round(input$bootstrap,0)
    Summary <- logistic.regression(X=Weekly[,input$Predictor],Y=Weekly$Direction,B=Boot)
    Beta <- Summary$Beta
    Beta
  },rownames = TRUE)

  output$Metrics.list <- renderTable({
    Metrics.list(predictor=input$Predictor,metrics=input$Metrics,step.1=input$step)
  })

  output$plot <- renderPlot({
    Metrics.plot(predictor=input$Predictor,metrics=input$Metrics,step.1=input$step)
  })

}



# Run the application
shinyApp(ui = ui, server = server)

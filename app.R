#####
#Bugger App
#####



#Initializing the required packages
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(DT)


#Supplying example datan options
data(iris)
data(mtcars)

#make some factors
#easier to let ggplot2 control plotting (color, fill) based on type
uvals<-sapply(mtcars,function(x){length(unique(x))})
mtcars<-map_if(mtcars,uvals<4,as.factor) %>%
  as.data.frame()


#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)


# UI for app
ui<-(fluidPage(
  
  # title
  headerPanel("The Bugger Data Visualization App"),
  
  #input
  sidebarPanel(
    # SlidebarPanel for first and secound tabs
    conditionalPanel(condition = "$('li.active a').first().html()==='Data Visualization'",
                     h2("About this App:"),
                     p("This app can take your (or sample) data, displays a series of ggplot objects of your choosing, preform an ANOVA test, and construct linear model of your data."),
                     p("If you would like to test the app before using your own data, you can choose from two sample options: mtcars & iris."),
                     p("You also have the option of selecting from three plot types: Boxplot, Histogram, Bar graph, and a Scatter plot"),
                     br()
    ),
  
    conditionalPanel(condition = "$('li.active a').first().html()==='Plot'",
                     h2("Plots, plots, and more plots!!:"),
                     p("Plot Choice:"),
                     p("In this tab, you can see the output based on the plot you chose."),
                     br()
    ),
    
    # Input: Select a file ----
    fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    # Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons("sep", "Separator",
                 choices = c(Semicolon = ";",
                             Comma = ",",
                             Tab = "\t"),
                 selected = ","),
    # Horizontal line ----
    tags$hr(),
    
    
    # Input: Select what to display
    selectInput("dataset","Data:",
                choices =list(iris = "iris", mtcars = "mtcars",
                              uploaded_file = "inFile"), selected=NULL),
    selectInput("variable1","Variable 1:", choices = NULL),
    selectInput("variable2","Variable 2:", choices = NULL),
    selectInput("variable3","Variable 3:", choices = NULL),
    selectInput("plot.type","Plot Type:",
                list(boxplot = "boxplot", histogram = "histogram", bar = "bar", scatter = "scatter")
    ),
    checkboxInput("show.points", "show points", TRUE)
  ),
  
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Data Visualization", DTOutput('datavis')),
      tabPanel('Plot', plotOutput('plots')), # The name "Scatter Plot" is the name of the tab. 'scatter' is the part of the plot name to tell are to build the plot under Scatter Plot tab
      tabPanel('Linear Model', verbatimTextOutput('linear')),
      tabPanel('ANOVA', tableOutput('aovSummary'))
    ),
    h3(textOutput("caption")),
    uiOutput("plot") # depends on input
  )
))


# shiny server side code for each call
server<-(function(input, output, session){
  
  #update group and
  #variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "variable1", choices = var.opts)
    updateSelectInput(session, "variable2", choices = var.opts)
    updateSelectInput(session, "variable3", choices = var.opts)
  })
  
  output$caption<-renderText({
    switch(input$plot.type,
           "boxplot" 	= 	"Boxplot",
           "histogram" =	"Histogram",
           "bar" 		=	"Bar graph",
           "scatter" = "Scatter Plot")
  })
  
  
  output$plot <- renderUI({
    plotOutput("p")
  })
  
  #get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj<-list(data=get(input$dataset),
              variable1=input$variable1,
              variable2=input$variable2
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$variable1, obj$variable2) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    
    obj
    
  })
  #printing data to first tab
  #printing data to first tab
  output$datavis <- renderDT({
    data.obj <- as.data.frame(get_data()) 
    datatable(data.obj)})
  
  #plotting function using ggplot2
  output$plots <- renderPlot({
    
    plot.obj<-get_data()
    
    #conditions for plotting
    if(is.null(plot.obj)) return()
    
    #make sure variable and group have loaded
    if(plot.obj$variable1 == "" | plot.obj$variable2 =="") return()
    
    #plot types
    plot.type<-switch(input$plot.type,
                      "boxplot" 	= geom_boxplot(),
                      "histogram" =	geom_histogram(alpha=0.5,position="identity"),
                      "bar" 		=	geom_bar(position="dodge",),
                      "scatter" = geom_point()
    )
    
    
    if(input$plot.type=="boxplot" || input$plot.type=="scatter")	{		#control for 1D or 2D graphs
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable2,
                  y 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2 # let type determine plotting
                )
      ) + plot.type
      
      if(input$show.points==TRUE)
      {
        p<-p+ geom_point(color='black',alpha=0.5, position = 'jitter')
      }
      
    } else {
      
      p<-ggplot(plot.obj$data,
                aes_string(
                  x 		= plot.obj$variable1,
                  fill 	= plot.obj$variable2,
                  variable2 	= plot.obj$variable2
                  #color 	= as.factor(plot.obj$variable2)
                )
      ) + plot.type
    }
    
    p<-p+labs(
      fill 	= input$variable2,
      x 		= "",
      y 		= input$variable1
    )  +
      .theme
    print(p)
  })
  
  #Linear model of data
  
  output$linear <- renderPrint({
    
    lm.obj <- get_data()
    
    fit <- lm(lm.obj$data[,input$variable1] ~ lm.obj$data[,input$variable2])
    names(fit$coefficients) <- c("Intercept", input$var2)
    summary(fit)
    
  })
  
  # ANOVA

  output$aovSummary = renderTable({
    
  an.obj <- get_data()
  Variable1 <- an.obj$data[,input$variable1]
  Variable2 <- an.obj$data[,input$variable2]
  Variable3 <- an.obj$data[,input$variable3]
  rev.aov <- anova(lm(Variable1 ~ Variable2 + Variable3 + Variable2:Variable3))
  rev.aov
   }, rownames = TRUE, colnames = TRUE)
 
  
  # set uploaded file
  upload_data<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  observeEvent(input$file1,{
    inFile<<-upload_data()
  })
})

# Create Shiny app ----
shinyApp(ui, server)

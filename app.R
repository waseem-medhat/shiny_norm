library(shiny)
library(nortest)

source('prep.R')
source('histogram.R')
source('normal_qq.R')

ui <- fluidPage(
  
  tags$style(
    'body {
      margin:30px;
    }'
    ),
  
  titlePanel('Test distributions for normality'),
  actionLink('main_help', 'What is this about?', icon('question-circle')),
  fluidRow(br()),
  
  fluidRow(
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('1: load the data'),
        selectInput(
          'builtin_dataset',
          'Choose a built-in dataset:',
          choices = set_choices,
          selected = 'mtcars'
        )
        
      )
      
    ),
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('2: choose a variable'),
        uiOutput('var_ui')
        
      )
      
    ),
    
    column(
      width = 4,
      
      wellPanel(
        
        h3('3: analyze'),
        actionButton('execute', 'Execute', style = 'margin:20px 10px;'),
        actionButton('clear', 'Clear', style = 'margin:20px 10px;')
        
      )
      
    )
    
  ),
  
  
  wellPanel(
    h3('Tests'),
    fluidRow(
      column(4, h4('Histogram'), fluidRow(br()), plotOutput('hist')),
      column(4, h4('Q-Q plot'), fluidRow(br()), plotOutput('qq')),
      column(
        4,
        h4('Goodness-of-fit tests'),
        fluidRow(br()),
        tableOutput('gof'),
        fluidRow(br()),
        h4('Sample size'),
        textOutput('n')
      )
    )
  )
  
)


server <- function(input, output) {
  
  df <- reactive({ get(input$builtin_dataset) })
  
  output$var_ui <- renderUI({
    selectInput(
      'var',
      'Available (numeric) variables',
      choices = names(df()[sapply(df(), is.numeric)])
    )
  })
  
  df_var <- reactive({ na.omit(df()[, input$var]) })
  
  gof_p <- reactive({
    c(
      lillie.test(df_var())$p.value,
      shapiro.test(df_var())$p.value,
      ifelse(length(df_var()) <= 7, NA, ad.test(df_var())$p.value)
    )
  })
  
  
  r_hist <- eventReactive(input$execute, {
    histogram(df_var())
  })
  
  r_qq   <- eventReactive(input$execute, {
    normal_qq(df_var())
  })
  
  r_gof    <- eventReactive(input$execute, {
    data.frame(
      x = c('Kolmogorov-Smirnov', 'Shapiro-Wilk', 'Anderson-Darling'),
      y = format(gof_p(), digits = 3)
    )
  })
  
  r_n      <- eventReactive(input$execute, {
    length(df_var()) 
  })
  
  observeEvent(input$execute, {
    output$hist <- renderPlot({ r_hist() })
    output$qq   <- renderPlot({ r_qq()  })
    output$gof  <- renderTable({ r_gof() }, colnames = FALSE)
    output$n <- renderText({ r_n() })
  })
  
  observeEvent(input$clear, {
    output$hist <- NULL
    output$qq   <- NULL
    output$gof  <- NULL
    output$n <- NULL
  })
  
}


shinyApp(ui, server)
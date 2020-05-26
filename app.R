library(shiny)
library(nortest)
library(shinyhelper)
library(shinythemes)
library(magrittr)
library(foreign)

source('prep.R')
source('histogram.R')
source('normal_qq.R')

ui <- fluidPage(
  
  tags$style(
    
    'body {
      margin: 30px;
    }
    
    .action-button {
      width: 90px;
      margin: 10px auto;
      display: block;
      font-size: 1.1em;
      padding: 5px 10px;
      border-radius: 5px;
      text-align: left;
      background-color: darkslateblue;
      color: white;
      /* TODO: style on hover */
    }
    
    .progress {
      margin: 0;
    }
    
    .col-sm-5 {
      padding: 0px 25px;
    }
    
    #main_div .fa {     /* main helper */
      font-size: 1.7em;
    }
    
    td {               /* gof table */
      font-size: 1.2em
    }
    
    #n {
      font-size: 1.8em
    }'
    
  ),
  
  titlePanel('Test distributions for normality') %>%
    helper(content = 'main', fade = TRUE, id = 'main_div', colour = accent),
  fluidRow(br()),
  
  wellPanel(
    fluidRow(
      
      column(
        width = 2,
        radioButtons(
          'source',
          'Select data source',
          choices = c(
            'Built-in' = 'builtin',
            'Upload' = 'upload'
          )
        )
      ),
      
      column(width = 4,  uiOutput('import_ui')),
      
      column(
        width = 4,
        
        uiOutput('var_ui') %>% 
          helper(content = 'variables', fade = TRUE, colour = accent)
        
      ),
      
      column(
        width = 2,
        
          actionButton('execute', 'Analyze', icon = icon('angle-right')),
          actionButton('clear', 'Clear', icon = icon('angle-right'))
        
      )
      
    )
  ),
  
  
  wellPanel(
    
    h3('Analysis') %>%
      helper(content = 'analyze', fade = TRUE, colour = accent),
    
    em(textOutput('loaded'), br()),
    
    fluidRow(
      column(4, h4('Histogram'), plotOutput('hist')),
      column(4, h4('Q-Q plot'), plotOutput('qq')),
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
  
  observe_helpers()
  observeEvent(input$help_main, {
    showModal(modalDialog(help_main_text, title = 'Why test normality?'))
  })
  
  observeEvent(input$source, {
    df <- NULL
    output$hist <- NULL
    output$qq   <- NULL
    output$gof  <- NULL
    output$n <- NULL
    output$loaded <- NULL
  })
  
  output$import_ui <- renderUI({
    
    if (input$source == 'builtin') {
      
      selectInput(
        'builtin_dataset',
        'Choose a built-in dataset:',
        choices = set_choices,
        selected = 'mtcars'
      ) %>%
        helper(content = 'load', fade = TRUE, colour = accent)
      
    } else {
      
      div(
        fileInput('uploaded_dataset', 'Upload file'),
        textOutput('uploaded_text')
      )
      
    }
    
  })
  
  df <- reactive({
    
    if (input$source == 'builtin' & !is.null(input$builtin_dataset))  {
      get(input$builtin_dataset)
    } else if (input$source == 'upload' & !is.null(input$uploaded_dataset)) {
      read.spss(input$uploaded_dataset$datapath, to.data.frame = TRUE)
      # TODO: support more data formats
    }
    
  })
  
  output$uploaded_text <- renderText({
    if (!is.null(input$uploaded_dataset))
      paste('Loaded:', input$uploaded_dataset$name)
  })
  
  output$var_ui <- renderUI({
   
    selectInput(
      'var',
      'Available (numeric) variables',
      choices = sort( names(df()[sapply(df(), is.numeric)]) )
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
  
  
  r_loaded <- eventReactive(input$execute, {
    sprintf(
      'Variable: %s (%s)',
      input$var,
      ifelse(
        input$source == 'builtin',
        input$builtin_dataset,
        input$uploaded_dataset$name # TODO: remove file extension
      )
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
      y = format(gof_p(), digits = 2)
    )
  })
  
  r_n      <- eventReactive(input$execute, {
    length(df_var()) 
  })
  
  observeEvent(input$execute, {
    if (!is.null(df_var())) {
      output$hist <- renderPlot({ r_hist() })
      output$qq   <- renderPlot({ r_qq()  })
      output$gof  <- renderTable({ r_gof() }, colnames = FALSE)
      output$n <- renderText({ r_n() })
      output$loaded <- renderText({ r_loaded() })
    }
  })
  
  observeEvent(input$clear, {
    output$hist <- NULL
    output$qq   <- NULL
    output$gof  <- NULL
    output$n <- NULL
    output$loaded <- NULL
  })
  
}


shinyApp(ui, server)
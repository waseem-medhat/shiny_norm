library(shiny)
library(nortest)
library(shinyhelper)
library(shinythemes)
library(magrittr)

source('prep.R')
source('histogram.R')
source('normal_qq.R')

ui <- fluidPage(
  theme = shinytheme('spacelab'),
  
  tags$style(
    
    'body {
      margin: 30px;
    }
    
    #btn-container {
      padding: 5% 0%;
      margin: auto;
      display: block;
    }
    
    .action-button {
      width: 160px;
      margin: 100px 10px 10px;
      color: white;
      font-size: 1.1em;
      background-color: darkcyan;
      padding: 5px 10px;
      border-radius: 5px;
    }
    
    .col-sm-5 {
      padding: 0px 25px;
    }
    
    #main_div .fa {
      font-size: 1.7em;
    }
    
    td {
      font-size: 1.2em
    }
    
    #n {
      font-size: 1.8em
    }'
    
  ),
  
  titlePanel('Test distributions for normality') %>%
    helper(content = 'main', fade = TRUE, id = 'main_div', colour = 'darkcyan'),
  fluidRow(br()),
  
  wellPanel(
    fluidRow(
      
      column(
        width = 4,
        
        h3('Load the data') %>%
          helper(content = 'load', fade = TRUE, colour = 'darkcyan'),
        selectInput(
          'builtin_dataset',
          'Choose a built-in dataset:',
          choices = set_choices,
          selected = 'mtcars'
        )
        
      ),
      
      column(
        width = 4,
        
        h3('Choose a variable') %>% 
          helper(content = 'variables', fade = TRUE, colour = 'darkcyan'),
        uiOutput('var_ui')
        
      ),
      
      column(
        width = 4,
        
        h3('Execute'),
        div(
          id = 'btn-container',
          actionLink('execute', 'Analyze', icon = icon('angle-right')),
          actionLink('clear', 'Clear', icon = icon('angle-right'))
        ) 
        
      )
      
    )
  ),
  
  
  wellPanel(
    h3('Analyze') %>%
      helper(content = 'analyze', fade = TRUE, colour = 'darkcyan'),
    fluidRow(
      column(
        4,
        h4('Histogram'),
        fluidRow(br()),
        plotOutput('hist')
      ),
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
  
  observe_helpers()
  observeEvent(input$help_main, {
    showModal(modalDialog(help_main_text, title = 'Why test normality?'))
  })
  
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
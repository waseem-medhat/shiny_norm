library(shiny)

source('prep.R')
source('histogram.R')

ui <- fluidPage(
  
  titlePanel('Test distributions for normality'),
  actionLink('main_help', 'What is this about?', icon('question-circle')),
  fluidRow(br()),
  
  h3('Step 1: load the data'),
  fluidRow(
    column(
      5,
      selectInput(
        'builtin_datasets',
        'Choose a built-in dataset:',
        choices = set_choices,
        selected = 'iris'
      )
    ),
    column(2, strong('or...')),
    column(5, fileInput('upload_dataset', 'Upload your own dataset:'))
  ),
  
  h3('Step 2: choose a variable'),
  fluidRow(
    column(5, textInput('vars', 'Enter variable name:')),
    column(7, strong('Available variables:'), textOutput('var_names'))
  ),
  
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


server <- function(input, output) {
  
  output$var_names <- renderText('hello')
  
  output$hist <- renderPlot({ histogram(rnorm(10)) })
  
  output$qq <- renderPlot({ normal_qq(rnorm(10))  })
  
  output$gof <- renderTable({
    data.frame(
      x = c('Kolmogorov-Smirnov', 'Shapiro-Wilk', 'Anderson-Darling'),
      y = 1:3
    )
  },
  colnames = FALSE
  )
  
}


shinyApp(ui, server)
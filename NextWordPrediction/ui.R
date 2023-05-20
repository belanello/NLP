library(shiny)
library(shinybusy)
library(stringr)
library(shinyWidgets)


fluidPage(
  
  # Use once in UI
  add_busy_spinner(spin = "double-bounce", timeout = 1500, position = "full-page"),
  
  titlePanel("Word Predictor"),
  theme = shinythemes::shinytheme("simplex"),
  tabsetPanel(
    tabPanel("Interpolation",
             tags$br(),
             tags$div(tags$strong("This model uses Interpolation and shows 3 
                                   options that have highest 
                                   probabilities.")),
             tags$br(),
             textAreaInput("sentence", 
                           "Enter texts.", 
                            value = "What's your favorite", 
                            width = "80%"),
             checkboxGroupButtons("options",
                                   label = "", 
                                   choiceNames = c("", "", ""),
                                   choiceValues = c("", "", ""),
                                   status = "primary"),
  
             actionButton("clear", "CLEAR", style = "unite")
  
    ),

    tabPanel("Stupid back-off",
             tags$br(),
             tags$div(tags$strong("This model uses Stupid Back-off.")),
             tags$br(),
             textAreaInput("sentence2", 
                           "Enter texts.",
                           value = "What's your favorite",
                           width = "80%"),
             
             checkboxGroupButtons("options2",
                                  label = "",
                                  choiceNames = c("", "", ""),
                                  choiceValues = c("", "", ""),
                                  status = "primary"),
             
             actionButton("clear2", "CLEAR", style = "unite")
    ),
    
    tabPanel("Doc",
             fluidRow(
             column(1,),
             column(10,
             tags$br(),
             h4("Description"),
             
             p("This App shows you the next word options as you type texts.
                    When you press one of the options, the word will be added to
                    your texsts. To clear the inputs, press 'CLEAR' button."),
             
             tags$ol(tags$li("Interpolation: interpolate all N-grams and choose 
                             3 words that have highest probabilities."),

                     tags$li("Stupid back-off: Starts from 4-gram to find 3 words.
                              If there's no same history, back-off to lower order
                              N-gram.")),
             
             h4("Algorithm"),
             p("Interpolation of N-gram & 
                     Stupid back-off"),
             h4("Training corpora"),
             p("News/Twitter posts/Blogs (3,868,514 sentences)"),
             p("Vocabulary used: 31,693 words (96% coverage)"),
             p("N-gram used : up to 4-grams(at least appeared 5 times)"),
             h4("Reference"),
             tags$a(href = "https://web.stanford.edu/~jurafsky/slp3/3.pdf",
                    "Speech and Language Processing :
                    N-gram Language Model / Daniel Jurafsky & James H. Martin",
                    target="_blank"),
             h4("Author"),
             p("Ayako Nagao")
             ),
             column(1,)
             )
             
    )
  )
)

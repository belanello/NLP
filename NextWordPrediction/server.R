library(shiny)
source("func.R")

function(input, output, session) {
  
  # Return 3 highest prob words whenever input changes
  rval_words <- reactive({
    # lowercase > trim white space > add BOS to input
    sentence <- str_trim(input$sentence, side = "both")
    sentence <- str_glue("BOS BOS ", sentence)
    # if input is length zero return empty strings(to avoid error)
    if (input$sentence == ""){
      c("   ", "   ", "   ")
    }else{
      # split into words
      words <- str_split(sentence, " ", simplify = TRUE)
      # check if input words are in vocabulary list
      # if not, change it to UNK
      words <- checkUNK(words)
      n <- length(words)
      # get last 3words, 2words, and 1word
      params <- c(str_c(words[c(n-2,n-1,n)], collapse = " "), 
                  str_c(words[c(n-1,n)], collapse = " "), 
                  words[n])
      print(params)
      # obrain top3 words
      getNextWords(params)
    }
    
  })
  
  observeEvent(rval_words(), {
    # update the choices labels and values
    choices <- as.vector(rval_words())
    updateCheckboxGroupButtons(inputId='options', 
                               session = getDefaultReactiveDomain(),
                               choiceNames = choices,
                               choiceValues = choices)
    
  })
  
  observeEvent(input$options,{
    # If one of the options is pressed
    # input will be updated
    correct <- input$options
    # to avoid user to input multiple words
    updateCheckboxGroupButtons("options", 
                               session = getDefaultReactiveDomain(),
                               disabled = TRUE)
    
    new_sentence <- paste(str_trim(input$sentence, side = "both"), correct)
    updateTextAreaInput("sentence", 
                        session = getDefaultReactiveDomain(),
                        value = new_sentence)
    
  })
  
  # clear all the inputs
  observeEvent(input$clear,{
    
    updateTextAreaInput("sentence", 
                        session = getDefaultReactiveDomain(),
                        value = "")
    
  })
  
  
 
  # ==============================================================================
  # Model 2
  # Stupid backoff
  rval_words2 <- reactive({
    # lowercase > trim white space > add BOS to input
    sentence <- str_trim(input$sentence2, side = "both")
    print(sentence)
    sentence <- str_glue("BOS BOS ", sentence)
    print(sentence)
    # if input is length zero return empty strings(to avoid error)
    if (input$sentence2 == ""){
      c("   ", "   ", "   ")
    }else{
      # split into words
      words <- str_split(sentence, " ", simplify = TRUE)
      # check if input words are in vocabulary list
      # if not, change it to UNK
      words <- checkUNK(words)
      n <- length(words)
      # get last 3words, 2words, and 1word
      params <- c(str_c(words[c(n-2,n-1,n)], collapse = " "),
                  str_c(words[c(n-1,n)], collapse = " "),
                  words[n])
      print(params)
      # obtain sampled 3 words according to prob distribution
      getNextWords2(params)
    }
    
  })
  
  observeEvent(rval_words2(), {
    # update the choices labels and values
    choices <- as.vector(rval_words2())
    updateCheckboxGroupButtons(inputId='options2',
                               session = getDefaultReactiveDomain(),
                               choiceNames = choices,
                               choiceValues = choices)
    
  })
  
  observeEvent(input$options2,{
    # If one of the options is pressed
    # input will be updated
    correct <- input$options2
    # to avoid user to input multiple choices
    updateCheckboxGroupButtons("options2",
                               session = getDefaultReactiveDomain(),
                               disabled = TRUE)
    new_sentence <- paste(str_trim(input$sentence2, side = "both"), correct)
    updateTextAreaInput("sentence2",
                        session = getDefaultReactiveDomain(),
                        value = new_sentence)
    
  })

  # clear all the inputs
  observeEvent(input$clear2,{
    
    updateTextAreaInput("sentence2",
                        session = getDefaultReactiveDomain(),
                        value = "")
    
  })
  
  
}


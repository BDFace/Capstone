shinyUI(pageWithSidebar(
  headerPanel("Data Science. Capstone Project. August 2015"),
  sidebarPanel(
    
    textInput("input_text", "Text:", "At the end of the"),
    
    submitButton('Submit'),
    br(),br(),
    p('Please allow 30 seconds for the initial prediction. Subsequent predictions should be faster')
  ),
  
  
  mainPanel(
    tabsetPanel(
      tabPanel("App",
               h3('Prediction'),
               br(),
               h4('The following are five possible predicted next words (with the first word on the left the most likely): '),
               br(),
               verbatimTextOutput("pred")
      ),
      
      tabPanel("A little more info",
               h3('Further details'),
               br(),
               h4('The following shows the original input text'),
               verbatimTextOutput("text_UG"),
               br(),
               h4('The following shows the input string length'),
               verbatimTextOutput("len"),
               br(),
               p('This number is then used to tell the algorithm which n-gram to start searching in. I have split the
          original data into n-grams from word pairs up to six word stings. If the input string length is 5 or greater
          my algorithm looks at the last five words, trys to find them in the six n-gram for a match 
          and predicts the sixth word. If it cannot find a match it backs off through the lower n-grams looking for
          a match.'),
               br(),
               h4('The following shows the default list'),
               p('This is used only when it has not been possible to identify a prediction and is based on word count only'),
               p('"the", and", "that", "for", "with"')
      )
      
    )
  )
)
)

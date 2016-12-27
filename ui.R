library(shiny)

# defining the ui for the app
shinyUI(fluidPage(theme = "bootstrap.min.css",
                  
                  
  tags$style(type="text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),

 # div(absolutePanel(top = 0, left = 0, right = 0, fixed = T, bottom = 0, height = 100,
  # application title
  div(class = "heading", tags$h2("TwitterSpi",tags$img(src="eyes.jpeg", 
                       align = 'center', width = 100, height = 100), align = "center")),
  #),
  
  # tag line
  tags$div(tags$h5(tags$em("...why read pages when you can know it just by analysing tweets"), 
                   align = "center")
  ),
  div(textOutput("test")),
  #tags$style(type="text/css", ".heading {padding-top: 70px; padding-bottom: 70px;}")),
  
  withTags(
    div(align = "center",
        tabsetPanel(id = "tabs",
          tabPanel("user", h6("\'spi\' on your friends and foes ;)"),
                   ### search a particular user
                   br(),
                   br(),
                   textInput(inputId = "search", label = "search", placeholder = "user's name/screen-name"),
                   br(),
                   #dateRangeInput(inputId = "dateRange", label = "from:"),
                   actionButton("buttonUser", "RESULTS"),
                   
                   ## output
                   conditionalPanel(
                     condition = "input.buttonUser > 0",
                     div(
                       br(),
                       htmlOutput("profile"), ## name and screen name also displayed
                       br(),
                       h6("Profile"),  ## format this...color and font style
                       tableOutput("basic_info"),
                       br(),
                       
                       
                       div(h6("Followers"),
                       tableOutput("follw"),
                       br()),
                       div(h6("Friends"),
                       tableOutput("friends"),
                       br()),
                      
                       h6("The twitter Picture"),
                       imageOutput("wordcloud"),
                       br(),
                       h6("Optimism Level"),
                       textOutput("optimism"),
                       br()
                       #plotOutput("timeChange")
                     )
                   )
          ),
          tabPanel("reviews", h6("get reviews of the latest gadgets, movies, tourism places and almost anything..."),
                   ### get reviews of the latest gadgets, movies, tourism places and almost anything
                   br(),
                   br(),
                   textInput(inputId = "review", label = "review", placeholder = "item to be reviewed"),
                   # checkbox for emotion type: positive or negative
                   br(),
                   actionButton("buttonReviews", "RESULTS"),
                   conditionalPanel(
                     condition = "input.buttonReviews > 0",
                     div(
                       imageOutput("cloud1")
                     )
                   ),
                   conditionalPanel(
                     condition = "input.buttonReviews > 0",
                     div(
                       imageOutput("cloud2")
                     )
                   )
          ),
          tabPanel("hashtags", h6("whats up with this hashtag and the world..."),
                   ### whats up with this hashtag and the world
                   br(), 
                   br(),
                   textInput(inputId = "hashtags", label = "hashtag", placeholder = "hashtags eg. #CBSE"),
                   br(),
                   actionButton("buttonHash", "RESULTS"),
                   conditionalPanel(
                     condition = "input.buttonHash > 0",
                     div(
                       imageOutput("cloud3")
                     )
                   )
          ),
          tabPanel("place", h6("why is this place so much in news recently..."),
                   ### why is this place so much in news recently
                   br(), br(),
                   textInput(inputId = "place", label = "place", placeholder = "place-name"),
                   br(),
                   actionButton("buttonPlace", "RESULTS"),
                   conditionalPanel(
                     condition = "input.buttonPlace > 0",
                     div(
                       imageOutput("cloud4")
                     )
                   )
          )
      )
    )
  )
 )
)
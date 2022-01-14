#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Flip a Coin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    actionButton("flip", "Flip Once"),
    
    # Show a plot of the generated distribution
    mainPanel(
      uiOutput("img"),
      verbatimTextOutput("out")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- reactiveValues(outcome = NULL, count = 0)
  observeEvent(input$flip, {
    v$outcome <- c(
      v$outcome,
      sample(c("head", "tail"), size = 1, prob = c(0.7, 0.3))
    )
    v$count <- v$count + 1
  })
  output$img <- renderUI({
    img_list <- c(head = "https://pixabay.com/get/g66a154a0de64ead1327520e78dbf673ed2c56e89369b6ad5237df8bb4ec6d496ad15b21315da4e7a9539098a0e0013d5dada377b575ff9c39ef6940cc8c90a61799451a1fa050467e8c0237898eaf78c_640.png",
                  tail = "https://pixabay.com/get/g597292b33e323e222231fb413947f97bf29a00580b35f92d947fed961d774f765287a634e6299615e881788dacbc4a7dc34a90ea05e5e94cc7e8883bcd8354480073aaa5a7bd35d63c7636a8721fe760_640.jpg")
    tags$img(src = img_list[v$outcome[v$count]], width = "100")
  })
  output$out <- renderText({
    if (v$count == 0) count_seq <- NULL
    else count_seq <- seq.int(v$count)
    print(v$outcome)
    paste("The outcome of trial", count_seq, "is", v$outcome, collapse = "\n")
  }, sep = " ")
}

# Run the application 
shinyApp(ui = ui, server = server)

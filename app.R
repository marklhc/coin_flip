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
      sample(c("head", "tail"), size = 1, prob = c(0.65, 0.35))
    )
    v$count <- v$count + 1
  })
  output$img <- renderUI({
    img_list <- c(head = "https://www.pngall.com/wp-content/uploads/1/Bitcoin-PNG-Pic.png",
                  tail = "https://www.pngall.com/wp-content/uploads/1/Bitcoin-PNG.png")
    tags$img(src = img_list[v$outcome[v$count]], width = "100")
  })
  output$out <- renderText({
    if (v$count == 0) count_seq <- NULL
    else count_seq <- seq.int(v$count)
    paste("The outcome of trial", count_seq, "is", v$outcome, collapse = "\n")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

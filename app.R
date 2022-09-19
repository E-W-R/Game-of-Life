library(shiny)

# Define UI ----
dim <- 590
ui <- fluidPage(
  titlePanel("Conway's Game of Life"),
  
  hr(),
  
  sidebarLayout(position = "right",
                
                sidebarPanel(
                  HTML("<p>The Game of Life, also known simply as Life, is a cellular automaton devised by the British mathematician John Horton Conway in 1970. It is a zero-player game, meaning that its evolution is determined by its initial state, requiring no further input. One interacts with the Game of Life by creating an initial configuration and observing how it evolves. <a href='https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life'>Wikipedia</a></p>"),
                  tags$hr(style="border-color: black;"),
                  sliderInput(inputId = "size",
                              label = "Grid size:",
                              min = 10,
                              max = 30,
                              value = 18),
                  sliderInput(inputId = "fill",
                              label = "Percentage filled:",
                              min = 0,
                              max = 100,
                              value = 30),
                  sliderInput(inputId = "time",
                              label = "Generation interval:",
                              min = 0,
                              max = 1,
                              value = 0.1),
                  actionButton("reset", "Generate"),
                  actionButton("run", "Go!", style="color: #fff; background-color: #428BCA")
                  ),
                
                mainPanel(
                  htmlOutput("canvas"),
                  tags$canvas(id = "myCanvas", width = dim, height = dim),
                  tags$script(src = "myscript.js")
                  )
                )
  )

# Define server logic ----
server <- function(input, output, session) {
  
  fill <- function(size, p) {
    arr <- array(data = 0, dim = c(size+(2*bound),size+(2*bound)))
    for (i in 2:(size-1)) {
      for (j in 2:(size-1)) {
        if (sample(1:100, 1) <= p) {
          arr[i+bound,j+bound] <- 1
        }
      }
    }
    return(arr)
  }
  
  neighbours <- function(arr, i, j) {
    total <- 0
    for (m in -1:1) {
      for (n in -1:1) {
        total <- total + arr[m+i, n+j]
      }
    }
    
    return(total-arr[i,j])
  }
  
  updateCell <- function(a1, a2, i, j) {
    nb <- neighbours(a1, i, j)
    
    if (nb == 3 || (a1[i,j] == 1 && nb == 2)) {
      a2[i,j] <- 1
      session$sendCustomMessage("update", ((i-bound) * 100 + (j-bound)))
    }
    else {
      a2[i,j] <- 0
    }
    
    return(a2)
  }
  
  gameArr <- reactiveValues(data = NULL)
  halt <- reactiveValues(state = 0)
  bound <- 5
  
  observeEvent(input$reset, {
    size <- vals()[1]
    p <- vals()[2]
    gameArr$data <- fill(size, p)
    halt$state <- 1
  })
  
  observeEvent(input$run, {
    halt$state <- 2
  })
  
  vals <- eventReactive(input$reset, {
    c(input$size,input$fill,input$time)
  })
  
  output$canvas <- renderUI({
    if (halt$state == 1) {
      size <- vals()[1]
      session$sendCustomMessage("size", size)
      arr1 <- gameArr$data
      for (i in (bound+1):(size+bound)) {
        for (j in (bound+1):(size+bound)) {
          if (arr1[i,j] == 1) {
            session$sendCustomMessage("update", ((i-bound) * 100 + (j-bound)))
          }
        }
      }
      session$sendCustomMessage("updateGame", 0)
    }
    else if (halt$state == 2) {
      size <- vals()[1]
      t <- vals()[3]
      arr1 <- gameArr$data
      arr2 <- array(data = 0, dim = c(size+(bound*2),size+(bound*2)))
      heat <- array(data = 0, dim = c(size+(bound*2),size+(bound*2)))
      
      key <- TRUE
      olda1 <- array(data = 1, dim = c(size+(bound*2),size+(bound*2)))
      olda2 <- array(data = 1, dim = c(size+(bound*2),size+(bound*2)))
      
      while (key) {
        
        for (i in 2:(size+(2*bound)-1)) {
          for (j in 2:(size+(2*bound)-1)) {
            arr2 <- updateCell(arr1, arr2, i, j)
          }
        }
        if (all(arr2 == olda2)) {
          key <- FALSE
        }
        olda2 <- arr2
        heat <- heat + arr2
        session$sendCustomMessage("updateGame", 0)
        Sys.sleep(t)
        
        for (i in 2:(size+(2*bound)-1)) {
          for (j in 2:(size+(2*bound)-1)) {
            arr1 <- updateCell(arr2, arr1, i, j)
          }
        }
        if (all(arr1 == olda1)) {
          key <- FALSE
        }
        olda1 <- arr1
        heat <- heat + arr1
        session$sendCustomMessage("updateGame", 0)
        Sys.sleep(t)
      }
      
      
      heat <- heat/(max(heat)+1)
      h <- c()
      for (i in (bound+1):(size+bound)) {
        for (j in (bound+1):(size+bound)) {
          heat[i,j] <- min(0.4, heat[i,j])
          h <- append(h, (i-bound)*100+(j-bound)+floor(heat[i,j]*1500)*10000)
        }
      }
      h <- sort(h, decreasing = TRUE)
      for (n in h) {
        session$sendCustomMessage("heat", n)
      }
    }
    else {
      return()
    }
  })
  
}

# Run the app ----
shinyApp(ui, server)

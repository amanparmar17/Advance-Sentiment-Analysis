ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(width=2,
      helpText("Crawl tweets and display the summary of the crawled tweets"),
      
      selectInput("var", 
                    label = "Target keyword: ",
                  choices = c("airline", 
                              "demonitisation",
                              "iphone", 
                              "android"),
      
                  selected = "airline"),
      sliderInput("range", 
                  "Number of tweets: ",1,
                  6000,3000)
    ),
    
    mainPanel(
      htmlOutput("selected_var"),
      htmlOutput("min_max"),
      htmlOutput("heading"),
      htmlOutput("contents"),
      htmlOutput("heading2"),
      htmlOutput("contents2"),
      htmlOutput("heading3"),
      htmlOutput("contents4"),
      htmlOutput("heading5"),
      htmlOutput("contents5"),
      imageOutput("preImage"),
      htmlOutput("heading6"),
      htmlOutput("contents6")
    )
  )
) 


server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("Target keyword: <b>", input$var,"</b>")
  })
  
  output$min_max <- renderText({ 
    paste("Number of Crawled Tweets: <b>",
          input$range[1],"tweets</b> <br><br>")
  })
  
  output$heading <- renderText({ 
    paste("<h2>Crawled tweets(brief) </h2>")
  })
  
  output$contents <- renderTable({
    secc
  })
  
  output$heading2 <- renderText({ 
    paste("<h2>Crawled Tweets: </h2><br> <h4> <b>Summary</b></h4>")
  })
  
  output$contents2 <- renderText({
    paste("Number of records fetched:  <b>", nrow(final),"</b><br>")
  })
  
  output$heading3 <- renderText({ 
    paste("<h3>Most retweeted tweet</h3>")
  })
  
  output$contents3 <- renderText({
    paste("<b>Tweet: </b>", tweet,"<br>",
          "<b>Creation date:</b>",create_date,"<br>",
          "<b>Creation time:</b>",create_time,"<br>",
          "<b>Retweet count:</b>",retweet_count,'<br>',
          "<b>Sentiment Value:</b>",sentival,"<br>",
          "<b>Sentiment: </b>",sentiment,'<br>')
  })
  
  output$contents4 <- renderTable({
    d
  })
  output$heading5<-renderText({
    paste("<h4><b>Name of the columns of the Dataset<b></h4>")
  })
  output$contents5 <- renderTable({
    e<-colnames(file)
  })
  
  #geoplot
  output$preImage <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
        list(src = "twitter_crawl.png",contentType="image/*",
         alt = "image")
    
  }, deleteFile = FALSE)

  
    #summary of the plotted points
  output$heading6 <- renderText({ 
    paste("<br><br><h3>Summary of plotted tweets</h3>")
  })
  
  output$contents6 <- renderTable({
    data
  })
  

}

shinyApp(ui=ui, server=server)
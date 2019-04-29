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
      # htmlOutput("contents"),
      htmlOutput("heading2"),
      htmlOutput("contents2"),
      htmlOutput("contents23"),
      imageOutput("preImage1"),
      htmlOutput("contents24"),
      htmlOutput("contents25"),
      imageOutput("preImage2"),
      htmlOutput("heading3"),
      htmlOutput("contents4"),
      htmlOutput("heading4"),
      imageOutput("preImage3"),
      htmlOutput("heading6")
      # htmlOutput("contents6")
    )
  )
) 



server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("Target keyword: <b>", input$var,"</b>")
  })
  
  output$min_max <- renderText({ 
    paste("Number of Crawled Tweets: <b>",
          nrow(dataset),"</b>")
  })
  
  output$heading <- renderText({ 
    paste("<h2>WordCloud </h2>")
  })
  
  output$heading2 <- renderText({ 
    paste("<h4>R packages used: </h4><br> <ul> <li><b>Wordcloud</b></li>
          <li><b>Tm</b></li>
          <li><b>SnowBallC</b></li>
          <li><b>RcolorBrewer</b></li></ul>")
  })
  
  output$contents2 <- renderText({
    paste("<h3>On creating a wordcloud of the preprocessed tweets:  <h3>")
  })
  
  output$contents23 <- renderText({
    paste("Summary of the wordcloud formed: <ul> 
          <li>number of target rows: <b>14640</b></li>
          <li>minimum frequency of words: <b>1</b></li>
          <li>maximum numberof words: <b>300</b></li>
          <li>color theme: <b>brewer.pal(8, 'Dark2')</b></li>")
  })
  
  
  output$preImage1 <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
    list(src = "wordcloud1.png",contentType="image/*",
         alt = "image")
    
  }, deleteFile = FALSE)
  
  output$contents24 <- renderText({
    paste("<br><br><br><br><br><br><br><h3>On creating second wordcloud of the preprocessed tweets:  <h3>")
  })
  
  output$contents25 <- renderText({
    paste("Summary of the wordcloud formed: <ul> 
          <li>number of target rows: <b>14640</b></li>
          <li>maximum numberof words: <b>200</b></li>
          <li>color theme: <b>Random</b></li></ul><br><br><br>")
  })
  
  
  output$preImage2 <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
    list(src = "wordcloud2.png",contentType="image/*",
         alt = "image")
    
  }, deleteFile = FALSE)
  
  
  output$heading3 <- renderText({ 
    paste("<br><br><h4>Frequency table of the wordcloud</h4>")
  })
  output$contents4 <- renderTable({
    head(d, 15)
  })
  
  output$heading4 <- renderText({ 
    paste("<h4>Frequency bargraph</h4>")
  })
  
  output$preImage3 <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
    list(src = "wordcloud4.png",contentType="image/*",
         alt = "image")
    
  }, deleteFile = FALSE)
  


  
}

shinyApp(ui=ui, server=server)
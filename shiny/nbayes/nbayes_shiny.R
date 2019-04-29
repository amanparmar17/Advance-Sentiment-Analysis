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
      htmlOutput("heading3"),
      htmlOutput("heading4"),
      htmlOutput("heading5"),
      htmlOutput("contents2"),
      htmlOutput("contents23"),
      # imageOutput("preImage1"),
      
      # imageOutput("preImage2"),
      # htmlOutput("heading3"),
      htmlOutput("contents4"),
      
      htmlOutput("contents5"),
      htmlOutput("contents25"),
      htmlOutput("contents24")
      # imageOutput("preImage3"),
      # htmlOutput("heading6")
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
    paste("<h2>Naive Bayes </h2>")
  })
  
  output$heading2 <- renderText({ 
    paste("<h4>R packages used: </h4><br> <ul> <li><b>RTextTools</b></li>
          <li><b>e1071</b></li>
          <li><b>dplyr</b></li>
          <li><b>TidyText</b></li>
          <li><b>Class</b></li>
          <li><b>Caret</b></li></ul>")
  })
  
  

  
  
  output$heading3 <- renderText({ 
    paste("<h4>Preprocessing steps involved: </h4><br> <ul> <li><b>Removing Special characters</b></li>
          <li><b>Removing Punctuations</b></li>
          <li><b>Removing Endline characters</b></li>
          <li><b>Converting the tweets to lower caset</b></li>
          <li><b>Substitutions of various special words</b></li>
          </ul>")
  })
  
  output$heading4 <- renderText({ 
    paste("<h4>Creating training and test data: </h4><br> <ul> <li><b>Training Dataset: ",smp_size,"</b></li>
          <li><b>Test Dataset: ",nrow(application)-smp_size,"</b></li>
          </ul>")
  })
  
  
  output$heading5 <- renderText({ 
    paste("<h4>Creating sparse matrix for datasets: </h4><br> <ul> <li><b>Training Dataset:
            <ul><li>size:",smp_size,"</li><li>Non-/sparse entries: 48156/38583822</li><li>Maximal term length: 39</li><li>Sparsity:100%</li></ul></b></li>
          <li><b>Test Dataset:
            <ul><li>size:",nrow(application)-smp_size,"</li><li>Non-/sparse entries: 1142/2284</li><li>Maximal term length: 8</li><li>Sparsity:67%</li></ul></b></li>
          </ul>")
  })
  
  output$contents2 <- renderText({
    paste("<h3>On running Naive Bayes algorithm on preprocessed tweets:  <h3>")
  })
  
  output$contents23 <- renderText({
    paste("Summary of the model formed: <ul> 
          <li>Levels: <b><ul><li>negative</li><li>neutral</li><li>positive</li></b></li></ul>
          <li>Default function Call: <b>naiveBayes.default(x = matrix_train, y = (train$Response))
</b></li></ul>")
  })
  
  output$contents5 <- renderTable({
    classifier$apriori
  })

  
  
  
  
  output$contents4 <- renderTable({
    predicted_table
  })

  output$contents25 <- renderText({
    paste("<h3>On evaluating the model on 700 rows: </h3>")
  })
  
  output$contents24 <- renderText({
    paste("Accuracy:  <b>",predicted_accuracy,"</b><br><br>")
  })
  
  }

shinyApp(ui=ui, server=server)
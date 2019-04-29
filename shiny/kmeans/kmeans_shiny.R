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
      # htmlOutput("min_max"),
      htmlOutput("heading"),
      htmlOutput("contents"),
      # htmlOutput("heading2"),
      htmlOutput("contents2"),
      htmlOutput("heading3"),
      htmlOutput("contents4"),
      imageOutput("preImage"),
      htmlOutput("heading5"),
      htmlOutput("contents5"),
      
      
      htmlOutput("heading6"),
      htmlOutput("contents6"),
      htmlOutput("contents8"),
      imageOutput("preImage2"),
      
      htmlOutput("contents88")
    )
  )
) 


server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("<ul><li>Target keyword: <b>", input$var,"</b></li></ul>")
  })

  output$heading <- renderText({ 
    paste("<ul><li><h3>By Elbow Method </h3></li><li><h4>Optimal number of clusters: 26</li></ul>")
  })

  # output$heading2 <- renderText({ 
  #   paste("<ul></h4>")
  # })
  
  #elbow image
  
  output$contents2 <- renderText({
    paste("<ul><li><b><h2>Running KMeans clustering with optimal number of clusters: </h2></b></li></ul>")
  })
  
  
  #cluster image
  
  output$heading3 <- renderText({
    paste("<ul><li><h4>Cluster Info</h4></li></ul>")
  })
  # 
  
  output$contents4 <- renderTable({
    final_df
  })
  
  output$preImage <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
    list(src = "km1.png",contentType="image/*",
         alt = "image",width=1100,height=450)

  }, deleteFile = FALSE)

  
  output$heading5<-renderText({
    paste("<br><br><br><br><ul><li><h4>Within cluster sum of squares by cluster: <b>",wss,"</b></h4></li></ul>")
  })
  
  output$heading6 <- renderText({
    paste("<ul><li><h4>Total elements clustered: <b>",nrow(KMeans_3),"</b></h4></li></ul>")
  })
  
  output$contents5 <- renderText({
    paste("<ul><li><h4>Cluster bifergation into different sentiments</h4></li></ul>")
  })
  
  output$contents6 <- renderTable({
    info
  })

  output$contents8 <- renderText({
    paste("<ul><li><h4>Geological plotting of clusters</h4></li></ul><br>")
  })
  
  
  
  output$preImage2 <- renderImage({
    # filename <- normalizePath(file.path('./',
    #                                     paste('image', input$n, '.png', sep='')))
    list(src = "km2.png",contentType="image/*",
         alt = "image",width=800,height=550)
    
  }, deleteFile = FALSE)
  
  output$contents88 <- renderText({
    paste("<br><br>.")
  })
  
  
  # output$contents5 <- renderTable({
  #   e<-colnames(file)
  # })
  # 
  # #geoplot
  # output$preImage <- renderImage({
  #   # filename <- normalizePath(file.path('./',
  #   #                                     paste('image', input$n, '.png', sep='')))
  #   list(src = "twitter_crawl.png",contentType="image/*",
  #        alt = "image")
  #   
  # }, deleteFile = FALSE)
  # 
  # 
  # #summary of the plotted points
  # output$heading6 <- renderText({ 
  #   paste("<br><br><h3>Summary of plotted tweets</h3>")
  # })
  # 
  # output$contents6 <- renderTable({
  #   data
  # })
  
  
}

shinyApp(ui=ui, server=server)
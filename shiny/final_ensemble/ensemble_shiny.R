ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(width=12,
                 helpText("Crawl tweets and display the summary of the crawled tweets"),
                 
                 selectInput("var", 
                             label = "Target keyword: ",
                             choices = c("airline", 
                                         "demonitisation",
                                         "iphone", 
                                         "android"),
                             
                             selected = "airline"),
                 sliderInput("range", 
                             "Range of tweets: ",min=1,
                             max=649,5,value = c(1,649)),
                 
                 helpText("Please select the required algorithms:"),
                 
                 helpText("Classification:"),
                 checkboxInput("checkbox1", label = "Decision Tree", value = FALSE),
                 checkboxInput("checkbox2", label = "Random Forest", value = FALSE),
                 checkboxInput("checkbox3", label = "Naive Bayes", value = FALSE)
                 # helpText("Clustering:"),
                 # checkboxInput("checkbox4", label = "KMeans", value = TRUE),
                 # actionButton("do", "Predict")
    ),
    
    mainPanel(
      htmlOutput("selected_var"),
      htmlOutput("min_max"),
      htmlOutput("heading"),
      # htmlOutput("contents"),
      htmlOutput("heading2"),
       textOutput("heading3"),
      htmlOutput("heading4"),
      # htmlOutput("heading5")
      htmlOutput("contents2")
      # htmlOutput("final")
      # imageOutput("preImage1"),
      
      # imageOutput("preImage2"),
      # htmlOutput("heading3"),
      # htmlOutput("contents4"),
      # 
      # htmlOutput("contents5"),
      # htmlOutput("contents25"),
      # htmlOutput("contents24")
      # imageOutput("preImage3"),
      # htmlOutput("heading6")
      # htmlOutput("contents6")
    )
  )
) 



server <- function(input, output) {
#   
  output$selected_var <- renderText({
    paste("Target keyword: <b>", input$var,"</b>")
  })
#   
  output$min_max <- renderText({ 
    paste("Number of Crawled Tweets: <b>",
          input$range[1],"to",input$range[2],"tweets</b> <br><br>")
  })
  
    output$heading <- renderText({
      paste("<h2>Naive Bayes </h2>")
    })
    
      output$heading4 <- renderText({
        paste("<h2>start index </h2>",input$range[1])
      })
    
      output$heading2 <- renderText({
        paste("<h2>end index </h2>",input$range[2])
      })

        output$final=renderTable({

        if(input$checkbox1==FALSE && input$checkbox2==FALSE && input$checkbox3==FALSE)
        {
          return(NULL)
        }
        
        start_index=as.integer(input$range[1])
        end_index=as.integer(input$range[2])
        file=read.csv("ensemblee_final.csv",header = TRUE)
        
        target_data = file[input$range[1]:input$range[2],]
        row=nrow(target_data)
        
        if(input$checkbox3==TRUE)
        {
          #nbayes
          file2=read.csv("preprocessed_dataset.csv",header=TRUE)
          # target_dataa=file2[input$range[1]:input$range[2],]
          target_dataa=file2[start_index:end_index,]
          data=target_dataa[,c('Response','tweets')]
          
          
          mat_test= create_matrix(data$Response, language="english", 
                                  removeStopwords=TRUE, removeNumbers=FALSE, 
                                  stemWords=FALSE) 
          matrix_test = as.matrix(mat_test)
          
          
          predicted = predict(classifier_nbayes,matrix_test)
          if(predicted[1]=='positive')
            senti_nb='positve'
          if(predicted[1]=='neutral')
            senti_nb='neutral'
          if(predicted[1]=='negative')
            senti_nb='negative'
        }
        


       

# 
#         decision tree
#         airlines=target_data$airline
#         timezones=target_data$timezone
#         d=as.data.frame(cbind(airlines,timezones))
#         colnames(d)=c('airline','timezone')
#         predicted_dt=predict(classifier_dec,d)
#         predicted_dt=as.data.frame(predicted_dt)
# 
#         neg_count=neu_count=pos_count=0
# 
#         for(i in 0:nrow(predicted_dt))
#         {
#           if(which.max(predicted_dt[i,])==1)
#             neg_count=neg_count+1
#           if(which.max(predicted_dt[i,])==2)
#             neu_count=neu_count+1
#           if(which.max(predicted_dt[i,])==3)
#             pos_count=pos_count+1
#         }
# 
#         fin=as.data.frame(cbind(neg_count,neu_count,pos_count))
#         j=which.max(fin[1,])
#         if(j==1)
#           senti_dc="negative"
#         if(j==2)
#           senti_dc="neutral"
#         if(j==3)
#           senti_dc="positive"

        # sem=as.data.frame(senti_nb)
        # sem

        #random_forest


        })
      
      # observeEvent(input$do, {
      #   start_index=input$range[1]
      #   end_index=input$range[2]
      #   target_data = file[input$range[1]:input$range[2],]
      #   session$sendCustomMessage(type = 'testmessage',
      #                             message = nrow(target_data))
      # })
      # showData<-reactive({
      #   target_data
      # })
    
        output$contents2 <- renderText({
          start_index=as.integer(input$range[1])
          end_index=as.integer(input$range[2])
          file2=read.csv("preprocessed_dataset.csv",header=TRUE)
          # target_dataa=file2[50:150,]
          target_dataa=file2[start_index:end_index,]
          nrow(target_dataa)
          
          
          data=target_dataa[,c('Response','tweets')]
          
          
          mat_test= create_matrix(data$Response, language="english", 
                                  removeStopwords=TRUE, removeNumbers=FALSE, 
                                  stemWords=FALSE) 
          matrix_test = as.matrix(mat_test)
          
          
          predicted = predict(classifier_nbayes,matrix_test[1:50,])
          if(predicted[1]=='positive')
            senti_nb='positve'
          if(predicted[1]=='neutral')
            senti_nb='neutral'
          if(predicted[1]=='negative')
            senti_nb='negative'
          
          senti_nb
        })
    
    
    
    
    
    
    
    
    
    
    
    
    
#   output$heading <- renderText({ 
#     paste("<h2>Naive Bayes </h2>")
#   })
#   
#   output$heading2 <- renderText({ 
#     paste("<h4>R packages used: </h4><br> <ul> <li><b>RTextTools</b></li>
#           <li><b>e1071</b></li>
#           <li><b>dplyr</b></li>
#           <li><b>TidyText</b></li>
#           <li><b>Class</b></li>
#           <li><b>Caret</b></li></ul>")
#   })
#   
#   
#   
#   
#   
#   output$heading3 <- renderText({ 
#     paste("<h4>Preprocessing steps involved: </h4><br> <ul> <li><b>Removing Special characters</b></li>
#           <li><b>Removing Punctuations</b></li>
#           <li><b>Removing Endline characters</b></li>
#           <li><b>Converting the tweets to lower caset</b></li>
#           <li><b>Substitutions of various special words</b></li>
#           </ul>")
#   })
#   
#   output$heading4 <- renderText({ 
#     paste("<h4>Creating training and test data: </h4><br> <ul> <li><b>Training Dataset: ",smp_size,"</b></li>
#           <li><b>Test Dataset: ",nrow(application)-smp_size,"</b></li>
#           </ul>")
#   })
#   
#   
#   output$heading5 <- renderText({ 
#     paste("<h4>Creating sparse matrix for datasets: </h4><br> <ul> <li><b>Training Dataset:
#           <ul><li>size:",smp_size,"</li><li>Non-/sparse entries: 48156/38583822</li><li>Maximal term length: 39</li><li>Sparsity:100%</li></ul></b></li>
#           <li><b>Test Dataset:
#           <ul><li>size:",nrow(application)-smp_size,"</li><li>Non-/sparse entries: 1142/2284</li><li>Maximal term length: 8</li><li>Sparsity:67%</li></ul></b></li>
#           </ul>")
#   })
#   
#   output$contents2 <- renderText({
#     paste("<h3>On running Naive Bayes algorithm on preprocessed tweets:  <h3>")
#   })
#   
#   output$contents23 <- renderText({
#     paste("Summary of the model formed: <ul> 
#           <li>Levels: <b><ul><li>negative</li><li>neutral</li><li>positive</li></b></li></ul>
#           <li>Default function Call: <b>naiveBayes.default(x = matrix_train, y = (train$Response))
# </b></li></ul>")
#   })
#   
#   output$contents5 <- renderTable({
#     classifier$apriori
#   })
#   
  
  # 
  # 
  # 
  # output$contents4 <- renderTable({
  #   predicted_table
  # })
  # 
  # output$contents25 <- renderText({
  #   paste("<h3>On evaluating the model on 700 rows: </h3>")
  # })
  # 
  # output$contents24 <- renderText({
  #   paste("Accuracy:  <b>",predicted_accuracy,"</b><br><br>")
  # })
  
  }

shinyApp(ui=ui, server=server)
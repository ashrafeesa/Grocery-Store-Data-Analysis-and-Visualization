library(shiny)
library(arules)
library(dplyr)

ui <- fluidPage(
  titlePanel("Grocery Dataset"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      numericInput("nClusters", "Number of Clusters", min = 2, max = 4, value = 2),
      actionButton("update", "update k-mean"),
      numericInput("min_support", "Minimum Support", 0.02, min = 0.001, max = 1, step = 0.01),
      numericInput("min_confidence", "Minimum Confidence", 0.5, min = 0.001, max = 1, step = 0.01)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Pie Chart - Cash/Credit", plotOutput("piePlot")),
        tabPanel("Histogram - Age", plotOutput("ageHistogram")),
        tabPanel("Bar Plot - City", plotOutput("cityBarPlot")),
        tabPanel("Box Plot - Distribution", plotOutput("boxPlot")),
        tabPanel("Apriori Table", tableOutput("aprioriTable")),
        tabPanel("K-Means Analysis", plotOutput("kmeansPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    tdata <- read.csv(input$file$datapath, sep = ",")
    MainData <- distinct(tdata)
    list_of_items=strsplit(as.character(MainData[,1]),split = ",")
    transactions=as(list_of_items,"transactions")
    apriori_rules <- apriori(transactions, parameter = list(supp = input$min_support, conf = input$min_confidence, minlen = 2))
    kmeans_data <- MainData[, c("age", "total","rnd","customer")]
    return(list(data = MainData, rules = apriori_rules, kmeans_data = kmeans_data))
  }) 
  
  output$piePlot <- renderPlot({
    big_data <- data()$data
    cleaned_data <- distinct(big_data)
    x <- table(cleaned_data$paymentType)
    total <- sum(x)
    percentages <- round(100 * x / total, 2)
    labels <- paste(names(x), "(", percentages, "%)", sep = "")
    
    pie(x, main = "Type of Payment", col = c("green", "black"))
    legend("bottomright", legend = labels, fill = c("green", "black"))
  })
  
  output$ageHistogram <- renderPlot({
    big_data <- data()$data
    cleaned_data <- distinct(big_data)
    cleaned_data <- cleaned_data[cleaned_data$age > 0, ]
    
    hist(cleaned_data$age, col = "orange", border = "darkblue", main = "Histogram of Age",
         xlab = "Age",
         ylab = "Total Spending",
         xlim = c(20, 60),
         ylim = c(0,2500))
  })
  
  output$cityBarPlot <- renderPlot({
    big_data <- data()$data
    cleaned_data <- distinct(big_data)
    
    df_city_spend <- cleaned_data %>%
      group_by(city) %>%
      summarize(total_spend = sum(total, na.rm = TRUE)) %>%
      arrange(desc(total_spend))
    
    barplot(df_city_spend$total_spend, col = c("#6C737E","#7393A7","#B5CFD8","#6C737E","#7393A7","#B5CFD8","#6C737E","#7393A7","#B5CFD8","#6C737E"), border = "#353E55", main = "Sum of Total Spending by City",
            xlab = "City",
            ylab = "Total Spending",
            ylim = c(0, max(df_city_spend$total_spend)),
            names.arg = df_city_spend$city)
  })
  
  output$boxPlot <- renderPlot({
    big_data <- data()$data
    cleaned_data <- distinct(big_data)
    
    boxplot(cleaned_data$total, main = "Distribution of total spending", xlab = "Total spend")
  })
  
  output$aprioriTable <- renderTable({
    data()  
    
    apriori_results <- data()$rules
    
    rules_info <- as(apriori_results, "data.frame")
    
    rules_info
  })
  
  
  
  observeEvent(input$update, {
    data() 
    s1 <- data()$kmeans_data %>%
      group_by(rnd,customer,age) %>%
      summarize(total_spend = sum(total))
    s2<-s1[,c("age" , "total_spend")]
    kmean_cluster <- kmeans(s2, centers = input$nClusters)
    
    
    output$kmeansPlot <- renderPlot({
      
      plot(s2, col = kmean_cluster$cluster, pch = 19,
           main = "K-Means Analysis", xlab = "Age", ylab = "Total")
      points(kmean_cluster$centers, col = 1:input$nClusters, pch = 8, cex = 2)
    })
  })
}

shinyApp(ui, server)

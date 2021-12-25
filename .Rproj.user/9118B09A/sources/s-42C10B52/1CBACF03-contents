library(shiny)

# Shiny Server
server <- shinyServer(function(input,output){
  output$rawdata <- renderTable(head(commerce,100))
  output$wordcloud <- renderPlot({
    wordcloud(corpus.clean, min.freq = 3000, max.word = 100, random.order=F, colors = brewer.pal(8, "Dark2"))
  })
  output$kmeans <- renderPlot({
    k1 <- kmeans(comm_norm, input$sizeKmeans, nstart = 10)
    paste('Summary for this k-means model with 4 centroids')
    paste('The size of each cluster is: '); k1$size
    paste('Between-cluster sum of squares: ', k1$betweenss)
    paste('Total within-cluster sum of squares: ', k1$tot.withinss)
    
    fviz_cluster(k1, data = comm_rfm[,1:3]) + 
      theme_minimal() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 16, face = 'italic'),
            legend.text = element_text(size = 13),
            plot.title = element_text(size = 20))
  })
  output$kmeansproduct <- renderPlot({
    pca.fit <- prcomp(desc_def)
    wss <- function(k) {
      kmeans(pca.fit$x[,c(1:3)], k, nstart = 10)$tot.withinss
    }
    
    # Compute and plot wss for k = 1 to k = 15
    k.values <- 1:15
    
    k2 <- stats::kmeans(pca.fit$x[,c(1:3)], input$sizeKmeans, nstart = 25)
    
    # Unit Price by cluster 
    
    fviz_cluster(k2, data = pca.fit$x[,1:2]) + 
      theme_minimal() + 
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 16, face = 'italic'),
            legend.text = element_text(size = 13),
            plot.title = element_text(size = 20))
  })
  output$countrygroup <- renderPlot({
    world_map = map_data("world")
    to_world <- comm_clean %>% 
      group_by(Country) %>% 
      summarise(InvoiceAmount = log(sum(InvoiceAmount, na.rm = T))) %>% 
      mutate(Country = fct_recode(Country, "UK" = "United Kingdom", 'Ireland' = 'EIRE')) %>% 
      right_join(world_map, by = c("Country" = "region"))
    
    options(repr.plot.width = 20, repr.plot.height = 11) #resize for worldmap
    to_world %>%
      ggplot(aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill=InvoiceAmount), colour = "grey50") +
      theme_void() +
      scale_fill_viridis(option = 'C', na.value = 'grey90')
  })
})



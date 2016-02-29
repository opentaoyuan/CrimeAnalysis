shinyServer(function(input, output) {
  output$Data <- renderTable({
    if(input$data_breau != "All" & input$data_type != "All"){
      data %>% filter(year == input$data_year,month == input$data_month,breau == input$data_breau,type == input$data_type)
    }else if(input$data_breau !="All" & input$data_type == "All"){
      data %>% filter(year == input$data_year,month == input$data_month,breau == input$data_breau)
    }else if(input$data_breau =="All" & input$data_type != "All"){
      data %>% filter(year == input$data_year,month == input$data_month,type == input$data_type)
    }else{
      data %>% filter(year == input$data_year,month == input$data_month)
    }
  })

  output$downloadData <- downloadHandler(
    filename = function() { paste('Taoyuan_crime', '.csv') },
    content = function(file) {
      write.csv(data, file)
    })

  zoom <- reactive({
    input$map_zoom
  })
  
  
  output$map <- renderPlot({
    if(input$map_breau == "All" & input$map_type == "All"){
      fdata <- data %>%
        filter(year == input$map_year,month == input$map_month)
    }else if(input$map_breau == "All" & input$map_type != "All"){
      fdata <- data %>%
        filter(year == input$map_year,month == input$map_month,type == input$map_type)
    }else if (input$map_breau != "All" & input$map_type == "All"){
      fdata <- data %>%
        filter(year == input$map_year,month == input$map_month,breau == input$map_breau)
      }else{
      fdata <- data %>%
        filter(year == input$map_year,month == input$map_month,breau == input$map_breau,type == input$map_type)
      }#filter map & choice zoom
    
      getmap <- get_googlemap(center = c(lon = mean(fdata$lon),lat = mean(fdata$lat)),zoom=zoom(),maptype = "roadmap")
      ggmap(getmap,extent = "device",ylab = "lat",xlab = "lon",maprange=FALSE) +
        geom_point(data = fdata,colour = "darkred", pch=16, cex= 2.5,alpha = 1) +
        stat_density2d(data = fdata, aes(x = lon, y = lat,  fill = ..level.., alpha = ..level..),size = 0.01, geom = 'polygon')+
        scale_fill_gradient(low = "green", high = "red") +
        scale_alpha(range = c(0.05, 0.15))  +
        theme(legend.position = "none") 
      })
  
  breau_plot <- reactive({
    data %>% 
      filter(year == input$anb_year,month == input$anb_month) %>% 
      group_by(breau,type) %>%
      summarise(count = n()) %>%
      ggvis(~breau,~count) %>%
      layer_bars(fill = ~type,width = 0.5)
  })
  
  breau_plot %>% bind_shiny("breau_plot")

})
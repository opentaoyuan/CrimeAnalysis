shinyUI(fluidPage(
  titlePanel("Taoyuan Crimes"),
  h3("Data:104/11~105/1", style = "color:red"),
  downloadButton('downloadData', 'Download'),
  br(),
  span("last update:105/2/5", style = "color:blue"),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("MAP",
                           fluidRow(     
                            column(3,
                              selectInput("map_type", label = h4("Type"),c("All","Motorcycle theft","Residential burglary","Car theft")),
                              selectInput("map_breau", label = h4("Breau"),
                                         c("All","Taoyuan","Zhongli","Yangmei","Dayaun","Daxi","Pingzhen","Bade","Guishan","Luzhu","Longtan")),                                   
                              selectInput("map_year", label = h4("Year"),c(104,105)),      
                              sliderInput("map_month", label = h4("Month"),min = 1, max = 12, value = 11),
                              sliderInput("map_zoom", label = h4("Zoom"),min = 11, max = 14, value = 11),
                              span("Design by DSP", style = "color:darkorange"),
                              img(src = "DSP.png", height = 100, width = 100)
                                  ),
                            column(6,
                              h3("Density maps"),
                              plotOutput("map",width = "800px",height = "800px")
                                  )
                           )),#map_panel
                  tabPanel("Analysis_breau",
                           fluidRow(
                             column(3,
                                    selectInput("anb_year", label = h4("Year"),c(104,105)),      
                                    sliderInput("anb_month", label = h4("Month"),min = 1, max = 12, value = 11),
                                    span("Design by DSP", style = "color:darkorange"),
                                    img(src = "DSP.png", height = 100, width = 100)
                             ),
                               column(3,
                                   h3("Bars by breau"),
                                   ggvisOutput("breau_plot")
                                   ) 
                           )
                  ),#Analysis_breau_panel
                    tabPanel("Data",
                           fluidRow(
                             column(3,
                                    selectInput("data_type", label = h4("Type"),c("All","Motorcycle theft","Residential burglary","Car theft")),
                                    selectInput("data_breau", label = h4("Breau"),
                                                c("All","Taoyuan","Zhongli","Yangmei","Dayaun","Daxi","Pingzhen","Bade","Guishan","Luzhu","Longtan")),
                                    selectInput("data_year", label = h4("Year"),c(104,105)),      
                                    sliderInput("data_month", label = h4("Month"),min = 1, max = 12, value = 11),
                                    span("Design by DSP", style = "color:darkorange"),
                                    img(src = "DSP.png", height = 100, width = 100)
                             ),
                             column(6,
                                    h3("Table"),
                                    tableOutput("Data")     
                             )
                           )
                  )#Data_panel
       )
    )#mainPanel
))

shinyUI(fluidPage(
  titlePanel(h3("Confusion matrix for accuracy assessment")),
  
  sidebarLayout(
                sidebarPanel(h4("Disclaimer for FAO"),
                               img(src="FAO_blue_50.jpg", height = 50, width = 50),
                               p("FAO can't be held responsible for anything here"),
                               code(paste("Version 0.1 - ", Sys.Date())) ,
                               htmlOutput("selectUI_for"),
                               htmlOutput("selectUI_nofor"),
                               htmlOutput("selectUI_other")
                ),
                mainPanel(fluidRow(
                            column(5,h4("Non aggregated data")),
                            column(5,h4("Aggegated data"))
                ),
                          fluidRow(
                            column(5,plotOutput("histogram_all")),
                            column(5,plotOutput("histogram_agg"))
                                  ),
                          fluidRow(
                            column(5,tableOutput("accuracy_all")),
                            column(5,tableOutput("accuracy_agg"))
                                  ),
                          tableOutput("matrix_all"),
                          tableOutput("matrix_agg")
                                  )
                          )                               
 ))
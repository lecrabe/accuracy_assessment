
shinyUI(fluidPage(
  titlePanel(h3("Évaluation de la précision de la carte de changement de couvert forestier au Congo, 2000-2012")),
  
  sidebarLayout(sidebarPanel(
                               img(src="FAO_blue_50.jpg", height = 50, width = 50),
                               p("Cette interface présente les résultats de l'évaluation de la précision de la carte de changement de couvert forestier pour la République du Congo, 2000-2012"),
                               p("L'agrégation des classes peut être modifiée ci-dessous"),
                               code(paste("Version 0.1 - ", Sys.Date())) ,
                               htmlOutput("selectUI_for"),
                               htmlOutput("selectUI_nofor"),
                               htmlOutput("selectUI_other"),
                               h5("Disclaimer for FAO"),
                               h6("The designations employed and the presentation of material in this information
                                  product do not imply the expression of any opinion whatsoever on the part of the
                                  Food and Agriculture Organization of the United Nations (FAO) concerning the legal
                                  or development status of any country, territory, city or area or of its authorities, or
                                  concerning the delimitation of its frontiers or boundaries. The mention of specific
                                  companies or products of manufacturers, whether or not these have been patented,
                                  does not imply that these have been endorsed or recommended by FAO in preference
                                  to others of a similar nature that are not mentioned.
                                  The views expressed in this information product are those of the author(s) and do not
                                  necessarily reflect the views or policies of FAO.
                                  © FAO, 2015")
                ),
                mainPanel(fluidRow(
                            column(5,h4("Données non agrégées")),
                            column(5,h4("Données agrégées"))
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
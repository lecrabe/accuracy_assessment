##############################################################################
########################## CALL NECESSARY LIBRARIES ##########################
##############################################################################
options(stringsAsFactors=FALSE)

library(ggplot2)
library(xtable)

##############################################################################
########################## READ THE DATA           ##########################
##############################################################################
df_i     <- read.csv("data/clean_dataset.csv")
areas_i  <- read.csv("data/area_congo.csv")
legend_i <- levels(as.factor(df_i$map_code))
legend_class_i <- areas_i$class[areas_i$code %in% as.numeric(legend_i)]

legend_agg <- c(1,2,3)
names(legend_agg)<-c("Forêt","Non Forêt","Pertes")
areas_i[,3]<-areas_i[,3]*900/10000

shinyServer(
  function(input, output) {
    
    output$selectUI_for <- renderUI({ 
      the_list <- as.list(legend_i)
      names(the_list)<-legend_class_i
      
      selectInput("class_for", 
                  label = h3(paste("Classes à agréger sous Forêt")), 
                  choices = the_list,
                  multiple = TRUE,
                  selected = c("Fp","Fs","Fm")
      )
    })
    
    output$selectUI_nofor <- renderUI({ 
      the_list <- as.list(legend_i)
      names(the_list)<-legend_class_i
      
      selectInput("class_nofor", 
                  label = h3(paste("Classes à agréger sous Non-Forêt")), 
                  choices = the_list,
                  multiple = TRUE,
                  selected = c("Eau","NF")
      )
    })
    
    output$selectUI_other <- renderUI({ 
      the_list <- as.list(legend_i)
      names(the_list)<-legend_class_i
      
      selectInput("class_other", 
                  label = h3(paste("Classes à agréger sous Pertes")), 
                  choices = the_list,
                  multiple = TRUE,
                  selected = c("Pp","Ps","Pm","Pr")
      )
    })
    
################################################    
################ Data aggregated
################################################
df_agg <- reactive({
  df <- df_i
  list_class_for   <- input$class_for
  list_class_nofor <- input$class_nofor
  list_class_other <- input$class_other
  
  df$map_code <- 0
  df$ref_code <- 0
  
  tryCatch(df[df_i$map_code %in% list_class_for,]$map_code <- 1,finally=print("no class in forest"))
  tryCatch(df[df_i$map_code %in% list_class_nofor,]$map_code <- 2,finally=print("no class in no forest"))
  tryCatch(df[df_i$map_code %in% list_class_other,]$map_code <- 3,finally=print("no class in other"))
  
  tryCatch(df[df_i$ref_code %in% list_class_for,]$ref_code <- 1,finally=print("no class in forest"))
  tryCatch(df[df_i$ref_code %in% list_class_nofor,]$ref_code <- 2,finally=print("no class in no forest"))
  tryCatch(df[df_i$ref_code %in% list_class_other,]$ref_code <- 3,finally=print("no class in other"))
  
  df
})
    
################################################    
################ Areas aggregated
################################################
areas_agg <- reactive({
  areas_agg_item <- data.frame(cbind(legend_agg,names(legend_agg)))
  names(areas_agg_item) <- c("code","class")
  areas_agg_item$area   <- 0
  
  list_class_for   <- input$class_for
  list_class_nofor <- input$class_nofor
  list_class_other <- input$class_other

  tryCatch(areas_agg_item[areas_agg_item$code==1,]$area<-sum(areas_i[areas_i$code %in% list_class_for,]$area),finally = print("No class in Forest"))
  tryCatch(areas_agg_item[areas_agg_item$code==2,]$area<-sum(areas_i[areas_i$code %in% list_class_nofor,]$area),finally = print("No class in No Forest"))
  tryCatch(areas_agg_item[areas_agg_item$code==3,]$area<-sum(areas_i[areas_i$code %in% list_class_other,]$area),finally = print("No class in Other"))
  
  areas_agg_item
  })
    
################################################    
################ Matrix for all classes
################################################
matrix_all <- reactive({
  df <- df_i
  areas <- areas_i
  legend <- legend_i
  
  tmp <- as.matrix(table(df$map_code,df$ref_code))
  matrix<-matrix(0,nrow=length(legend),ncol=length(legend))
  
  for(i in 1:length(legend)){
    tryCatch({
      cat(paste(legend[i],"\n"))
      matrix[,i]<-tmp[,legend[i]]
    }, error=function(e){cat("Configuration impossible \n")}
    )
  }
  
  matrix
  })

################################################    
################ Table of accuracies
################################################

accuracy_all <- reactive({
        matrix <- matrix_all()
        df <- df_i
        areas <- areas_i
        legend <- legend_i
        
        matrix_w<-matrix
        for(i in 1:length(legend)){
          for(j in 1:length(legend)){
            matrix_w[i,j]<-matrix[i,j]/sum(matrix[i,])*areas[areas$code==legend[i],]$area/sum(areas$area)
          }}
      
        matrix_se<-matrix
        for(i in 1:length(legend)){
          for(j in 1:length(legend)){
            matrix_se[i,j]<-areas[areas$code==legend[i],]$area/sum(areas$area)*areas[areas$code==legend[i],]$area/sum(areas$area)*
              matrix[i,j]/
              sum(matrix[i,])*
              (1-matrix[i,j]/sum(matrix[i,]))/
              (sum(matrix[i,])-1)  
          }
        }
        
        confusion<-data.frame(matrix(nrow=length(legend)+1,ncol=9))
        names(confusion)<-c("class","code","Pa","PaW","Ua","area","area_adj","se","ci")
                
        ### Integration des elements dans le jeu de donnees synthese
        for(i in 1:length(legend)){
          confusion[i,]$class<-areas[areas$code==legend[i],]$class
          confusion[i,]$code<-areas[areas$code==legend[i],]$code
          confusion[i,]$Pa<-matrix[i,i]/sum(matrix[,i])
          confusion[i,]$Ua<-matrix[i,i]/sum(matrix[i,])
          confusion[i,]$PaW<-matrix_w[i,i]/sum(matrix_w[,i])
          confusion[i,]$area_adj<-sum(matrix_w[,i])*sum(areas$area)
          confusion[i,]$area<-areas[areas$code==legend[i],]$area
          confusion[i,]$se<-sqrt(sum(matrix_se[,i]))*sum(areas$area)
          confusion[i,]$ci<-confusion[i,]$se*1.96
        }
        
        ### Calculer la Precision Generale
        confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$area),sum(areas$area),"","")
        confusion
  })  
# ################################################    
# ################ OUtput : Summary of accuracies 
# ################################################
#     
    output$accuracy_all <- renderTable({
                    item<-data.frame(accuracy_all())
                    item<-item[,c("class","PaW","Ua","area_adj")]
                    item$PaW<-floor(as.numeric(item$PaW)*100)
                    item$Ua<-floor(as.numeric(item$Ua)*100)
                    item$area_adj<-floor(as.numeric(item$area_adj))
                    names(item) <-c("Classe","Préc. Prod.","Préc. Util.","Superficie corr. ('000 ha)")
                    item
                    })
#   
# #################################################    
# ################ OUtput item     confusion matrix
# #################################################

    output$matrix_all <- renderTable({
      df <- df_i
      areas <- areas_i
      legend <- legend_i
      
      item<-as.matrix(matrix_all())
      dimnames(item) <- list(areas$class[areas$code %in% as.numeric(legend)],areas$class[areas$code %in% as.numeric(legend)])
      item                                  })


# #################################################    
# ################ OUtput histograms adjusted areas
# #################################################

output$histogram_all <- renderPlot({
  dfa<-as.data.frame(accuracy_all())
  legend <- legend_i
  
  dfa<-dfa[c(1:length(legend)),]
  dfa[dfa=="NaN"]<-0
  dfa$ci<-as.numeric(dfa$ci)
  dfa$area_adj<-as.numeric(dfa$area_adj)
  avg.plot<-qplot(class,area_adj,data=dfa, geom="bar",stat="identity")
  avg.plot+geom_bar()+geom_errorbar(aes(ymax=area_adj+ci, ymin=area_adj-ci))+theme_bw()
})


#############################################################################################

################################################    
################ Matrix for aggregated classes
################################################
matrix_agg <- reactive({
  df <- df_agg()
  areas <- areas_agg()
  legend <- legend_agg
  
  tmp <- as.matrix(table(df$map_code,df$ref_code))
  matrix<-matrix(0,nrow=length(legend),ncol=length(legend))
  
  for(i in 1:length(legend)){
    tryCatch({
      cat(paste(legend[i],"\n"))
      matrix[,i]<-tmp[,legend[i]]
    }, error=function(e){cat("Configuration impossible \n")}
    )
  }
  
  matrix
})

################################################    
################ Table of accuracies
################################################

accuracy_agg <- reactive({
  matrix <- matrix_agg()
  df <- df_agg()
  areas <- areas_agg()
  legend <- legend_agg
  
  matrix_w<-matrix
  for(i in 1:length(legend)){
    for(j in 1:length(legend)){
      matrix_w[i,j]<-matrix[i,j]/sum(matrix[i,])*areas[areas$code==legend[i],]$area/sum(areas$area)
    }}
  
  matrix_se<-matrix
  for(i in 1:length(legend)){
    for(j in 1:length(legend)){
      matrix_se[i,j]<-areas[areas$code==legend[i],]$area/sum(areas$area)*areas[areas$code==legend[i],]$area/sum(areas$area)*
        matrix[i,j]/
        sum(matrix[i,])*
        (1-matrix[i,j]/sum(matrix[i,]))/
        (sum(matrix[i,])-1)  
    }
  }
  
  confusion<-data.frame(matrix(nrow=length(legend)+1,ncol=9))
  names(confusion)<-c("class","code","Pa","PaW","Ua","area","area_adj","se","ci")
    
  ### Integration des elements dans le jeu de donnees synthese
  for(i in 1:length(legend)){
    confusion[i,]$class<-areas[areas$code==legend[i],]$class
    confusion[i,]$code<-areas[areas$code==legend[i],]$code
    confusion[i,]$Pa<-matrix[i,i]/sum(matrix[,i])
    confusion[i,]$Ua<-matrix[i,i]/sum(matrix[i,])
    confusion[i,]$PaW<-matrix_w[i,i]/sum(matrix_w[,i])
    confusion[i,]$area_adj<-sum(matrix_w[,i])*sum(areas$area)
    confusion[i,]$area<-areas[areas$code==legend[i],]$area
    confusion[i,]$se<-sqrt(sum(matrix_se[,i]))*sum(areas$area)
    confusion[i,]$ci<-confusion[i,]$se*1.96
  }
  
  ### Calculer la Precision Generale
  confusion[length(legend)+1,]<-c("Total","",sum(diag(matrix))/sum(matrix[]),sum(diag(matrix_w))/sum(matrix_w[]),"",sum(areas$area),sum(areas$area),"","")
  confusion
})  
# ################################################    
# ################ OUtput : Summary of accuracies 
# ################################################
#     
output$accuracy_agg <- renderTable({
  item<-data.frame(accuracy_agg())
  item<-item[,c("class","PaW","Ua","area_adj")]
  item$PaW<-floor(as.numeric(item$PaW)*100)
  item$Ua<-floor(as.numeric(item$Ua)*100)
  item$area_adj<-floor(as.numeric(item$area_adj))
  names(item) <-c("Classe","Préc. Prod.","Préc. Util.","Superficie corr. ('000 ha)")
  item
})
#   
# #################################################    
# ################ OUtput item     confusion matrix
# #################################################

output$matrix_agg <- renderTable({
  df <- df_agg()
  areas <- areas_agg()
  legend <- legend_agg
  
  item<-as.matrix(matrix_agg())
  dimnames(item) <- list(areas$class[areas$code %in% as.numeric(legend)],areas$class[areas$code %in% as.numeric(legend)])
  item                                  })


# #################################################    
# ################ OUtput histograms adjusted areas
# #################################################

output$histogram_agg <- renderPlot({
  dfa<- data.frame(accuracy_agg())
  legend <- legend_agg
  
  dfa<-dfa[c(1:length(legend)),]
  dfa[dfa=="NaN"]<-0
  dfa$ci<-as.numeric(dfa$ci)
  dfa$area_adj<-as.numeric(dfa$area_adj)
  avg.plot<-qplot(class,area_adj,data=dfa, geom="bar",stat="identity")
  avg.plot+geom_bar()+geom_errorbar(aes(ymax=area_adj+ci, ymin=area_adj-ci))+theme_bw()
})

  })
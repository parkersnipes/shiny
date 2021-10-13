library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(sp)
#library(rgdal)
library(sf)
library(sp)
library(forcedMigration)
library(geosphere)
#library(Matrix)
library(igraph)
#library(shp2graph)
#library(maptools)
library(rgeos)
#library(spdep)
#library(rvest)
#library(rlist)
#library(readr)
#library(xlsx)
#library(reshape2)
library(plotrix)
library(shiny)
library(shinythemes)
library(cowplot)
library(ggmap)

register_google(key = "AIzaSyAt2fwrqNrBhoFIywiLLikFEwibI0Vy4Is")

cross_section_geo = select(cross_section,municipality,lat_mean,lon_mean,popn1993,ruggedness,elevation,slope,V_cum)

addCO <- function(muni){
  paste("CO",muni,sep = "")
}

AttributeTableFinal <- read.csv("Data/AttributeTableFinal.csv")

AttributeTableFinal <- mutate(AttributeTableFinal,latnum = as.numeric(lat),
                              lonnum = as.numeric(lon))


cross_section_geo <- mutate(cross_section_geo,
                            ADM2_PCODE = purrr::map_chr(municipality,addCO))

cross_section_merged <- merge(cross_section_geo,AttributeTableFinal,by = c("ADM2_PCODE"))

muni_pol <- st_read("Data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp")

ll_map <- merge(muni_pol,AttributeTableFinal,by=c("ADM2_PCODE"))

# Create adjacency matrix of shapefile.

munigraph <- read_graph("munigraph.txt",format = "edgelist")

calcdist <- function(lat1,lon1,lat2,lon2){
  return (distGeo(c(lat1,lon1),c(lat2,lon2))/1000)
}

roads = read.csv("Data/real_roads.csv")
road_code_list <- roads$ADM2_PCODE
AttributeTableFinal <- mutate(AttributeTableFinal, has_road = (ADM2_PCODE %in% road_code_list))
weight_matrix <- matrix(,nrow = 1120,ncol = 1120)


summary_1 <- rep(NA,677329)
summary_2 <- rep(NA,677329)
summary_db <- rep(NA,677329)
summary_db_d1 <- rep(NA,677329)


calculateDF <- function(k,h,distanceMetric,granularity,epicenter,a,b){
  #print(width())
  #print(k())
  for(i in 1:1120){
    for(j in 1:1120){
      total_dist<-1000000
      road_factor <- 1
      if(AttributeTableFinal$has_road[i] == 1 && (AttributeTableFinal$has_road[j] == 1 && distanceMetric == 3)){
        road_factor <- k
      }
      if(are.connected(munigraph,i,j)){
        d1 <- calcdist(AttributeTableFinal$latnum[i],AttributeTableFinal$lonnum[i],AttributeTableFinal$latnum[j],AttributeTableFinal$lonnum[j])
        
        total_dist <- d1
        if(i<nrow(cross_section_merged) & j<nrow(cross_section_merged)){
          max_slope <- max(cross_section_merged$slope)
          max_elevation <- max(cross_section_merged$elevation)
          elev_difference <- max((cross_section_merged$elevation[i]-cross_section_merged$elevation[j])/max_elevation,0)
          average_ruggedness <- 0.5*cross_section_merged$ruggedness[i]+0.5*cross_section_merged$ruggedness[j]
          average_slope <- (0.5*cross_section_merged$slope[i]+0.5*cross_section_merged$slope[j])/max_slope
          average_forest <- 0.5*sample(0:1,1)+0.5*sample(0:1,1)
          average_reflectiveness <- 0.5*sample(0:1,1)+0.5*sample(0:1,1)
          
          # Use PCA from our prior analysis.
          PCA_1 <- 0.5*average_slope + 0.5*average_ruggedness
          PCA_2 <- 0.6*elev_difference + 0.3 * average_forest + 0.1 * average_reflectiveness
          summary_db_d1[823*(i-1)+j] <-  d1 + 0.6 * max(elev_difference,0)
          temp <- d1 * exp(a*PCA_1) * exp(b * PCA_2)
          
          summary_db[823*(i-1)+j] <- temp
          if(distanceMetric>1){
            total_dist <- temp * road_factor
          }
          if(distanceMetric>3){
            total_dist <- d1 + 0.6 * elev_difference
          }
        }
        
        
        edge <- get.edge.ids(munigraph,c(i,j))
        munigraph <- set_edge_attr(munigraph,"weight",edge,total_dist)
      }
      weight_matrix[i,j]<-total_dist
    }
  }
  
  
  
  delta_1 = distances(munigraph,v = 259,to = V(munigraph),algorithm = "dijkstra")
  delta_2 = distances(munigraph,v = 1009,to = V(munigraph),algorithm = "dijkstra")
  summary_1<- na.omit(summary_1)
  summary_2 <- na.omit(summary_2)
  summary_db <- na.omit(summary_db)
  summary_db_d1 <- na.omit(summary_db_d1)
  print(summary(summary_1))
  print(summary(summary_db))
  print(summary(summary_db_d1))
  
  
  deltas <- data.frame(matrix(c(delta_1,delta_2),ncol = 2))
  colnames(deltas) <- c("delta_1","delta_2");
  deltas <- mutate(deltas,
                   delta_min = purrr::map2(delta_1,delta_2,min))
  
  var1 <- as.numeric(deltas$delta_min)
  if(epicenter == 2){
    var1 <- as.numeric(deltas$delta_1)
  }
  
  delta_min_vector <- vector(length = 1120)
  vertex_ids = vector(length = 1120)
  for(i in 1:1120){
    vertex_ids[i]<- AttributeTableFinal$ADM2_PCODE[i]
    delta_min_vector[i] <- var1[i]
  }
  
  merge_deltamins <<- data.frame(delta_min = delta_min_vector,ADM2_PCODE = vertex_ids)
  FinalWithDeltamins <<- merge(cross_section_merged,merge_deltamins,by = "ADM2_PCODE")
  
  violence_data <- read.csv("Data/Book1.csv")
  violence_data <- mutate(violence_data, ADM2_PCODE = addCO(municipality))
  violence_set <- merge(violence_data,FinalWithDeltamins,by="ADM2_PCODE")
  
  
  violence_set <- mutate(violence_set, violence = ifelse(victims__UR>0,year,99999))
  violence_set <- mutate(violence_set, total_violence = ifelse(year==2012,cum_victims_UR,0))
  violence_set <- mutate(violence_set, total_violence_num = as.numeric(total_violence))
  #print(summarize(violence_set$delta_min))
  #violence_set <- mutate(violence_set, ring_num = as.integer())
  violence_set <- mutate(violence_set, ring_num = max(as.integer(10*ecdf(violence_set$delta_min)(delta_min))+1),10)
  
  
  vtot_set <- violence_set
  vtot_set <- select(violence_set,ADM2_PCODE,year,cum_victims_UR,total_violence_num,victims__UR,ring_num)
  
  for(i in 1:nrow(vtot_set)){
    if(vtot_set$year[i] == 2012){
      code = vtot_set$ADM2_PCODE[i]
      for(j in 1:nrow(vtot_set)){
        if(vtot_set$ADM2_PCODE[j] == code){
          vtot_set$total_violence_num[j] <- vtot_set$cum_victims_UR[i]
        }
      }
    }
    
  }
  
  # Part 3 here is just graphic year:share for each ring. Do this for all ring sizes.
  Vtot_cumulative <- select(vtot_set,year,victims__UR,cum_victims_UR,total_violence_num,ADM2_PCODE)
  Vtot_final <- merge(FinalWithDeltamins,vtot_set,by = "ADM2_PCODE")
  Vtot_final <- mutate(Vtot_final, share = victims__UR / total_violence_num)
  Vtot_final <- filter(Vtot_final,total_violence_num>0)
  
  Vtot_final <- mutate(Vtot_final, ring_num = max(as.integer(10*ecdf(Vtot_final$delta_min)(delta_min))+1),10)
  start_years <- seq(1996,2011,granularity)
  yearshare_matrix <- matrix(0,nrow= length(start_years),ncol = 12)
  stderr_matrix <- matrix(0,nrow= length(start_years),ncol = 12)
  ring_count_list <- matrix(0,nrow= 1,ncol = 12)
  
  for(index in 1:length(start_years)){
    year_index <- start_years[index]
    for(ring in 1:21){
      ring_index <- min(ring,12)
      endpt <- min(2012,year_index+granularity-1)
      year_range <- year_index:endpt
      temp_Vtot_Final <- filter(Vtot_final,year %in% year_range &ring_num == ring)
      ring_count <- nrow(temp_Vtot_Final)/length(year_range)
      yearshare_list <- temp_Vtot_Final$share
      stderr_matrix[index,ring_index] <- sd(yearshare_list)
      yearshare_matrix[index,ring_index] <- sum(yearshare_list)/ring_count
      ring_count_list[ring_index] <- ring_count
    }
    
  }
  pd = position_dodge2(width = NULL,
                       preserve = c("total", "single"),
                       padding = 0.1,
                       reverse = FALSE)
  
  yearshare_df <- data.frame(data = t(yearshare_matrix))
  stderr_df <- data.frame(data = t(stderr_matrix))
  cnames = paste(start_years)
  colnames(yearshare_df) <- cnames
  colnames(stderr_df) <- cnames
  yearshare_df <- cbind("ring" = as.numeric(rownames(yearshare_df)),yearshare_df)
  yearshare_df <- pivot_longer(yearshare_df,cnames,names_to = "year",values_to = "share")
  stderr_df <- cbind("ring" = as.numeric(rownames(stderr_df)),stderr_df)
  stderr_df <- pivot_longer(stderr_df,cnames,names_to = "year",values_to = "se")
  yearshare_df <- merge(yearshare_df,stderr_df,by = c("ring","year"))
  
  
  return(yearshare_df)
}


ui <- fluidPage(
  sliderInput('k', "Select a parameter k (distance reduction factor between municipalities with roads)", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput('distanceMetric',"Select a distance metric",min = 1,max = 4,value = 1,step = 1),
  sliderInput('granularity',"Select a granularity (number of years/rings)",min = 1,max = 6,value = 3,step = 1),
  sliderInput('a','Select parameter a (PCA_1 parameter for metric 2 (0.5*rugged + 0.5*slope))',min = 1,max = 2,value = 1,step = 0.1),
  sliderInput('b','Select parameter b (PCA_2 parameter for metric 2 (0.6*elev_difference + 0.3*forest + 0.1*reflectiveness))',min = 1,max = 2,value = 1,step = 0.1),

  sliderInput('epicenter','Select distance to closest or main epicenter (closest 1, main 2)',min = 1,max = 2,value = 1,step = 1),
  sliderInput('heatmap','Heatmap off/on (on = 2; makes country map smaller if on)',min = 1,max = 2,value = 1,step = 1),
  plotOutput('plot'),
  plotOutput('plot2')
)

server <- function(input, output) {
  output$plot <- renderPlot({
    validate(
      need(input$k, 'Please select parameter k.')
    )
    
    
    granularity <- reactive(input$granularity)
    k <- reactive(input$k)
    distanceMetric <- reactive(input$distanceMetric)
    h <- reactive(input$h)
    epicenter <- reactive(input$epicenter)
    heatmap <- reactive(input$heatmap)
    a <- reactive(input$a)
    b <- reactive(input$b)
    
    yearshare_df_1<- calculateDF(k(),h(),distanceMetric(),granularity(),epicenter(),a(),b())
    
    #FinalWithDeltamins <- merge(AttributeTableFinal,merge_deltamins,by = "ADM2_PCODE")
    #FinalWithDeltamins <- mutate(AttributeTableFinal, ring_num = as.integer(delta_min/as.numeric(width())) +1)
    
    #print(yearshare_df_1)
    
    ll_map <- merge(FinalWithDeltamins,ll_map,by = "ADM2_PCODE")
    ll_map <- mutate(ll_map, ring_num = max(as.integer(10*ecdf(ll_map$delta_min)(delta_min))+1),10)
    
    plot1<-ggplot(yearshare_df_1,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)+facet_wrap(~ ring)
    
    print(ll_map)
    
    
    
    plot3 <- NULL
    yearshare_heatmap = na.omit(yearshare_df_1)
    if(heatmap() == 2){
      plot3<- ggplot(data=yearshare_heatmap,mapping=aes(x=year,y=ring,fill=share))+
        geom_tile()+theme_minimal()+scale_fill_gradient(name="Violence Share",low="darkblue",high="red")+
        scale_x_discrete(breaks = seq(1,12,granularity()),label = paste(seq(1,12,granularity())))
    }
    
    plot_grid(plot1,plot3)
  })
}


shinyApp(ui, server)

library(tidyverse)
library(dplyr)
library(ggplot2)
library(purrr)
library(sp)
library(rgdal)
library(sf)
library(st)
library(forcedMigration)
library(geosphere)
library(Matrix)
library(igraph)
library(shp2graph)
library(maptools)
library(rgeos)
library(spdep)
library(rvest)
library(rlist)
library(readr)
library(xlsx)
library(reshape2)
library(plotrix)
library(shiny)
library(shinythemes)

cross_section_geo = select(cross_section,municipality,lat_mean,lon_mean,popn1993,ruggedness,slope,V_cum)

addCO <- function(muni){
  paste("CO",muni,sep = "")
}

AttributeTableFinal <- read.csv("Data/AttributeTableFinal.csv")

AttributeTableFinal <- mutate(AttributeTableFinal,latnum = as.numeric(lat),
                              lonnum = as.numeric(lon))


cross_section_geo <- mutate(cross_section_geo,
                            ADM2_PCODE = purrr::map_chr(municipality,addCO))

cross_section_merged <- merge(cross_section_geo,AttributeTableFinal,by = c("ADM2_PCODE"))

# Create adjacency matrix of shapefile. 

munigraph <- read_graph("munigraph.txt",format = "edgelist")

calcdist <- function(lat1,lon1,lat2,lon2){
  return (distGeo(c(lat1,lon1),c(lat2,lon2))/1000)
}

roads = read.csv("Data/real_roads.csv")
road_code_list <- roads$ADM2_PCODE
AttributeTableFinal <- mutate(AttributeTableFinal, has_road = (ADM2_PCODE %in% road_code_list))
weight_matrix <- matrix(,nrow = 1120,ncol = 1120)

calculateDF <- function(width,k,distanceMetric,granularity){
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
        d <- calcdist(AttributeTableFinal$latnum[i],AttributeTableFinal$lonnum[i],AttributeTableFinal$latnum[j],AttributeTableFinal$lonnum[j])
        total_dist <- d
        if(i<nrow(cross_section_merged) & j<nrow(cross_section_merged)){
          temp <- road_factor*d/2 * (1+cross_section_merged$ruggedness[i])^(1+cross_section_merged$slope[i]/90)+
            road_factor*d/2 * (1+cross_section_merged$ruggedness[j])^(1+cross_section_merged$slope[j]/90)
          if(distanceMetric>1){
            total_dist <- temp
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
  
  
  deltas <- data.frame(matrix(c(delta_1,delta_2),ncol = 2))
  colnames(deltas) <- c("delta_1","delta_2")
  deltas <- mutate(deltas, 
                   delta_min = purrr::map2(delta_1,delta_2,min))
  
  
  delta_min_vector = vector(length = 1120)
  path_count_vector = vector(length = 1120)
  vertex_ids = vector(length = 1120)
  for(i in 1:1120){
    vertex_ids[i]<- AttributeTableFinal$ADM2_PCODE[i]
    delta_min_vector[i] <- var1[i]
    path_count_vector[i] <- var2[i]
  }
  
  merge_deltamins <- data.frame(delta_min = delta_min_vector,path_count = path_count_vector,ADM2_PCODE = vertex_ids) 
  FinalWithDeltamins <- merge(cross_section_merged,merge_deltamins,by = "ADM2_PCODE")
  
  violence_data <- read.csv("Data/Book1.csv")
  violence_data <- mutate(violence_data, ADM2_PCODE = addCO(municipality))
  violence_set <- merge(violence_data,FinalWithDeltamins,by="ADM2_PCODE")
  
  
  violence_set <- mutate(violence_set, violence = ifelse(victims__UR>0,year,99999))
  violence_set <- mutate(violence_set, total_violence = ifelse(year==2012,cum_victims_UR,0))
  violence_set <- mutate(violence_set, total_violence_num = as.numeric(total_violence))
  violence_set <- mutate(violence_set, ring_num = as.integer(delta_min/as.numeric(width))+1)
  
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
  
  Vtot_final <- mutate(Vtot_final, ring_num = as.integer(delta_min/(as.numeric(width))+1))
  start_years <- seq(1996,2011,granularity)
  yearshare_matrix <- matrix(0,nrow= length(start_years),ncol = 21)
  stderr_matrix <- matrix(0,nrow= length(start_years),ncol = 21)
  ring_count_list <- matrix(0,nrow= 1,ncol = 21)
  
  for(index in 1:length(start_years)){
    year_index <- start_years[index]
    for(ring_index in 1:21){
      endpt <- min(2012,year_index+granularity-1)
      year_range <- year_index:endpt
      temp_Vtot_Final <- filter(Vtot_final,year %in% year_range &ring_num == ring_index)
      ring_count <- nrow(temp_Vtot_Final)/length(year_range)
      yearshare_list <- temp_Vtot_Final$share
      stderr_matrix[index,ring_index] <- std.error(yearshare_list)
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
  
  yearshare_df_1 <- filter(yearshare_df,ring<7)
  yearshare_df_2 <- filter(yearshare_df,ring>=7 & ring<14)
  yearshare_df_3 <- filter(yearshare_df,ring>=14 & ring<21)
  
  return(yearshare_df_1)
}


ui <- fluidPage(
  sliderInput('width', 'Select a width', min = 30, max = 120, value = 60,step = 30),
  sliderInput('k', "Select a parameter k (distance reduction factor between municipalities with roads)", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput('distanceMetric',"Select a distance metric",min = 1,max = 3,value = 1,step = 1),
  sliderInput('granularity',"Select a granularity (number of years)",min = 1,max = 6,value = 3,step = 1),
  plotOutput('plot')
)



server <- function(input, output) {
  output$plot <- renderPlot({
    validate(
      need(input$width, 'Check at least one letter!'),
      need(input$k, 'Please choose a state.')
    )
    
    
    granularity <- reactive(input$granularity)
    width<- reactive(input$width)
    k <- reactive(input$k)
    distanceMetric <- reactive(input$distanceMetric)
    yearshare_df_1 <- calculateDF(width(),k(),distanceMetric(),granularity())
    
    print(yearshare_df_1)
    ggplot(yearshare_df_1,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+ scale_x_discrete(limits=paste(cnames))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)+facet_wrap(~ ring)
    #ggplot(yearshare_heatmap,aes(x = year,y = ring,fill = share)) + geom_bin2d(binwidth = c(20,20))+scale_fill_gradient(low = "royalblue",high = "red")
    #plot(yearshare_df_1$year,yearshare_df_1$share, main = paste(c(input$k, input$width), collapse = ', '))
  })
}

shinyApp(ui, server)
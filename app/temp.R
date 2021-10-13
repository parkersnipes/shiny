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
library(cowplot)

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

muni_pol <- st_read("Data/col muni polygons/col_admbnda_adm2_mgn_20200416.shp",
                    stringsAsFactors=FALSE)

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
calculateDF <- function(width,k,h,distanceMetric,granularity,which6,epicenter){
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
          elev_difference <- abs(cross_section_merged$elevation[i]-cross_section_merged$elevation[j])
          hypotenuse <- (elev_difference^2+(d1*1000)^2)^0.5
          force <- 9.81*(elev_difference/hypotenuse)
          term1 <-1+h*force
          term2 <- (0.5*cross_section_merged$rugged[i]^(1+cross_section_merged$slope[i]/90)+0.5*cross_section_merged$rugged[j]^(1+cross_section_merged$slope[j]/90))
          summary_1[823*(i-1)+j] <- term1
          summary_2[823*(i-1)+j] <- term2
          temp <- term1 * term2
          summary_db[823*(i-1)+j] <- temp
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
  summary_1<- na.omit(summary_1)
  summary_2 <- na.omit(summary_2)
  summary_db <- na.omit(summary_db)
  print(summary(summary_1))
  print(summary(summary_db))
  
  
  deltas <- data.frame(matrix(c(delta_1,delta_2),ncol = 2))
  colnames(deltas) <- c("delta_1","delta_2")
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
  yearshare_df_2 <- filter(yearshare_df,ring>=7)
  
  print(yearshare_df_1)
  to_return <- yearshare_df_1
  if(which6 > 1){
    to_return <- yearshare_df_2
  }
  return(to_return)
}


ui <- fluidPage(
  sliderInput('width', 'Select a width', min = 30, max = 120, value = 60,step = 30),
  sliderInput('k', "Select a parameter k (distance reduction factor between municipalities with roads)", min = 0, max = 1, value = 0.5, step = 0.1),
  sliderInput('distanceMetric',"Select a distance metric",min = 1,max = 3,value = 1,step = 1),
  sliderInput('granularity',"Select a granularity (number of years/rings)",min = 1,max = 6,value = 3,step = 1),
  sliderInput('which6','Select which panel of 6 rings to display',min = 1,max = 2,value = 1,step = 1),
  sliderInput('h','Select parameter h (distance 2 parameter)',min = 1,max = 2,value = 1,step = 0.1),
  sliderInput('epicenter','Select distance to closest or main epicenter (closest 1, main 2)',min = 1,max = 2,value = 1,step = 1),
  sliderInput('timering','Select whether the graph should fix ring or time (ring 1,time 2)',min = 1,max = 2,value = 1,step = 1),
  sliderInput('heatmap','Heatmap off/on (on = 2; makes country map smaller if on)',min = 1,max = 2,value = 1,step = 1),
  plotOutput('plot'),
  uiOutput('matrix')
)

server <- function(input, output) {
  output$plot <- renderPlot({
    validate(
      need(input$width, 'Please select width'),
      need(input$k, 'Please select parameter k.')
    )
    
    
    granularity <- reactive(input$granularity)
    width<- reactive(input$width)
    k <- reactive(input$k)
    distanceMetric <- reactive(input$distanceMetric)
    which6 <- reactive(input$which6)
    h <- reactive(input$h)
    epicenter <- reactive(input$epicenter)
    timering <- reactive(input$timering)
    heatmap <- reactive(input$heatmap)
    yearshare_df_1<- calculateDF(width(),k(),h(),distanceMetric(),granularity(),which6(),epicenter())
    global_container <- reactiveValues()
    global_container$matrix <- observe(calculateDF(k(),distanceMetric(),granularity(),epicenter()))
    ll_map <- merge(ll_map,merge_deltamins,by = "ADM2_PCODE")
    ll_map <- mutate(ll_map, ring_num = as.integer(delta_min/as.numeric(width())) +1)
    
    #print(yearshare_df_1)
    
    
    plot1<-ggplot(yearshare_df_1,aes(x = year,y = share,group = factor(ring)))+geom_point(aes(colour = factor(ring)))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(ring)),width = 0.2)+facet_wrap(~ ring)
    
    if(timering() == 2){
      plot1<-ggplot(yearshare_df_1,aes(x = ring,y = share,group = factor(year)))+geom_point(aes(colour = factor(year)))+ scale_x_discrete(limits=paste(seq(1,12,granularity())))+geom_errorbar( aes(ymin = share-se, ymax = share+se,colour = factor(year)),width = 0.2)+facet_wrap(~ year)
    }
    
    
    plot2<-ggplot() +
      #geom_sf(data = muni_map, fill='white', color = 'grey34',lwd=.05) +
      geom_sf(data = st_as_sf(ll_map), aes(fill=ring_num),color = 'grey34',lwd=.05) +
      #scale_fill_manual(values=c("white","royalblue1"),aesthetics = c("fill"),name= paste("Between 720-630 km from nearest source ", sep="")) +
      #binned_scale(aesthetics = c("fill"),scale_name = "bin",palette = scale,breaks = c(0,10,20,180,240,300),labels = c("km","180km","270km","360km","450km"))
      scale_fill_stepsn(colors = c("red","gold","darkgreen","blue","violet"),values = NULL,space = "Lab",na.value = "grey50",guide = "coloursteps",aesthetics = "fill",breaks = c(1:12),limits= c(1, 12))
    
    
    
    plot3 <- NULL
    
    yearshare_heatmap = na.omit(yearshare_df_1)
    if(heatmap() == 2){
      plot3<- ggplot(data=yearshare_heatmap,mapping=aes(x=year,y=ring,fill=share))+
        geom_tile()+theme_minimal()+scale_fill_gradient(name="Violence Share",low="darkblue",high="red")+
        scale_x_discrete(breaks = seq(1,12,granularity()),label = paste(seq(1,12,granularity())))
    }
    
    plot_grid(plot1,plot2,plot3)
  })
  
  output$matrix <- renderTable(global_matrix$d1)
  
}

shinyApp(ui, server)
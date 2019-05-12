# Project 3 - The Air That I Breathe
# Course - CS 424 Visualization Analytics - Spring'19 

# Team(Group 3): 
# Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu 
# Praveen Chandrasekaran - pchand34@uic.edu 
# Varsha Jayaraman - vjayar6@uic.edu 
# Abdullah Aleem - aaleem2@uic.edu 

#libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(scales)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(splitstackshape)
library(cdlTools)
library(AotClient)
library(tidyr)
library(darksky)
Sys.setenv(DARKSKY_API_KEY = "0bd3b0d26f9540d876f72a488c268588")
library(rgdal)
library(sp)
library(spatstat)  # Used for the dirichlet tessellation function
library(maptools)  # Used for conversion from SPDF to ppp
library(raster)    # Used to clip out thiessen polygons
library(httr)
library(jsonlite)
library(rjson)
library(ropenaq)
library(promises)
library(future)
library(plyr)
plan(multiprocess)
pdf(NULL)

ui <- dashboardPage(
	skin = 'red',
	dashboardHeader(title = "The Air That I Breathe"),
	dashboardSidebar(
	sidebarMenu(
    htmlOutput("UnitConversion"),
		menuItem("Array of Things and Darksky", tabName = "aot"),
		menuItem("OpenAq", tabName = "openaq"),
		menuItem("Weatherbit", tabName = "weatherbit"),
    menuItem("Information", tabName = "information")
	)	
	),
	dashboardBody(

	#custom css
    	tags$head( 
    		tags$style(
    			HTML(".fa-calendar { display:none !important;} 
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.tabbable > .nav-tabs {margin-bottom: 20px !important;} 
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;} 
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:18px;}
    				.dayswithaqitext { font-size:22px;font-weight:bold;margin-bottom:20px; } 
            		.sidebar-menu {margin-top: 60% !important; }
            		.content > .tab-content {margin-top:3%} 
            		a,span,label,.selectize-input,.sw-air-picker { font-size : 20px !important; }
            		#content { font-size : 22px !important; }
            		.row { margin:12px 0px !important;}")
    	)
    ),
    	tabItems(
    		tabItem(
        		tabName = "aot",
        		tabsetPanel(
        			tabPanel("Leaflet",
        				fluidRow(
        					column(6,withSpinner(leafletOutput("leaflet",height = 750))),
        					column(6, withSpinner(DT::dataTableOutput("leafletstats"))),
        					column(6, withSpinner(DT::dataTableOutput("leafletdarkskystats")))
        				),
        				fluidRow(
					    	column(8,withSpinner(plotOutput("dailyleafletline"))),
					    	column(4,withSpinner(plotOutput("dailyleafletbar")))
					    ),
					    fluidRow(
					    	column(8,withSpinner(plotOutput("weeklyleafletline"))),
					    	column(4,withSpinner(plotOutput("weeklyleafletbar")))
					    ),
	    				fluidRow(
	    					column(4,htmlOutput("radio")),
	    					column(4,htmlOutput("getVarieties"))
	    				),
	    				fluidRow(
	    					column(12, htmlOutput("getPollutants"))
	    				)
        			),
        			tabPanel("Table",
        				fluidRow(
					      column(5, withSpinner(DT::dataTableOutput("table"))),
					      column(7, withSpinner(DT::dataTableOutput("tablestats"))),
					      column(7, withSpinner(DT::dataTableOutput("darskystats")))
					    ),
					    fluidRow(
					    	column(8,withSpinner(plotOutput("dailytableline"))),
					    	column(4,withSpinner(plotOutput("dailytablebar")))
					    ),
					    fluidRow(
					    	column(8,withSpinner(plotOutput("weeklytableline"))),
					    	column(4,withSpinner(plotOutput("weeklytablebar")))
					    ),
					    # fluidRow(
					    # 	column(8,withSpinner(plotOutput("dailydarkskytableline"))),
					    # 	column(4,withSpinner(plotOutput("dailydarkskytablebar")))
					    # ),
					    # fluidRow(
					    # 	column(8,withSpinner(plotOutput("weeklydarkskytableline"))),
					    # 	column(4,withSpinner(plotOutput("weeklydarkskytablebar")))
					    # ),
					    fluidRow(
					    	column(6,htmlOutput("tableradiobuttons")),
					    	column(6,htmlOutput("darkskytableradiobuttons"))
					    ),
					    fluidRow(
					    	column(12,htmlOutput("getPollutantstable"))
					    )					    

        			),
        			tabPanel("Heatmap" ,
        				withSpinner(leafletOutput("heatmapAot",height = 750)),
        				fluidRow(

        					column(2,htmlOutput("heatmapselect")),
        					column(6,htmlOutput("heatmapsensorradio")),
        					column(2,htmlOutput("whenradio")),
        					column(2,htmlOutput("statsradio"))
        				),
        				fluidRow(
        					column(6,htmlOutput("heatmappollutantradio"))
        				)
        			)

        		)

    		),
    		tabItem(
    			tabName = "openaq",
          		   		fluidRow(
          		   			column(6,leafletOutput("openaqleaf",height = 750)),
          		   			column(6, dataTableOutput("openaqtable"))
                   		),
                   		fluidRow(
                     		column(12, htmlOutput("Currentnodes"))
                   		),
                   		fluidRow(
                     		column(12, htmlOutput("OpenaqgetPollutants"))
                   		)
    		),
    		tabItem(
    			tabName = "weatherbit",
          		   		fluidRow(
          		   			column(6,leafletOutput("wbitleaf",height = 600)),
                     		column(6, dataTableOutput("wbittable"))
                   		),
                   		fluidRow(
                   			column(12, htmlOutput("wbitoptions"))
                   		),
                   		fluidRow(
                   			column(6, plotOutput("wbitline")),
                   			column(6, dataTableOutput("wbidaily"))	
                   		),
                   		fluidRow(
                   			column(4, plotOutput("wbitmin")),
                   			column(4, plotOutput("wbiavg")),
                   			column(4, plotOutput("wbimax"))
                   		)
    		),
    		tabItem(
    			tabName = "information",
    			div(htmlOutput("content"),style="font-size:18px;font-weight:bold;margin-bottom:20px;")
    		)	

 	   )	

	)
	)

server <- function(input, output, session) { 

RV<-reactiveValues(Clicks=c())

# change default font size
theme_set(theme_light(base_size = 22))

Observations <- reactive({
    invalidateLater(millis = 600000, session)
    Observations = ls.observations(filters = list(size='2000',project = "chicago", order = "desc:timestamp"))

    x<- do.call(rbind, Observations$location.geometry$coordinates)
       colnames(x) <- c("Longitude", "Latitude")
       Observations<-cbind(Observations, x)

    nodes = ls.nodes(filters=list(project="chicago"))
    colnames(nodes)[colnames(nodes)=="vsn"] <- "node_vsn"

    Observations <- left_join(Observations, nodes, by = c("node_vsn"))

    Observations = subset(Observations, select = c(value,uom,sensor_path,node_vsn,Latitude,Longitude,address))

    return(Observations)
})

Temperature <- reactive({
    Observations = Observations()
    Observations_temp = subset(Observations, Observations$sensor_path == "metsense.pr103j2.temperature")
    Temperature <- distinct(Observations_temp, node_vsn, .keep_all = TRUE)
    if(input$unitconv=="imp")
    {
      Temperature$uom = "F"
      Temperature$value = (Temperature$value*1.8) + 32
    }
    Temperature$value = round(Temperature$value, digits = 2)
    Temperature$pollutants = "TEMPERATURE"
    return(Temperature)
})

Light <- reactive({
    Observations = Observations()
    Observations_light = subset(Observations, Observations$sensor_path == "lightsense.tsl250rd.intensity")
    Light <- distinct(Observations_light, node_vsn, .keep_all = TRUE)
    if(input$unitconv=="imp")
    {
      Light$uom = "ft.cd"
      Light$value = (Light$value/1000000)*634527.76
    }
    else
    {
      Light$uom = "m.cd"
      Light$value = (Light$value/1000000)*6830000
    }

    Light$value = round(Light$value, digits = 2)
    Light$pollutants = "LIGHT"
    return(Light)
})

Humidity <- reactive({
    Observations = Observations()
    Observations_humidity = subset(Observations, Observations$sensor_path == "metsense.htu21d.humidity")
    Humidity <- distinct(Observations_humidity, node_vsn, .keep_all = TRUE)
    Humidity$value = round(Humidity$value, digits = 2)
    Humidity$pollutants = "HUMIDITY"
    return(Humidity)
})

Pollutant <- reactive({
    Observations = Observations()
    Observations_pollutant<-Observations[grep("concentration", Observations$sensor_path),,drop=FALSE]
    sensorvector = Observations_pollutant$sensor_path
    Pollutant <- Observations_pollutant%>% separate(sensor_path, c("a", "b"))
    Pollutant$pollutants <- toupper(Pollutant$b)
    Pollutant$sensor_path = sensorvector

    if(input$unitconv=="metric")
    {
      for(pollutant in unique(Pollutant$pollutants))
      {
         if(pollutant == "CO"){
           Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*28.01)/24.45), digits=2)
         }
         if(pollutant == "SO2"){
           Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*64.066)/24.45), digits=2)
         }
         if(pollutant == "NO2"){
           Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*46.0055)/24.45), digits=2)
         }
         if(pollutant == "O3"){
           Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*48)/24.45), digits=2)
         }
         if(pollutant == "REDUCING"){
          Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*23.4)/24.45), digits=2)
         }
         if(pollutant == "OXIDIZING"){
          Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*44.6)/24.45), digits=2)
         }
         if(pollutant == "H2S"){
          Pollutant$value[Pollutant$pollutant == pollutant] = round(((Pollutant$value[Pollutant$pollutant == pollutant]*34)/24.45), digits=2)
          
         }
      }
      Pollutant$uom = "m_m3"
    }
    Pollutant = subset(Pollutant, select = c(value,uom,sensor_path,node_vsn,Latitude,Longitude,pollutants,address))
    return(Pollutant)
})

current_time <- Sys.time()
current_time = `attr<-`(current_time,"tzone","GMT")
time_day <- current_time - 86400
time_week <- current_time - 604800
time_start <- paste0("lt:", format(as.Date(current_time), "%Y-%m-%d"), "T", format(current_time, format="%H:%M:%S"))
time_end_day <- paste0("ge:", format(as.Date(time_day), "%Y-%m-%d"), "T", format(time_day, format = "%H:%M:%S"))
time_end_week <- paste0("ge:", date(time_week), "T", format(time_week, format = "%H:%M:%S"))
all_times_daily <- data.frame(time = seq.POSIXt(from = as.POSIXct(strptime(time_end_day, "ge:%Y-%m-%dT%H:%M"), tz = "GMT"), to = as.POSIXct(current_time), by = "min"))
all_times_weekly <- data.frame(time = seq.POSIXt(as.POSIXct(strptime(time_end_week, "ge:%Y-%m-%dT%H"), tz = "GMT"), as.POSIXct(current_time), by = "hour"))
current_time_darksky <- paste0(format(as.Date(current_time), "%Y-%m-%d"), "T", format(current_time, format = "%H:%M:%S"), "-0600")

nodes <- reactive({
    Observations = Observations()
    nodes = ls.nodes(filters=list(project="chicago"))
    colnames(nodes)[colnames(nodes)=="vsn"] <- "node_vsn"
    x<- do.call(rbind, nodes$location.geometry$coordinates)
    colnames(x) <- c("Longitude", "Latitude")
    nodes<-cbind(nodes, x)
    return(nodes)
})

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = "blue"
)

dataframefortable <- function() {
  
	final <- data.frame()
    Observations=Observations()
    Temperature=Temperature()
    Humidity=Humidity()
    Light=Light()
    Pollutant=Pollutant()
    
    if("temp" %in% input$filtertable)
    {
        
        if(nrow(final)==0)
        {
            final=Temperature
        } 
        else
        {
            final = rbind(final, Temperature)
        }
    }
    if("light" %in% input$filtertable)
    {
        if(nrow(final)==0)
        {
            final=Light
        } 
        else
        {
            final = rbind(final, Light)
        }
    }
    if("humidity" %in% input$filtertable)
    {
        if(nrow(final)==0)
        {
            final=Humidity
        } 
        else
        {
            final = rbind(final, Humidity)
        }
    }

        Pollutant=Pollutant()
        if("SO2" %in% input$pollutantstablecheckbox){
          so2 <- Pollutant[grep("SO2", Pollutant$pollutants),,drop=FALSE]
          so2 <- distinct(so2, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=so2
          } else{
            final = rbind(final, so2)
          }
          #    vec <- paste0(vec, "SO2: ", so2$value)
        }
        
        if("REDUCING" %in% input$pollutantstablecheckbox){
          reducing <- Pollutant[grep("REDUCING", Pollutant$pollutants),,drop=FALSE]
          reducing <- distinct(reducing, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=reducing
          } else{
            final = rbind(final, reducing)
          }
     #     vec <- paste0(vec, "REDUCING GASES: ", so2$value)
        }
        
        if("OXIDIZING" %in% input$pollutantstablecheckbox){
          oxidizing <- Pollutant[grep("OXIDIZING", Pollutant$pollutants),,drop=FALSE]
          oxidizing <- distinct(oxidizing, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=oxidizing
          } else{
            final = rbind(final, oxidizing)
          }
          
        }
        
        if("O3" %in% input$pollutantstablecheckbox){
          o3 <- Pollutant[grep("O3", Pollutant$pollutants),,drop=FALSE]
          o3 <- distinct(o3, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=o3
          } else{
            final = rbind(final, o3)
          }
          
        }
        
        if("NO2" %in% input$pollutantstablecheckbox){
          no2<- Pollutant[grep("NO2", Pollutant$pollutants),,drop=FALSE]
          no2 <- distinct(no2, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=no2
          } else{
            final = rbind(final, no2)
          }
          
        }
        
        if("CO" %in% input$pollutantstablecheckbox){
          co <- Pollutant[grep("CO", Pollutant$pollutants),,drop=FALSE]
          co <- distinct(co, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=co
          } else{
            final = rbind(final, co)
          }
          
          
        }
        
        if("H2S" %in% input$pollutantstablecheckbox){
          h2s <- Pollutant[grep("H2S",Pollutant$pollutants),,drop=FALSE]
          h2s <- distinct(h2s, node_vsn, .keep_all = TRUE)
          if(nrow(final)==0){
            final=h2s
          } else{
            final = rbind(final, h2s)
          }
         
          
        }


    	dframe <- data.frame(node = final$node_vsn, value = round(final$value,digits=2), unit = final$uom, pollutant = final$pollutants , location = final$address)
    	return(dframe)
}


dataframeforleaflet <- function() {

final <- data.frame()
    Observations=Observations()
    Temperature=Temperature()
    Humidity=Humidity()
    Light=Light()
    Pollutant=Pollutant()

	if("temp" %in% input$filter)
	{
		
		if(nrow(final)==0)
		{
	        final=Temperature
	    } 
	    else
	    {
	        final = rbind(final, Temperature)
	    }
	}
	if("light" %in% input$filter)
	{
		if(nrow(final)==0)
		{
	        final=Light
	    } 
	    else
	    {
	        final = rbind(final, Light)
	    }
	}
	if("humidity" %in% input$filter)
	{
		if(nrow(final)==0)
		{
	        final=Humidity
	    } 
	    else
	    {
	        final = rbind(final, Humidity)
	    }
	}

        
	    if("SO2" %in% input$pollutantscheckbox){
	      so2 <- Pollutant[grep("SO2", Pollutant$pollutants),,drop=FALSE]
	      so2 <- distinct(so2, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=so2
	      } else{
	        final = rbind(final, so2)
	      }
	      #    vec <- paste0(vec, "SO2: ", so2$value)
	    }
	    
	    if("REDUCING" %in% input$pollutantscheckbox){
	      reducing <- Pollutant[grep("REDUCING", Pollutant$pollutants),,drop=FALSE]
	      reducing <- distinct(reducing, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=reducing
	      } else{
	        final = rbind(final, reducing)
	      }
	 #     vec <- paste0(vec, "REDUCING GASES: ", so2$value)
	    }
	    
	    if("OXIDIZING" %in% input$pollutantscheckbox){
	      oxidizing <- Pollutant[grep("OXIDIZING", Pollutant$pollutants),,drop=FALSE]
	      oxidizing <- distinct(oxidizing, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=oxidizing
	      } else{
	        final = rbind(final, oxidizing)
	      }
	      
	    }
	    
	    if("O3" %in% input$pollutantscheckbox){
	      o3 <- Pollutant[grep("O3", Pollutant$pollutants),,drop=FALSE]
	      o3 <- distinct(o3, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=o3
	      } else{
	        final = rbind(final, o3)
	      }
	      
	    }
	    
	    if("NO2" %in% input$pollutantscheckbox){
	      no2<- Pollutant[grep("NO2", Pollutant$pollutants),,drop=FALSE]
	      no2 <- distinct(no2, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=no2
	      } else{
	        final = rbind(final, no2)
	      }
	      
	    }
	    
	    if("CO" %in% input$pollutantscheckbox){
	      co <- Pollutant[grep("CO", Pollutant$pollutants),,drop=FALSE]
	      co <- distinct(co, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=co
	      } else{
	        final = rbind(final, co)
	      }
	      
	      
	    }
	    
	    if("H2S" %in% input$pollutantscheckbox){
	      h2s <- Pollutant[grep("H2S",Pollutant$pollutants),,drop=FALSE]
	      h2s <- distinct(h2s, node_vsn, .keep_all = TRUE)
	      if(nrow(final)==0){
	        final=h2s
	      } else{
	        final = rbind(final, h2s)
	      }
	     
	      
	    }
	    if(nrow(final != 0)){
	        final$popup <- paste0("<B>",final$pollutants, "</B>: ",round(final$value, 2))
	        final$popString<-rep("NA", nrow(final)) #popup string to display on leaflet
	        for(node in unique(final$node_vsn)){
	          popDF <- subset(final, node_vsn == node) #DF for every single node
	          val = paste(popDF$popup, sep="\n", collapse="<br>") #concatenates all pollutant values to a string
	          final$popString[final$node_vsn == node] <- val
	          #final$popString<-ifelse(final$node_vsn == node, val)
	        }
	    }

	return(final)    

}

# Grad requirements API variable declaration

wbitfinal <- function(lat, long) {
	final <- data.frame()
for (i in 1:length(lat))
{
	callString <- paste0("https://api.weatherbit.io/v2.0/current?lat=",lat[i],"&lon=",long[i],"&key=",API_KEY)
	result <- fromJSON(file = callString)
	wbit = data.frame(ID = table$node_vsn[i], Lat = result[["data"]][[1]][["lat"]], Long = result[["data"]][[1]][["lon"]], RelativeHumidity = result[["data"]][[1]][["rh"]], Solar_rad = result[["data"]][[1]][["solar_rad"]], Feels_like = result[["data"]][[1]][["app_temp"]], DewPoint = result[["data"]][[1]][["dewpt"]], Clouds = result[["data"]][[1]][["clouds"]], Visibility = result[["data"]][[1]][["vis"]], Preci = result[["data"]][[1]][["pres"]], UV_Index = result[["data"]][[1]][["uv"]])
	final <- rbind(final, wbit)
}
return(final)	
}

API_KEY<-"2da9ff78670941be99701d180f983814"
C_time <- Sys.time()

C_time_openaq <- Sys.time()
as.numeric(C_time_openaq)
attr(C_time_openaq, "tzone") <- "UTC" 
C_date_openaq <- date(C_time_openaq)

C_date_openaq_daily <- C_date_openaq - 1
C_date_openaq_weekly <- C_date_openaq - 7

C_time <- date(C_time)
P_time <- C_time - 1
table <- ls.observations(filters = list(size='1000',project = "chicago", order = "desc:timestamp"))
x<- do.call(rbind, table$location.geometry$coordinates)
colnames(x) <- c("longitude", "latitude")
table <- cbind(table, x)
table <- distinct(table, node_vsn, .keep_all = TRUE)
table <- subset(table, select = c(node_vsn, latitude, longitude))

OpenAqtable <- reactive({
  Observations = Observations()
  return(aq_latest(country="US", city="Chicago-Naperville-Joliet"))
})

lat = table$latitude
long = table$longitude
final <- future(wbitfinal(lat, long))

conversion <- function(data, sensor)
{

  if(input$unitconv=="imp")
  {

      if(sensor == "metsense.pr103j2.temperature")
      {
        data$value = round((data$value*1.8) + 32,digits=2)   
      }
      if(sensor == "lightsense.tsl250rd.intensity")
      {
        data$value = round((data$value/1000000)*634527.76,digits=2)
      }
      
  }
  else
  {

      if(sensor == "chemsense.reducing_gases.concentration")
      {
          data$value = round(((data$value*23.4)/24.45), digits=2)
      }
      if(sensor == "chemsense.oxidizing_gases.concentration")
      {
          data$value = round(((data$value*44.6)/24.45), digits=2)    
      }
      if(sensor == "chemsense.so2.concentration")
      {
          data$value = round(((data$value*64.066)/24.45), digits=2)    
      }
      if(sensor == "chemsense.o3.concentration")
      {
          data$value = round(((data$value*48)/24.45), digits=2)
      }
      if(sensor == "chemsense.no2.concentration")
      {
          data$value = round(((data$value*46.0055)/24.45), digits=2)
      }
      if(sensor == "chemsense.h2s.concentration")
      {
          data$value = round(((data$value*34)/24.45), digits=2)
      }
      if(sensor == "chemsense.co.concentration")
      {
          data$value = round(((data$value*28.01)/24.45), digits=2)
      }



  }
  return(data)

}


observeEvent(input$leaflet_marker_click, {

	click <- input$leaflet_marker_click
    if(length(RV$Clicks))
    {
		if(RV$Clicks[length(RV$Clicks)]!=click[1])
		{
   			RV$Clicks<-c(RV$Clicks,toString(click[1]))
   			if(length(RV$Clicks)==3)
   			{
   				RV$Clicks<-RV$Clicks[2:3] #Stores last two nodes selected from leaflet
   			}
		}
    }
   else
   {
   		RV$Clicks<-c(RV$Clicks,toString(click[1]))
   }

})

output$leaflet <- renderLeaflet({

	final <- dataframeforleaflet()

    map <- leaflet()
    if("streetview" %in% input$varietiesRadioButton){
      map<-addTiles(map)
    }
    if("mountainous" %in% input$varietiesRadioButton){
      map<-addProviderTiles(map, "Stamen.Terrain")
    }
    if("light" %in% input$varietiesRadioButton){
      map<-addProviderTiles(map, "CartoDB.Positron")
    }
    map <- setView(map, lng = final$Longitude[5], lat = final$Latitude[5], zoom = 10)
    map <- addAwesomeMarkers(map, layerId = final$node_vsn, lng = final$Longitude, lat = final$Latitude, label = paste0(final$node_vsn), popup = paste0(final$popString),icon=icons)
    map
})

output$radio = renderUI({
checkboxGroupInput("filter", "Select filter", choices = list("Temperature(C)" = "temp", "Light Intensity(uW/cm^2)" = "light","Humidity(RH)" = "humidity","Pollutant(PPM)" = "pollutant"), 
    selected = "temp",inline=TRUE)
})

output$tableradiobuttons = renderUI({
checkboxGroupInput("filtertable", "Select filter", choices = list("Temperature(C)" = "temp", "Light Intensity(uW/cm^2)" = "light","Humidity(RH)" = "humidity","Pollutant(PPM)" = "pollutant"), 
    selected = "temp",inline=TRUE)
})

output$getVarieties = renderUI({
    radioButtons("varietiesRadioButton", "Select Variety", c("Mountainous" = "mountainous","Street View" = "streetview", "Light" = "light"),inline=TRUE, selected = c("streetview"))
})

output$getPollutants = renderUI({
    
   if('pollutant' %in% input$filter)
   {
	   	pollutants = c("SO2","NO2","H2S","CO","O3","REDUCING","OXIDIZING")
    	checkboxGroupInput("pollutantscheckbox", label = "Choose Pollutant", choices = unique(pollutants),selected=pollutants[1],inline=TRUE)
	}
})

output$getPollutantstable = renderUI({
    
   if('pollutant' %in% input$filtertable)
   {
	  	pollutants = c("SO2","NO2","H2S","CO","O3","REDUCING","OXIDIZING")
    	checkboxGroupInput("pollutantstablecheckbox", label = "Choose Pollutant", choices = unique(pollutants),selected=pollutants[1],inline=TRUE)
	}
})


output$table = DT::renderDataTable({

	dframe = dataframefortable()	    	
    dframe
  } ,selection = "multiple", options = list(searching = TRUE, pageLength = 10, info = FALSE, paging = TRUE),callback=DT::JS("return table;")
)

output$tablestats = DT::renderDataTable({

		dframe = dataframefortable()
        s = input$table_rows_selected
        if (length(s)) 
        {

            pollutantsel = toString(dframe$pollutant[s[length(s)]])
            pollutantsel = tolower(pollutantsel)
            if(pollutantsel=="temperature")
            {
                sensor = "metsense.pr103j2.temperature"
            }
            if(pollutantsel=="light")
            {
                sensor = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel=="humidity")
            {
                sensor = "metsense.htu21d.humidity"
            }
            if(pollutantsel=="reducing" || pollutantsel=="oxidizing")
            {
                sensor = paste0("chemsense.",pollutantsel,"_gases.concentration")
            }
            if(pollutantsel=="so2" || pollutantsel=="o3" || pollutantsel=="no2" || pollutantsel=="h2s" || pollutantsel=="co")
            {
                sensor = paste0("chemsense.",pollutantsel,".concentration") 
            }
            
            statsDaily <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor, size = 10000))
            statsWeekly <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor, size = 99999))
            
            statsDaily <- conversion(statsDaily,sensor)
            statsWeekly <- conversion(statsWeekly,sensor)

            node1_name = toString(dframe$node[s[length(s)]])
            value1_pol = toString(dframe$value[s[length(s)]])
            if(length(s) > 1)
            {
                pollutantsel2 = toString(dframe$pollutant[s[length(s)-1]])
                pollutantsel2 = tolower(pollutantsel2)
                
                if(pollutantsel2=="temperature")
                {
                    sensor2 = "metsense.pr103j2.temperature"
                }
                if(pollutantsel2=="light")
                {
                    sensor2 = "lightsense.tsl250rd.intensity"
                }
                if(pollutantsel2=="humidity")
                {
                    sensor2 = "metsense.htu21d.humidity"
                }
                if(pollutantsel2=="reducing" || pollutantsel2=="oxidizing")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,"_gases.concentration")
                }
                if(pollutantsel2=="so2" || pollutantsel2=="o3" || pollutantsel2=="no2" || pollutantsel2=="h2s" || pollutantsel2=="co")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,".concentration") 
                }

                statsDaily2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)-1]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor2, size = 10000))
                statsWeekly2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)-1]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor2, size = 99999))
                
                statsDaily2 <- conversion(statsDaily2,sensor2)
                statsWeekly2 <- conversion(statsWeekly2,sensor2)

                node2_name = toString(dframe$node[s[length(s)-1]])
                value2_pol = toString(dframe$value[s[length(s)-1]])

                data = data.frame( c(value1_pol,value2_pol),c(round(mean(statsDaily$value),digits = 2),round(mean(statsDaily2$value),digits = 2)),c(round(max(statsDaily$value),digits =2),round(max(statsDaily2$value),digits =2)),c(round(min(statsDaily$value),digits =2),round(min(statsDaily2$value),digits =2)),c(round(mean(statsWeekly$value),digits =2),round(mean(statsWeekly2$value),digits =2)),c(round(max(statsWeekly$value),digits =2),round(max(statsWeekly2$value),digits =2)),c(round(min(statsWeekly$value),digits =2),round(min(statsWeekly2$value),digits =2)) )
                colnames(data) <- c("Current", "Last 24 hours(Mean)", "Last 24 hours(Max)", "Last 24 hours(Min)", "Last 7 days(Mean)", "Last 7 days(Max)", "Last 7 days(Min)")
                row.names(data) <- c(node1_name,node2_name)
            }
            else
            {
                data = data.frame( c(value1_pol),c(round(mean(statsDaily$value),digits = 2)),c(round(max(statsDaily$value),digits =2)),c(round(min(statsDaily$value),digits =2)),c(round(mean(statsWeekly$value),digits =2)),c(round(max(statsWeekly$value),digits =2)),c(round(min(statsWeekly$value),digits =2)) )
                colnames(data) <- c("Current", "Last 24 hours(Mean)", "Last 24 hours(Max)", "Last 24 hours(Min)", "Last 7 days(Mean)", "Last 7 days(Max)", "Last 7 days(Min)")
                row.names(data) <- c(node1_name)    
            }
            data
        }    

  } ,options = list(searching = FALSE, info = FALSE, paging = FALSE))


 
output$darskystats = DT::renderDataTable({

    dframe = dataframefortable()
    Observations=Observations()
    s = input$table_rows_selected
        if (length(s)) 
        {
            selected_node = toString(dframe$node[s[length(s)]])
            data = subset(Observations, Observations$node_vsn == selected_node)
            data = distinct(data, node_vsn, .keep_all = TRUE)
            lat = data$Latitude
            lng = data$Longitude
            darksydata <- get_current_forecast(lat, lng)
            temperature = darksydata[["currently"]][["temperature"]]
            temperature_c = round((temperature - 32)* 0.55,digits = 2)
            if(length(s)>1)
            {
                selected_node_2 = toString(dframe$node[s[length(s)-1]])
                data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
                data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
                lat_2 = data_2$Latitude
                lng_2 = data_2$Longitude
                darksydata_2 <- get_current_forecast(lat_2, lng_2)
                temperature_2 = darksydata_2[["currently"]][["temperature"]]
                temperature_2_c = round((temperature_2 - 32)* 0.55,digits = 2)

                if(input$unitconv=="metric")
                {
                    finaldata = data.frame(c(temperature_c,temperature_2_c),c(darksydata[["currently"]][["humidity"]],darksydata_2[["currently"]][["humidity"]]),c(darksydata[["currently"]][["windSpeed"]],darksydata_2[["currently"]][["windSpeed"]]),c(darksydata[["currently"]][["windBearing"]],darksydata_2[["currently"]][["windBearing"]]),c(darksydata[["currently"]][["cloudCover"]],darksydata_2[["currently"]][["cloudCover"]]),c(darksydata[["currently"]][["visibility"]],darksydata_2[["currently"]][["visibility"]]),c(darksydata[["currently"]][["pressure"]],darksydata_2[["currently"]][["pressure"]]),c(darksydata[["currently"]][["ozone"]],darksydata_2[["currently"]][["ozone"]]),c(darksydata[["currently"]][["summary"]],darksydata_2[["currently"]][["summary"]]))
                }
                else
                {
                    finaldata = data.frame(c(temperature,temperature_2),c(darksydata[["currently"]][["humidity"]],darksydata_2[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])*0.62,(darksydata_2[["currently"]][["windSpeed"]])*0.62),c((darksydata[["currently"]][["windBearing"]])*0.62,(darksydata_2[["currently"]][["windBearing"]])*0.62),c(darksydata[["currently"]][["cloudCover"]],darksydata_2[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])*0.62,(darksydata_2[["currently"]][["visibility"]])*0.62),c((darksydata[["currently"]][["pressure"]])*0.0145038,(darksydata_2[["currently"]][["pressure"]])*0.0145038),c((darksydata[["currently"]][["ozone"]]),(darksydata_2[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]],darksydata_2[["currently"]][["summary"]]))
                }

                colnames(finaldata) <- c("Temperature", "Humidity", "Wind Speed", "Wind Bearing", "Cloud Cover", "Visibility","Pressure","Ozone","Summary")
                row.names(finaldata) <- c(selected_node,selected_node_2)
                finaldata
            }
            else
            {
                if(input$unitconv=="metric")
                {
                    finaldata = data.frame(c(temperature_c),c(darksydata[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])),c((darksydata[["currently"]][["windBearing"]])),c(darksydata[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])),c((darksydata[["currently"]][["pressure"]])),c((darksydata[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]]))
                }
                else
                {
                    finaldata = data.frame(c(temperature),c(darksydata[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])*0.62),c((darksydata[["currently"]][["windBearing"]])*0.62),c(darksydata[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])*0.62),c((darksydata[["currently"]][["pressure"]])*0.0145038),c((darksydata[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]]))
                }
                colnames(finaldata) <- c("Temperature", "Humidity", "Wind Speed", "Wind Bearing", "Cloud Cover", "Visibility","Pressure","Ozone","Summary")
                row.names(finaldata) <- c(selected_node)
                finaldata   
            }
            
        }

    },options = list(searching = FALSE, info = FALSE, paging = FALSE))


output$leafletstats = DT::renderDataTable({

		sensorVec <- c()
		pollutantsVec <- c()
		if(length(RV$Clicks))
    	{
			if('temp' %in% input$filter)
			{
				sensorVec <- rbind(sensorVec, "metsense.pr103j2.temperature")
				pollutantsVec <- rbind(pollutantsVec, "TEMPERATURE")
			}
			if('light' %in% input$filter)
			{
				sensorVec <- rbind(sensorVec, "lightsense.tsl250rd.intensity")
				pollutantsVec <- rbind(pollutantsVec, "LIGHT")
			}
			if('humidity' %in% input$filter)
			{
				sensorVec <- rbind(sensorVec, "metsense.htu21d.humidity")
				pollutantsVec <- rbind(pollutantsVec, "HUMIDITY")
			}

		    	if("SO2" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.so2.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "SO2")
		    	}

		    	if("REDUCING" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.reducing_gases.concentration")		    		
		    		pollutantsVec <- rbind(pollutantsVec, "REDUCING")
		    	}

		    	if("OXIDIZING" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.oxidizing_gases.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "OXIDIZING")
		    	}

		    	if("O3" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.o3.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "O3")
		    	}

		    	if("NO2" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.no2.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "NO2")
		    	}

		    	if("H2S" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.h2s.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "H2S")
		    	}

		    	if("CO" %in% input$pollutantscheckbox)
		    	{
		    		sensorVec <- rbind(sensorVec, "chemsense.co.concentration")
		    		pollutantsVec <- rbind(pollutantsVec, "CO")
		    	}

		    	final <- dataframeforleaflet()

		    	s = input$leaflet_marker_click
				if (length(RV$Clicks)) 
				{	
					data <- data.frame()
					rowNames <- c()
					index = 1
					for(index in 1:length(sensorVec)){
						statsDaily <- data.frame()
					
						statsWeekly <- data.frame()
						statsWeekly <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_week, sensor = toString(sensorVec[index]), size = 700))
						statsDaily <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_day, sensor = toString(sensorVec[index]), size = 700))
						
            statsDaily <- conversion(statsDaily,toString(sensorVec[index]))
            statsWeekly <- conversion(statsWeekly,toString(sensorVec[index]))

						node1_name <- toString(RV$Clicks[1])	
						value_check_1 = subset(final$value, final$node_vsn == toString(RV$Clicks[1]) & final$pollutants == pollutantsVec[index])					
						if(length(value_check_1))
						{
							value1_pol = value_check_1
						}
						else
						{
							value1_pol = 0
						}

						
						
						if(length(RV$Clicks) > 1)
						{
							statsDaily2 <- data.frame()
							statsWeekly2 <- data.frame()
							statsDaily2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_day, sensor = toString(sensorVec[index]), size = 10000))
							statsWeekly2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_week, sensor = toString(sensorVec[index]), size = 99999))
						
              statsDaily2 <- conversion(statsDaily2,toString(sensorVec[index]))
              statsWeekly2 <- conversion(statsWeekly2,toString(sensorVec[index]))

							node2_name = toString(RV$Clicks[2])
							value_check_2 <- subset(final$value, final$node_vsn == toString(RV$Clicks[2]) & final$pollutants == pollutantsVec[index])
							
							if(length(value_check_2))
							{
								value2_pol = value_check_2
							}
							else
							{
								value2_pol = 0
							}

							data = rbind(data, data.frame( c(value1_pol,value2_pol),c(round(mean(statsDaily$value),digits = 2),round(mean(statsDaily2$value),digits = 2)),c(round(max(statsDaily$value),digits =2),round(max(statsDaily2$value),digits =2)),c(round(min(statsDaily$value),digits =2),round(min(statsDaily2$value),digits =2)),c(round(mean(statsWeekly$value),digits =2),round(mean(statsWeekly2$value),digits =2)),c(round(max(statsWeekly$value),digits =2),round(max(statsWeekly2$value),digits =2)),c(round(min(statsWeekly$value),digits =2),round(min(statsWeekly2$value),digits =2)) ))
							rowNames <- rbind(rowNames, paste0(node1_name,"-", pollutantsVec[index]), paste0(node2_name, "-", pollutantsVec[index]))
						}
						else
						{
							data = rbind(data, data.frame( c(value1_pol),c(round(mean(statsDaily$value),digits = 2)),c(round(max(statsDaily$value),digits =2)),c(round(min(statsDaily$value),digits =2)),c(round(mean(statsWeekly$value),digits =2)),c(round(max(statsWeekly$value),digits =2)),c(round(min(statsWeekly$value),digits =2)) ))
							rowNames <- rbind(rowNames, paste0(node1_name, "-", pollutantsVec[index]))
						}
					}	
					row.names(data) <- rowNames
					colnames(data) <- c("Current", "Last 24 hours(Mean)", "Last 24 hours(Max)", "Last 24 hours(Min)", "Last 7 days(Mean)", "Last 7 days(Max)", "Last 7 days(Min)")
					data
				}

				

		}	

			},options = list(searching = FALSE, info = FALSE, paging = FALSE))


output$leafletdarkskystats = DT::renderDataTable({

    Observations=Observations()
    nodes = nodes()
	if(length(RV$Clicks))
    {
		if(input$filter=='temp')
		{
			sensor = "metsense.pr103j2.temperature"
		}
		else if(input$filter=='light')
		{
			sensor = "lightsense.tsl250rd.intensity"
		}
		else if(input$filter=='humidity')
		{
			sensor = "metsense.htu21d.humidity"
		}

		if(input$filter!="pollutant")
		{
			obs <- ls.observations(filters=list(size="1000", project = "chicago", sensor = sensor))
			obs <- distinct(obs, node_vsn, .keep_all = TRUE)
			obs <- left_join(obs, nodes, by = c("node_vsn"))
			dframe <- data.frame(node = obs$node_vsn, value = obs$value, unit = obs$uom, location = obs$address, stringsAsFactors=F)

		}
		else
		{
			obs <- ls.observations(filters=list(size="1000", project = "chicago"))
	    	obs <- obs[grep("concentration", obs$sensor_path),,drop=FALSE]
	    	obs <- obs%>% separate(sensor_path, c("a", "b"))
			obs$pollutants <- toupper(obs$b)
	    	obs <- left_join(obs, nodes, by = c("node_vsn"))

	    	final <- data.frame()

	    	if("SO2" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "SO2")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}

	    	if("REDUCING" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "REDUCING")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}	
	    		
	    	}

	    	if("OXIDIZING" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "OXIDIZING")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}

	    	if("O3" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "O3")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}

	    	if("NO2" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "NO2")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}

	    	if("H2S" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "H2S")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)   		
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}

	    	if("CO" %in% input$pollutantscheckbox)
	    	{
	    		data = subset(obs, obs$pollutants == "CO")
	    		data = subset(data, select=c(node_vsn,value,uom,pollutants,address))
	    		data = distinct(data, node_vsn, .keep_all = TRUE)	
	    		if(nrow(final)==0)
	    		{
	    			final = data
	    		}
	    		else
	    		{
	    			final = rbind(final, data)
	    		}
	    	}


	    	dframe <- data.frame(node = final$node_vsn, value = final$value, unit = final$uom, pollutant = final$pollutants , location = final$address)

		}	

		s = input$leaflet_marker_click
			if (length(RV$Clicks)) 
			{
			 	selected_node = toString(RV$Clicks[1])
				data = subset(Observations, Observations$node_vsn == selected_node)
				data = distinct(data, node_vsn, .keep_all = TRUE)
				lat = data$Latitude
				lng = data$Longitude
				darksydata <- get_current_forecast(lat, lng)
				temperature = darksydata[["currently"]][["temperature"]]
				temperature_c = round((temperature - 32)* 0.55,digits = 2)
				if(length(RV$Clicks) > 1)
				{
					selected_node_2 = toString(RV$Clicks[2])
					data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
					data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
					lat_2 = data_2$Latitude
					lng_2 = data_2$Longitude
					darksydata_2 <- get_current_forecast(lat_2, lng_2)
					temperature_2 = darksydata_2[["currently"]][["temperature"]]
					temperature_2_c = round((temperature_2 - 32)* 0.55,digits = 2)

          if(input$unitconv=="metric")
          {
              finaldata = data.frame(c(temperature_c,temperature_2_c),c(darksydata[["currently"]][["humidity"]],darksydata_2[["currently"]][["humidity"]]),c(darksydata[["currently"]][["windSpeed"]],darksydata_2[["currently"]][["windSpeed"]]),c(darksydata[["currently"]][["windBearing"]],darksydata_2[["currently"]][["windBearing"]]),c(darksydata[["currently"]][["cloudCover"]],darksydata_2[["currently"]][["cloudCover"]]),c(darksydata[["currently"]][["visibility"]],darksydata_2[["currently"]][["visibility"]]),c(darksydata[["currently"]][["pressure"]],darksydata_2[["currently"]][["pressure"]]),c(darksydata[["currently"]][["ozone"]],darksydata_2[["currently"]][["ozone"]]),c(darksydata[["currently"]][["summary"]],darksydata_2[["currently"]][["summary"]]))
          }
          else
          {
              finaldata = data.frame(c(temperature,temperature_2),c(darksydata[["currently"]][["humidity"]],darksydata_2[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])*0.62,(darksydata_2[["currently"]][["windSpeed"]])*0.62),c((darksydata[["currently"]][["windBearing"]])*0.62,(darksydata_2[["currently"]][["windBearing"]])*0.62),c(darksydata[["currently"]][["cloudCover"]],darksydata_2[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])*0.62,(darksydata_2[["currently"]][["visibility"]])*0.62),c((darksydata[["currently"]][["pressure"]])*0.0145038,(darksydata_2[["currently"]][["pressure"]])*0.0145038),c((darksydata[["currently"]][["ozone"]]),(darksydata_2[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]],darksydata_2[["currently"]][["summary"]]))
          }
					
					colnames(finaldata) <- c("Temperature", "Humidity", "Wind Speed", "Wind Bearing", "Cloud Cover", "Visibility","Pressure","Ozone","Summary")
					row.names(finaldata) <- c(selected_node,selected_node_2)
					finaldata
				}
				else
				{
					
          if(input$unitconv=="metric")
          {
              finaldata = data.frame(c(temperature_c),c(darksydata[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])),c((darksydata[["currently"]][["windBearing"]])),c(darksydata[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])),c((darksydata[["currently"]][["pressure"]])),c((darksydata[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]]))
          }
          else
          {
              finaldata = data.frame(c(temperature),c(darksydata[["currently"]][["humidity"]]),c((darksydata[["currently"]][["windSpeed"]])*0.62),c((darksydata[["currently"]][["windBearing"]])*0.62),c(darksydata[["currently"]][["cloudCover"]]),c((darksydata[["currently"]][["visibility"]])*0.62),c((darksydata[["currently"]][["pressure"]])*0.0145038),c((darksydata[["currently"]][["ozone"]])),c(darksydata[["currently"]][["summary"]]))
          }


					colnames(finaldata) <- c("Temperature", "Humidity", "Wind Speed", "Wind Bearing", "Cloud Cover", "Visibility","Pressure","Ozone","Summary")
					row.names(finaldata) <- c(selected_node)
					finaldata	
				}
				
			}
	}		

	},options = list(searching = FALSE, info = FALSE, paging = FALSE))


output$dailytableline <- renderPlot({


		dframe = dataframefortable()

		s = input$table_rows_selected
		if (length(s)) 
		{
			plot = ggplot() + scale_x_datetime(date_labels = "%H:%M" , date_breaks = "1 hour") +
			labs(x = NULL, y = "Values", title = NULL)+ 
			theme_light(18)+
			theme(legend.title = element_text(color = "white"), panel.grid = element_blank(),legend.text=element_text(size=12),legend.key = element_rect(fill = "grey30"),panel.background = element_rect(fill = 'grey30'))+
	        guides(color=guide_legend(keywidth = 3, keyheight = 2))

	        node1Label = paste0(dframe$node[s[length(s)]], " - ", dframe$pollutant[s[length(s)]])

	        pollutantsel = toString(dframe$pollutant[s[length(s)]])
            pollutantsel = tolower(pollutantsel)
            if(pollutantsel=="temperature")
            {
                sensor = "metsense.pr103j2.temperature"
            }
            if(pollutantsel=="light")
            {
                sensor = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel=="humidity")
            {
                sensor = "metsense.htu21d.humidity"
            }
            if(pollutantsel=="reducing" || pollutantsel=="oxidizing")
            {
                sensor = paste0("chemsense.",pollutantsel,"_gases.concentration")
            }
            if(pollutantsel=="so2" || pollutantsel=="o3" || pollutantsel=="no2" || pollutantsel=="h2s" || pollutantsel=="co")
            {
                sensor = paste0("chemsense.",pollutantsel,".concentration") 
            }

			statsDaily1 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor, size = 10000))
			statsDaily1 <- conversion(statsDaily1,sensor)
      statsDaily1$time <- as.POSIXct(strptime(statsDaily1$timestamp, "%Y-%m-%dT%H:%M"), tz = "GMT")
			statsDaily1Aggregate <- aggregate(value~time,statsDaily1,mean)
			finalDaily1 <- left_join(all_times_daily, statsDaily1Aggregate, by = c("time"))
			plot = plot + geom_line(aes(x=finalDaily1$time, y=finalDaily1$value, colour = node1Label))
			if(length(s) > 1)
			{
				node2Label = paste0(dframe$node[s[length(s)-1]], " - ", dframe$pollutant[s[length(s)-1]])

				pollutantsel2 = toString(dframe$pollutant[s[length(s)-1]])
            	pollutantsel2 = tolower(pollutantsel2)
				if(pollutantsel2=="temperature")
                {
                    sensor2 = "metsense.pr103j2.temperature"
                }
                if(pollutantsel2=="light")
                {
                    sensor2 = "lightsense.tsl250rd.intensity"
                }
                if(pollutantsel2=="humidity")
                {
                    sensor2 = "metsense.htu21d.humidity"
                }
                if(pollutantsel2=="reducing" || pollutantsel2=="oxidizing")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,"_gases.concentration")
                }
                if(pollutantsel2=="so2" || pollutantsel2=="o3" || pollutantsel2=="no2" || pollutantsel2=="h2s" || pollutantsel2=="co")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,".concentration")
                }    
				statsDaily2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)-1]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor2, size = 10000))
				statsDaily2 <- conversion(statsDaily2,sensor2)
        statsDaily2$time <- as.POSIXct(strptime(statsDaily2$timestamp, "%Y-%m-%dT%H:%M"), tz = "GMT")
				statsDaily2Aggregate <- aggregate(value~time,statsDaily2,mean)
				finalDaily2 <- left_join(all_times_daily, statsDaily2Aggregate, by = c("time"))
				plot = plot + geom_line(aes(x=finalDaily2$time, y=finalDaily2$value, colour = node2Label))

			}

			plot

		}


})

output$dailytablebar <- renderPlot({

		dframe = dataframefortable()

		s = input$table_rows_selected
		if (length(s) > 1) 
		{
			pollutantsel = toString(dframe$pollutant[s[length(s)]])
            pollutantsel = tolower(pollutantsel)
            if(pollutantsel=="temperature")
            {
                sensor = "metsense.pr103j2.temperature"
            }
            if(pollutantsel=="light")
            {
                sensor = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel=="humidity")
            {
                sensor = "metsense.htu21d.humidity"
            }
            if(pollutantsel=="reducing" || pollutantsel=="oxidizing")
            {
                sensor = paste0("chemsense.",pollutantsel,"_gases.concentration")
            }
            if(pollutantsel=="so2" || pollutantsel=="o3" || pollutantsel=="no2" || pollutantsel=="h2s" || pollutantsel=="co")
            {
                sensor = paste0("chemsense.",pollutantsel,".concentration") 
            }

            pollutantsel2 = toString(dframe$pollutant[s[length(s)-1]])
        	pollutantsel2 = tolower(pollutantsel2)
			if(pollutantsel2=="temperature")
            {
                sensor2 = "metsense.pr103j2.temperature"
            }
            if(pollutantsel2=="light")
            {
                sensor2 = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel2=="humidity")
            {
                sensor2 = "metsense.htu21d.humidity"
            }
            if(pollutantsel2=="reducing" || pollutantsel2=="oxidizing")
            {
                sensor2 = paste0("chemsense.",pollutantsel2,"_gases.concentration")
            }
            if(pollutantsel2=="so2" || pollutantsel2=="o3" || pollutantsel2=="no2" || pollutantsel2=="h2s" || pollutantsel2=="co")
            {
                sensor2 = paste0("chemsense.",pollutantsel2,".concentration")
            } 

        	node1Label = paste0(dframe$node[s[length(s)]], " - ", dframe$pollutant[s[length(s)]])
        	node2Label = paste0(dframe$node[s[length(s)-1]], " - ", dframe$pollutant[s[length(s)-1]])
			statsDaily1 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor, size = 10000))
			statsDaily2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)-1]]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensor2, size = 10000))
			statsDaily1 <- conversion(statsDaily1,sensor)
      statsDaily2 <- conversion(statsDaily2,sensor2)
			dailySummary <- data.frame(node = c(node1Label, node1Label, node1Label, node2Label, node2Label, node2Label), 
                           stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
                           value= c(min(statsDaily1$value), mean(statsDaily1$value), max(statsDaily1$value), min(statsDaily2$value), mean(statsDaily2$value), max(statsDaily2$value)))

			ggplot(dailySummary, aes(y=value, x=node, color=node, fill=node)) + 
			geom_bar( stat="identity") +
			facet_wrap(~stat)

		}

})


output$weeklytableline <- renderPlot({

		dframe = dataframefortable()
    	s = input$table_rows_selected
		if (length(s)) 
		{
			plot = ggplot() + scale_x_datetime(date_labels = "%Y-%m-%d" , date_breaks = "1 day")+
			labs(x = NULL, y = "Values", title = NULL)+ 
			theme_light(18)+
			theme(legend.title = element_text(color = "white"), panel.grid = element_blank(),legend.text=element_text(size=12),legend.key = element_rect(fill = "grey30"),panel.background = element_rect(fill = 'grey30'))+
	        guides(color=guide_legend(keywidth = 3, keyheight = 2))

	        node1Label = paste0(dframe$node[s[length(s)]], " - ", dframe$pollutant[s[length(s)]])
	        
	        pollutantsel = toString(dframe$pollutant[s[length(s)]])
            pollutantsel = tolower(pollutantsel)
            if(pollutantsel=="temperature")
            {
                sensor = "metsense.pr103j2.temperature"
            }
            if(pollutantsel=="light")
            {
                sensor = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel=="humidity")
            {
                sensor = "metsense.htu21d.humidity"
            }
            if(pollutantsel=="reducing" || pollutantsel=="oxidizing")
            {
                sensor = paste0("chemsense.",pollutantsel,"_gases.concentration")
            }
            if(pollutantsel=="so2" || pollutantsel=="o3" || pollutantsel=="no2" || pollutantsel=="h2s" || pollutantsel=="co")
            {
                sensor = paste0("chemsense.",pollutantsel,".concentration") 
            }

			statsWeekly1 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor, size = 99999))
			statsWeekly1 <- conversion(statsWeekly1,sensor)
      statsWeekly1$time <- as.POSIXct(strptime(statsWeekly1$timestamp, "%Y-%m-%dT%H"), tz = "GMT")
			statsWeekly1Aggregate <- aggregate(value~time,statsWeekly1,mean)
			finalWeekly1 <- left_join(all_times_weekly, statsWeekly1Aggregate, by = c("time"))
			plot = plot + geom_line(aes(x=finalWeekly1$time, y=finalWeekly1$value, colour = node1Label))

			if(length(s) > 1)
			{

				node2Label = paste0(dframe$node[s[length(s)-1]], " - ", dframe$pollutant[s[length(s)-1]])
			
				pollutantsel2 = toString(dframe$pollutant[s[length(s)-1]])
                pollutantsel2 = tolower(pollutantsel2)
                if(pollutantsel2=="temperature")
                {
                    sensor2 = "metsense.pr103j2.temperature"
                }
                if(pollutantsel2=="light")
                {
                    sensor2 = "lightsense.tsl250rd.intensity"
                }
                if(pollutantsel2=="humidity")
                {
                    sensor2 = "metsense.htu21d.humidity"
                }
                if(pollutantsel2=="reducing" || pollutantsel2=="oxidizing")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,"_gases.concentration")
                }
                if(pollutantsel2=="so2" || pollutantsel2=="o3" || pollutantsel2=="no2" || pollutantsel2=="h2s" || pollutantsel2=="co")
                {
                    sensor2 = paste0("chemsense.",pollutantsel2,".concentration")
                } 

				statsWeekly2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s) - 1]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor2, size = 99999))
        statsWeekly2 <- conversion(statsWeekly2,sensor2)
        statsWeekly2$time <- as.POSIXct(strptime(statsWeekly2$timestamp, "%Y-%m-%dT%H"), tz = "GMT")
				statsWeekly2Aggregate <- aggregate(value~time,statsWeekly2,mean)
				finalWeekly2 <- left_join(all_times_weekly, statsWeekly2Aggregate, by = c("time"))
				plot = plot + geom_line(aes(x=finalWeekly2$time, y=finalWeekly2$value, colour = node2Label))

			}

			plot

		}

	

})

output$weeklytablebar <- renderPlot({

		dframe = dataframefortable()

    	s = input$table_rows_selected
		if (length(s) > 1) 
		{
			node1Label = paste0(dframe$node[s[length(s)]], " - ", dframe$pollutant[s[length(s)]])
	        	
	        pollutantsel = toString(dframe$pollutant[s[length(s)]])
            pollutantsel = tolower(pollutantsel)
            if(pollutantsel=="temperature")
            {
                sensor = "metsense.pr103j2.temperature"
            }
            if(pollutantsel=="light")
            {
                sensor = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel=="humidity")
            {
                sensor = "metsense.htu21d.humidity"
            }
            if(pollutantsel=="reducing" || pollutantsel=="oxidizing")
            {
                sensor = paste0("chemsense.",pollutantsel,"_gases.concentration")
            }
            if(pollutantsel=="so2" || pollutantsel=="o3" || pollutantsel=="no2" || pollutantsel=="h2s" || pollutantsel=="co")
            {
                sensor = paste0("chemsense.",pollutantsel,".concentration") 
            }
            
            pollutantsel2 = toString(dframe$pollutant[s[length(s)-1]])
            pollutantsel2 = tolower(pollutantsel2)
            if(pollutantsel2=="temperature")
            {
                sensor2 = "metsense.pr103j2.temperature"
            }
            if(pollutantsel2=="light")
            {
                sensor2 = "lightsense.tsl250rd.intensity"
            }
            if(pollutantsel2=="humidity")
            {
                sensor2 = "metsense.htu21d.humidity"
            }
            if(pollutantsel2=="reducing" || pollutantsel2=="oxidizing")
            {
                sensor2 = paste0("chemsense.",pollutantsel2,"_gases.concentration")
            }
            if(pollutantsel2=="so2" || pollutantsel2=="o3" || pollutantsel2=="no2" || pollutantsel2=="h2s" || pollutantsel2=="co")
            {
                sensor2 = paste0("chemsense.",pollutantsel2,".concentration")
            } 	
			
			node2Label = paste0(dframe$node[s[length(s)-1]], " - ", dframe$pollutant[s[length(s)-1]])
			

			statsWeekly1 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s)]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor, size = 99999))
			statsWeekly2 <- ls.observations(filters = list(node = toString(dframe$node[s[length(s) - 1]]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensor2, size = 99999))	
			
      statsWeekly1 <- conversion(statsWeekly1,sensor) 
      statsWeekly2 <- conversion(statsWeekly2,sensor2)

			weeklySummary <- data.frame(node = c(node1Label, node1Label, node1Label, node2Label, node2Label, node2Label), 
                            stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
                            value= c(min(statsWeekly1$value), mean(statsWeekly1$value), max(statsWeekly1$value), min(statsWeekly2$value), mean(statsWeekly2$value), max(statsWeekly2$value)))

			ggplot(weeklySummary, aes(y=value, x=node, color=node, fill=node)) + 
			geom_bar( stat="identity") +
			facet_wrap(~stat)

		}

})

output$heatmapselect = renderUI({
radioButtons("heatmapselect", label = "",choices = list("AOT" = "aot", "Darksky" = "darksky", "OpenAq" = "openaq"), 
    selected = "aot",inline=TRUE)
})

output$heatmapsensorradio = renderUI({

	if(input$heatmapselect == "aot")
	{
		radioButtons("heatmapfilter",label = "", choices = list("Temperature" = "metsense.pr103j2.temperature", "Light Intensity" = "lightsense.tsl250rd.intensity","Humidity" = "metsense.htu21d.humidity","Pollutant" = "pollutant"), 
    	selected = "lightsense.tsl250rd.intensity",inline=TRUE)
	}	
	else if(input$heatmapselect == "darksky")
	{
		radioButtons("heatmapfilter",label = "", choices = list("Temperature" = "temperature","Humidity"="humidity","Wind Speed"="windSpeed","Wind Bearing"="windBearing","Cloud Cover"="cloudCover","Visibility"="visibility","Pressure"="pressure","Ozone"="ozone"), 
    	selected = "temperature",inline=TRUE)	
	}
	else
	{
		radioButtons("heatmapfilter",label = "", choices = list("PM2.5" = "pm25","PM10"="pm10","CO"="co","SO2"="so2","BC"="bc","NO2"="no2","O3"="o3"), 
    	selected = "pm25",inline=TRUE)
	}

})

output$darkskytableradiobuttons = renderUI({

	radioButtons("darkskytableradio",label = "", choices = list("Temperature" = "temperature","Humidity"="humidity","Wind Speed"="windSpeed","Wind Bearing"="windBearing","Cloud Cover"="cloudCover","Visibility"="visibility","Pressure"="pressure","Ozone"="ozone"), 
    	selected = "temperature",inline=TRUE)

})

output$whenradio = renderUI({
	radioButtons("whenfilter",label = "", choices = list("Now" = "current", "Daily" = "lastday","Weekly" = "lastweek"), 
    	selected = "current",inline=TRUE)
})

output$statsradio = renderUI({
	radioButtons("statsfilter",label = "", choices = list("Mean" = "mean", "Min" = "min","Max" = "max"), 
    	selected = "mean",inline=TRUE)
})

output$heatmappollutantradio = renderUI({
    
   if(input$heatmapfilter == "pollutant")
   {
	   
    	radioButtons("heatmappollutantfilter", "Select Pollutant", choices = list("CO" = "chemsense.co.concentration","NO2" ="chemsense.no2.concentration" , "SO2" ="chemsense.so2.concentration" , "H2S" ="chemsense.h2s.concentration" , "O3" = "chemsense.o3.concentration","REDUCING" = "chemsense.reducing_gases.concentration","OXIDIZING"="chemsense.oxidizing_gases.concentration") , 
    	selected = "chemsense.o3.concentration" ,inline=TRUE)
	}
})

output$heatmapAot <- renderLeaflet({

	api = input$heatmapselect
  nodes <- nodes()

if(api == "aot"){
  
  when <- input$whenfilter

  if(input$heatmapfilter=="pollutant")
  {
      aot_sensor <- input$heatmappollutantfilter
  }
  else
  {
      aot_sensor <- input$heatmapfilter
  }
  stat <- input$statsfilter
  
  if(when == "current"){
    
    # Reading Observations
    obs <- ls.observations(filters=list(size="1000", project = "chicago", sensor = aot_sensor))
    obs <- distinct(obs, node_vsn, .keep_all = TRUE)
    lat <- obs$location.geometry["coordinates"]
    cord = lat %>% separate(coordinates, c('longitude', 'latitude'), sep=",")
    obs$longitude = gsub("[(c]", "", cord$longitude)
    obs$latitude = gsub("[)]", "", cord$latitude)
    
    # Combining both nodes and observations
    obs <- left_join(obs, nodes, by = c("node_vsn"))
    data <- data.frame(node = obs$node_vsn, value = obs$value, unit = obs$uom,
                       location = obs$address, lat = obs$latitude, lng = obs$longitude)
    data$lat <- as.numeric(paste(data$lat))
    data$lng <- as.numeric(paste(data$lng))
    
  }
  if(when == "lastday"){
    
    aot_lastday <- data.frame()
    aot_lastday_data <- ls.observations(filters = list(order = "desc:timestamp",  timestamp = time_end_day, sensor = aot_sensor, size = 99999))
    
    if(stat == "min"){
      aot_lastday_data_stat <- aggregate(x=aot_lastday_data$value,  by=list(node_vsn = aot_lastday_data$node_vsn), FUN=min)
    }
    if(stat == "mean"){
      aot_lastday_data_stat <- aggregate(x=aot_lastday_data$value,  by=list(node_vsn = aot_lastday_data$node_vsn), FUN=mean)
    }
    if(stat == "max"){
      aot_lastday_data_stat <- aggregate(x=aot_lastday_data$value,  by=list(node_vsn = aot_lastday_data$node_vsn), FUN=max)
    }
    
    aot_lastday_data_stat <- left_join(aot_lastday_data_stat, distinct(aot_lastday_data, node_vsn, .keep_all = TRUE), by = "node_vsn")
    aot_lastday_data_stat[,c("value")] <- list(NULL)
    colnames(aot_lastday_data_stat)[colnames(aot_lastday_data_stat)=="x"] <- "value"
    
    lat <- aot_lastday_data_stat$location.geometry["coordinates"]
    cord <- lat %>% separate(coordinates, c('longitude', 'latitude'), sep=",")
    aot_lastday_data_stat$longitude = gsub("[(c]", "", cord$longitude)
    aot_lastday_data_stat$latitude = gsub("[)]", "", cord$latitude)
    
    # Combining both nodes and observations
    obs <- left_join(aot_lastday_data_stat, nodes, by = c("node_vsn"))
    data <- data.frame(node = obs$node_vsn, value = obs$value, unit = obs$uom,
                       location = obs$address, lat = obs$latitude, lng = obs$longitude)
    data$lat <- as.numeric(paste(data$lat))
    data$lng <- as.numeric(paste(data$lng))
    
    
    
  }

  if(when == "lastweek"){
  
  timestamp = time_end_week
  if(stat=="mean")
  {
  	stat = "avg"
  }
  timebucket = paste0(stat, ":1%20day")

  weekly_heatmap_data = data.frame()
  for(i in c(1:nrow(nodes))){
    node = toString(nodes$node_vsn[i])
  
    callString <- paste0("https://api.arrayofthings.org/api/observations?node=", node,"&sensor=", aot_sensor, "&timestamp=", timestamp, "&time_bucket=", timebucket)
    weekly_timebucket_data <- fromJSON(file = callString)
    
    if(length(weekly_timebucket_data$data) != 0){
      weekly_timebucket_singlenode = data.frame()
      
      for(j in c(1:length(weekly_timebucket_data$data))){
        weekly_timebucket_singlenode <- rbind(weekly_timebucket_singlenode, data.frame(timestamp = weekly_timebucket_data[["data"]][[j]][["bucket"]], value = weekly_timebucket_data[["data"]][[j]][["value"]]))
      }
      if(stat == "mean" || stat == "avg")
        weekly_heatmap_data <- rbind(weekly_heatmap_data, data.frame(node_vsn = toString(nodes$node_vsn[i]),lng = nodes$Longitude[i], lat = nodes$Latitude[i], location = nodes$address[i], value = mean(weekly_timebucket_singlenode$value)))
      if(stat == "max")
        weekly_heatmap_data <- rbind(weekly_heatmap_data, data.frame(node_vsn = toString(nodes$node_vsn[i]),lng = nodes$Longitude[i], lat = nodes$Latitude[i], location = nodes$address[i], value = max(weekly_timebucket_singlenode$value)))
      if(stat == "min")
        weekly_heatmap_data <- rbind(weekly_heatmap_data, data.frame(node_vsn = toString(nodes$node_vsn[i]),lng = nodes$Longitude[i], lat = nodes$Latitude[i], location = nodes$address[i], value = min(weekly_timebucket_singlenode$value)))
    }
  }
  data = weekly_heatmap_data
}
  
  
  
  
}



if(api == "darksky"){
  
  # Parameters
  when <- input$whenfilter
  dark_sensor <- input$heatmapfilter
  stat <- input$statsfilter
  
  
  # Map variables
  all_active <- ls.observations(filters=list(order = "desc:timestamp", size = 2000))
  all_active <- distinct(all_active, node_vsn, .keep_all = TRUE)
  all_active_lat <- all_active$location.geometry["coordinates"]
  all_active_cord = all_active_lat %>% separate(coordinates, c('lng', 'lat'), sep=",")
  all_active_cord <- data.frame(lng = gsub("[(c]", "", all_active_cord$lng), lat = gsub("[)]", "", all_active_cord$lat), stringsAsFactors=FALSE)
  all_active_cord$lat <- as.numeric(paste(all_active_cord$lat))
  all_active_cord$lng <- as.numeric(paste(all_active_cord$lng))
  
  
  # Weekly data to load. Takes around 25 seconds
  all_active_cord_weekly <- data.frame()
  for (i in c(0,1,2,3,4,5,6)) {
    
    current_time <- Sys.time()
    current_time = `attr<-`(current_time,"tzone","GMT")
    
    all_active_cord$timestamp <-  paste0(format(as.Date(current_time - 86400*i), "%Y-%m-%d"), "T", format(current_time - 86400*0, format = "%H:%M:%S"), "-0600")
    all_active_cord_weekly <- rbind(all_active_cord_weekly, all_active_cord)
  }
  
  darkskyweeklyfuture <- function(all_active_cord_weekly) {
    all_active_list_weekly <- pmap(list(all_active_cord_weekly$lat, all_active_cord_weekly$lng, all_active_cord_weekly$timestamp),
                                   get_forecast_for)
    return(all_active_list_weekly)
  }
  
  all_active_list_weekly_future <- future(darkskyweeklyfuture(all_active_cord))
  all_active_list_weekly <- value(all_active_list_weekly_future)
  
  
  
  
  if(when == "current"){
    all_active_cord$timestamp <- current_time_darksky
    all_active_list_current <- pmap(list(all_active_cord$lat, all_active_cord$lng, all_active_cord$timestamp, exclude = "hourly, daily"),
                               get_forecast_for)
    dark_sensor_data <- data.frame(lat = all_active_cord$lat, lng = all_active_cord$lng)
    
    for (i in c(1:nrow(dark_sensor_data))){
      dark_sensor_data$value[i] <- all_active_list_current[[i]][["currently"]][[dark_sensor]]
    }
  }
  
  
  if(when == "lastday"){
  
    all_active_cord$timestamp <- current_time_darksky
    all_active_list_daily <- pmap(list(all_active_cord$lat, all_active_cord$lng, all_active_cord$timestamp),
                                  get_forecast_for)
    
    dark_sensor_data <- data.frame(lat = all_active_cord$lat, lng = all_active_cord$lng)
    
    for (i in c(1:nrow(dark_sensor_data))){
      if(stat == "max")
        dark_sensor_data$value[i] <- max(all_active_list_daily[[i]][["hourly"]][[dark_sensor]])
      if(stat == "mean")
        dark_sensor_data$value[i] <- mean(all_active_list_daily[[i]][["hourly"]][[dark_sensor]])
      if(stat =="min")
        dark_sensor_data$value[i] <- min(all_active_list_daily[[i]][["hourly"]][[dark_sensor]])
    }
  }
  
  
  if(when == "lastweek"){
    
    dark_sensor_weekly <- data.frame(lat = all_active_cord_weekly$lat, lng = all_active_cord_weekly$lng)
    
    for (i in c(1:nrow(dark_sensor_weekly))){
      dark_sensor_weekly$max[i] <- max(all_active_list_weekly[[i]][["hourly"]][[dark_sensor]])
      dark_sensor_weekly$mean[i] <- mean(all_active_list_weekly[[i]][["hourly"]][[dark_sensor]])
      dark_sensor_weekly$min[i] <- min(all_active_list_weekly[[i]][["hourly"]][[dark_sensor]])
    }
    
    if(stat == "max")
      dark_sensor_data <- aggregate(dark_sensor_weekly$max,  by=list(lng = dark_sensor_weekly$lng, lat = dark_sensor_weekly$lat), FUN=max)
    if(stat == "mean")
      dark_sensor_data <- aggregate(dark_sensor_weekly$mean,  by=list(lng = dark_sensor_weekly$lng, lat = dark_sensor_weekly$lat), FUN=mean)
    if(stat == "min")
      dark_sensor_data <- aggregate(dark_sensor_weekly$min,  by=list(lng = dark_sensor_weekly$lng, lat = dark_sensor_weekly$lat), FUN=min)
    
    colnames(dark_sensor_data)[colnames(dark_sensor_data)=="x"] <- "value"
  }
  
  
  data = dark_sensor_data
}


if(api == "openaq")
{
	para = input$heatmapfilter
	when = input$whenfilter
	stat = input$statsfilter

	if(when == "current"){
  data <- aq_latest(country="US", city="Chicago-Naperville-Joliet", parameter = para)
  colnames(data)[colnames(data)=="latitude"] <- "lat"
  colnames(data)[colnames(data)=="longitude"] <- "lng"
}
if(when == "lastday"){
  data <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = para, date_from = date(time_day), date_to = date(current_time))
  colnames(data)[colnames(data)=="latitude"] <- "lat"
  colnames(data)[colnames(data)=="longitude"] <- "lng"
  
  if(stat == "mean")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=mean)
  if(stat == "min")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=min)
  if(stat == "max")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=max)
  
  data <- join(data_agg, data, by = "location", type = "left", match = "first")
  data[,c("value")] <- list(NULL)
  colnames(data)[colnames(data)=="x"] <- "value"
  
}
if(when == "lastweek"){
  data <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = para, date_from = date(time_week), date_to = date(current_time))
  colnames(data)[colnames(data)=="latitude"] <- "lat"
  colnames(data)[colnames(data)=="longitude"] <- "lng"
  
  if(stat == "mean")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=mean)
  if(stat == "min")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=min)
  if(stat == "max")
    data_agg <- aggregate(x=data$value,  by=list(location = data$location), FUN=max)
  
  data <- join(data_agg, data, by = "location", type = "left", match = "first")
  data[,c("value")] <- list(NULL)
  colnames(data)[colnames(data)=="x"] <- "value"
}


}

# Coverting data frame to spatial objects
points <- SpatialPointsDataFrame(coords = data.frame(data$lng, data$lat),
                                 data = data.frame(value = data$value),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
area <- readShapePoly("data/chicago", proj4string = CRS("+proj=longlat +datum=WGS84"))
points@bbox <- area@bbox


# Interpolating points using Chicago boundary
th  <-  as(dirichlet(as.ppp(points)), "SpatialPolygons")
proj4string(th) <- proj4string(points)
th.z     <- over(th, points, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
th.clp   <- raster::intersect(area,th.spdf)


# Plotting map and points
if(input$heatmapfilter=="pollutant")
{
	title = input$heatmappollutantfilter
}
else
{
	title = input$heatmapfilter
}
tmap_mode("view")
map <- tm_shape(th.clp) + 
       tm_polygons(col="value", palette="YlOrRd", title=title, style = "quantile") +
       tm_shape(st_as_sf(data, coords = c("lng", "lat"), crs = 4326)) +
       tm_dots(size=0.1, popup.vars=c("value"))


tmap_leaflet(map)

})


output$wbitleaf <- renderLeaflet({
	  map <- leaflet(data = value(final))
      map <- addTiles(map)
      map <- setView(map, lng = value(final)$Long[5], lat = value(final)$Lat[5], zoom = 10)
      map <- addAwesomeMarkers(map, layerId = value(final)$ID, lng = value(final)$Long, lat = value(final)$Lat, icon = my_icon_1)
      map
})

output$wbittable <- DT::renderDataTable({ DT::datatable(value(final), selection = "single", options = list(stateSave = TRUE))})
        
        
	
	prev_row <- reactiveVal()
 	my_icon = makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white')
 	my_icon_1 = makeAwesomeIcon(markerColor = 'blue')

  	observeEvent(input$wbittable_rows_selected, {
    row_selected = value(final)[input$wbittable_rows_selected,]
    proxy <- leafletProxy('wbitleaf')
        proxy %>%
      addAwesomeMarkers(layerId = row_selected$ID,
                        lng=row_selected$Long, 
                        lat=row_selected$Lat,
                        icon = my_icon)


    if(!is.null(prev_row()))
    {
      proxy %>%
        addAwesomeMarkers(layerId = prev_row()$ID,
                   lng=prev_row()$Long, 
                   lat=prev_row()$Lat,
                   icon = my_icon_1
                   )
    }
    # set new value to reactiveVal 
    prev_row(row_selected)
  })



output$wbitoptions = renderUI({
  checkboxGroupInput("wbitoptions", "Select any of the following:", c("Relative Humidity" = "rh","Solar Radition" = "srad", "Dew Point" = "dpoint", "Cloud Coverage" = "cloud", "Visibility" = "vis", "Precipitation" = "preci", "UV Index" = "uvi"),inline=TRUE, selected = c("rh"))
})

observeEvent(input$wbitleaf_marker_click, {
      clickId <- input$wbitleaf_marker_click$id
      dataTableProxy("wbittable") %>%
      selectRows(which(value(final)$ID == clickId)) %>%
      selectPage(which(input$wbittable_rows_all == clickId) %/% input$wbittable_state$length + 1)
  })

output$wbitline <- renderPlot({
		Click_selected <- value(final)[input$wbittable_rows_selected,]
		#Click_data <- subset(final, ID == Click_selected$ID, select = c(Lat, Long))
		Click_call <- paste0("https://api.weatherbit.io/v2.0/history/hourly?lat=",Click_selected$Lat,"&lon=",Click_selected$Long,"&start_date=",P_time,"&end_date=",C_time,"&key=",API_KEY)
		Click_result <- fromJSON(file = Click_call)
		hours <- c(1:24)

		Click_ggplot <- ggplot() +
           labs(x = NULL, y = NULL, title = NULL) + 
           theme_light(18) +
           theme(legend.title = element_text(color = "white"), panel.grid = element_blank(),legend.text=element_text(size=12),legend.key = element_rect(fill = "grey30"),panel.background = element_rect(fill = 'grey30'))+
           guides(color=guide_legend(keywidth = 3, keyheight = 2))


		#Click_Final = data.frame()
        if("rh" %in% input$wbitoptions)
        {	
        	RH <- c()
        	for (i in 1:24)
        		{
        			RH[i] <- Click_result[["data"]][[i]][["rh"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x= hours, y=RH, color = "Realative Humidity"))
        }

        if("srad" %in% input$wbitoptions)
        {	
        	SRAD <- c()
        	for (i in 1:24)
        		{
        			SRAD[i] <- Click_result[["data"]][[i]][["solar_rad"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x= hours, y=SRAD, color = "Solar Radition"))
        }


        if("dpoint" %in% input$wbitoptions)
        {	
        	DPoint <- c()
        	for (i in 1:24)
        		{
        			DPoint[i] <- Click_result[["data"]][[i]][["dewpt"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x= hours, y=DPoint, color = "Dew Point"))
        }

        if("cloud" %in% input$wbitoptions)
        {	
        	CLOUD <- c()
        	for (i in 1:24)
        		{
        			CLOUD[i] <- Click_result[["data"]][[i]][["clouds"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x=hours, y=CLOUD, color = "Cloud Coverage"))
        }

        if("vis" %in% input$wbitoptions)
        {	
        	VIS <- c()
        	for (i in 1:24)
        		{
        			VIS[i] <- Click_result[["data"]][[i]][["vis"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x=hours, y=VIS, color = "Visibility"))
        }

        if("preci" %in% input$wbitoptions)
        {	
        	PRECI <- c()
        	for (i in 1:24)
        		{
        			PRECI[i] <- Click_result[["data"]][[i]][["precip"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x=hours, y=PRECI, color = "Precipitation"))
        }

        if("uvi" %in% input$wbitoptions)
        {	
        	UVI <- c()
        	for (i in 1:24)
        		{
        			UVI[i] <- Click_result[["data"]][[i]][["uv"]]
        		}
        	Click_ggplot <- Click_ggplot + geom_line(aes(x=hours, y=UVI, color = "UV Index"))
        }

        Click_ggplot
        })

  output$wbidaily <- DT::renderDataTable(
        DT::datatable({ 
		Click_selected <- value(final)[input$wbittable_rows_selected,]
		#Click_data <- subset(final, ID == Click_selected$ID, select = c(Lat, Long))
		Click_call <- paste0("https://api.weatherbit.io/v2.0/history/hourly?lat=",Click_selected$Lat,"&lon=",Click_selected$Long,"&start_date=",P_time,"&end_date=",C_time,"&key=",API_KEY)
		Click_result <- fromJSON(file = Click_call)
		hours <- c(1:24)

		Click_table <- data.frame()
		#Click_Final = data.frame()
        
        if("rh" %in% input$wbitoptions)
        {	
        	RH <- c()
        	for (i in 1:24)
        		{
        			RH[i] <- Click_result[["data"]][[i]][["rh"]]
        		}

			RH_Table = data.frame("Selected Option" = "Relative Humidity", "Min" = min(RH), "Average" = mean(RH), "Max" = max(RH))
			Click_table <- rbind(Click_table, RH_Table)
        	}

        if("srad" %in% input$wbitoptions)
        {	
        	SRAD <- c()
        	for (i in 1:24)
        		{
        			SRAD[i] <- Click_result[["data"]][[i]][["solar_rad"]]
        
       		}
       		SRAD_Table = data.frame("Selected Option" = "Solar Radiance", "Min" = min(SRAD), "Average" = mean(SRAD), "Max" = max(SRAD))
			Click_table <- rbind(Click_table, SRAD_Table)
        }


        if("dpoint" %in% input$wbitoptions)
        {	
        	DPoint <- c()
        	for (i in 1:24)
        		{
        			DPoint[i] <- Click_result[["data"]][[i]][["dewpt"]]
        		}

        	DPoint_Table = data.frame("Selected Option" = "Dew Point", "Min" = min(DPoint), "Average" = mean(DPoint), "Max" = max(DPoint))
			Click_table <- rbind(Click_table, DPoint_Table)
        	}

        if("cloud" %in% input$wbitoptions)
        {	
        	CLOUD <- c()
        	for (i in 1:24)
        		{
        			CLOUD[i] <- Click_result[["data"]][[i]][["clouds"]]
        		}

        	Cloud_Table = data.frame("Selected Option" = "Cloud Coverage", "Min" = min(CLOUD), "Average" = mean(CLOUD), "Max" = max(CLOUD))
			Click_table <- rbind(Click_table, Cloud_Table)
        }

        if("vis" %in% input$wbitoptions)
        {	
        	VIS <- c()
        	for (i in 1:24)
        		{
        			VIS[i] <- Click_result[["data"]][[i]][["vis"]]
        		}


        	Vis_Table = data.frame("Selected Option" = "Visibility", "Min" = min(VIS), "Average" = mean(VIS), "Max" = max(VIS))
			Click_table <- rbind(Click_table, Vis_Table)
        	}

        if("preci" %in% input$wbitoptions)
        {	
        	PRECI <- c()
        	for (i in 1:24)
        		{
        			PRECI[i] <- Click_result[["data"]][[i]][["precip"]]
        		}


        	Preci_Table = data.frame("Selected Option" = "Precipitation", "Min" = min(PRECI), "Average" = mean(PRECI), "Max" = max(PRECI))
			Click_table <- rbind(Click_table, Preci_Table)
        	}

        if("uvi" %in% input$wbitoptions)
        {	
        	UVI <- c()
        	for (i in 1:24)
        		{
        			UVI[i] <- Click_result[["data"]][[i]][["uv"]]
        		}


        	UVI_Table = data.frame("Selected Option" = "UV Index", "Min" = min(UVI), "Average" = mean(UVI), "Max" = max(UVI))
			Click_table <- rbind(Click_table, UVI_Table)
        	}


        	Click_table
        }, selection = "single",
        options = list()
        )
        )
        
        output$wbitmin <- renderPlot({
		Click_selected <- value(final)[input$wbittable_rows_selected,]
		#Click_data <- subset(final, ID == Click_selected$ID, select = c(Lat, Long))
		Click_call <- paste0("https://api.weatherbit.io/v2.0/history/hourly?lat=",Click_selected$Lat,"&lon=",Click_selected$Long,"&start_date=",P_time,"&end_date=",C_time,"&key=",API_KEY)
		Click_result <- fromJSON(file = Click_call)
		hours <- c(1:24)

		Click_table <- data.frame()
		#Click_Final = data.frame()
        
        if("rh" %in% input$wbitoptions)
        {	
        	RH <- c()
        	for (i in 1:24)
        		{
        			RH[i] <- Click_result[["data"]][[i]][["rh"]]
        		}

			RH_Table = data.frame("Selected Option" = "Relative Humidity", "Min" = min(RH), "Average" = mean(RH), "Max" = max(RH))
			Click_table <- rbind(Click_table, RH_Table)
        	}

        if("srad" %in% input$wbitoptions)
        {	
        	SRAD <- c()
        	for (i in 1:24)
        		{
        			SRAD[i] <- Click_result[["data"]][[i]][["solar_rad"]]
        
       		}
       		SRAD_Table = data.frame("Selected Option" = "Solar Radiance", "Min" = min(SRAD), "Average" = mean(SRAD), "Max" = max(SRAD))
			Click_table <- rbind(Click_table, SRAD_Table)
        }


        if("dpoint" %in% input$wbitoptions)
        {	
        	DPoint <- c()
        	for (i in 1:24)
        		{
        			DPoint[i] <- Click_result[["data"]][[i]][["dewpt"]]
        		}

        	DPoint_Table = data.frame("Selected Option" = "Dew Point", "Min" = min(DPoint), "Average" = mean(DPoint), "Max" = max(DPoint))
			Click_table <- rbind(Click_table, DPoint_Table)
        	}

        if("cloud" %in% input$wbitoptions)
        {	
        	CLOUD <- c()
        	for (i in 1:24)
        		{
        			CLOUD[i] <- Click_result[["data"]][[i]][["clouds"]]
        		}

        	Cloud_Table = data.frame("Selected Option" = "Cloud Coverage", "Min" = min(CLOUD), "Average" = mean(CLOUD), "Max" = max(CLOUD))
			Click_table <- rbind(Click_table, Cloud_Table)
        }

        if("vis" %in% input$wbitoptions)
        {	
        	VIS <- c()
        	for (i in 1:24)
        		{
        			VIS[i] <- Click_result[["data"]][[i]][["vis"]]
        		}


        	Vis_Table = data.frame("Selected Option" = "Visibility", "Min" = min(VIS), "Average" = mean(VIS), "Max" = max(VIS))
			Click_table <- rbind(Click_table, Vis_Table)
        	}

        if("preci" %in% input$wbitoptions)
        {	
        	PRECI <- c()
        	for (i in 1:24)
        		{
        			PRECI[i] <- Click_result[["data"]][[i]][["precip"]]
        		}


        	Preci_Table = data.frame("Selected Option" = "Precipitation", "Min" = min(PRECI), "Average" = mean(PRECI), "Max" = max(PRECI))
			Click_table <- rbind(Click_table, Preci_Table)
        	}

        if("uvi" %in% input$wbitoptions)
        {	
        	UVI <- c()
        	for (i in 1:24)
        		{
        			UVI[i] <- Click_result[["data"]][[i]][["uv"]]
        		}


        	UVI_Table = data.frame("Selected Option" = "UV Index", "Min" = min(UVI), "Average" = mean(UVI), "Max" = max(UVI))
			Click_table <- rbind(Click_table, UVI_Table)
        	}


        	ggplot(data=Click_table, aes(x=Selected.Option, y=Min, fill = Selected.Option))+geom_bar(stat = "identity")


        	})

        output$wbiavg <- renderPlot({
		Click_selected <- value(final)[input$wbittable_rows_selected,]
		#Click_data <- subset(final, ID == Click_selected$ID, select = c(Lat, Long))
		Click_call <- paste0("https://api.weatherbit.io/v2.0/history/hourly?lat=",Click_selected$Lat,"&lon=",Click_selected$Long,"&start_date=",P_time,"&end_date=",C_time,"&key=",API_KEY)
		Click_result <- fromJSON(file = Click_call)
		hours <- c(1:24)

		Click_table <- data.frame()
		#Click_Final = data.frame()
        
        if("rh" %in% input$wbitoptions)
        {	
        	RH <- c()
        	for (i in 1:24)
        		{
        			RH[i] <- Click_result[["data"]][[i]][["rh"]]
        		}

			RH_Table = data.frame("Selected Option" = "Relative Humidity", "Min" = min(RH), "Average" = mean(RH), "Max" = max(RH))
			Click_table <- rbind(Click_table, RH_Table)
        	}

        if("srad" %in% input$wbitoptions)
        {	
        	SRAD <- c()
        	for (i in 1:24)
        		{
        			SRAD[i] <- Click_result[["data"]][[i]][["solar_rad"]]
        
       		}
       		SRAD_Table = data.frame("Selected Option" = "Solar Radiance", "Min" = min(SRAD), "Average" = mean(SRAD), "Max" = max(SRAD))
			Click_table <- rbind(Click_table, SRAD_Table)
        }


        if("dpoint" %in% input$wbitoptions)
        {	
        	DPoint <- c()
        	for (i in 1:24)
        		{
        			DPoint[i] <- Click_result[["data"]][[i]][["dewpt"]]
        		}

        	DPoint_Table = data.frame("Selected Option" = "Dew Point", "Min" = min(DPoint), "Average" = mean(DPoint), "Max" = max(DPoint))
			Click_table <- rbind(Click_table, DPoint_Table)
        	}

        if("cloud" %in% input$wbitoptions)
        {	
        	CLOUD <- c()
        	for (i in 1:24)
        		{
        			CLOUD[i] <- Click_result[["data"]][[i]][["clouds"]]
        		}

        	Cloud_Table = data.frame("Selected Option" = "Cloud Coverage", "Min" = min(CLOUD), "Average" = mean(CLOUD), "Max" = max(CLOUD))
			Click_table <- rbind(Click_table, Cloud_Table)
        }

        if("vis" %in% input$wbitoptions)
        {	
        	VIS <- c()
        	for (i in 1:24)
        		{
        			VIS[i] <- Click_result[["data"]][[i]][["vis"]]
        		}


        	Vis_Table = data.frame("Selected Option" = "Visibility", "Min" = min(VIS), "Average" = mean(VIS), "Max" = max(VIS))
			Click_table <- rbind(Click_table, Vis_Table)
        	}

        if("preci" %in% input$wbitoptions)
        {	
        	PRECI <- c()
        	for (i in 1:24)
        		{
        			PRECI[i] <- Click_result[["data"]][[i]][["precip"]]
        		}


        	Preci_Table = data.frame("Selected Option" = "Precipitation", "Min" = min(PRECI), "Average" = mean(PRECI), "Max" = max(PRECI))
			Click_table <- rbind(Click_table, Preci_Table)
        	}

        if("uvi" %in% input$wbitoptions)
        {	
        	UVI <- c()
        	for (i in 1:24)
        		{
        			UVI[i] <- Click_result[["data"]][[i]][["uv"]]
        		}


        	UVI_Table = data.frame("Selected Option" = "UV Index", "Min" = min(UVI), "Average" = mean(UVI), "Max" = max(UVI))
			Click_table <- rbind(Click_table, UVI_Table)
        	}


        	ggplot(data=Click_table, aes(x=Selected.Option, y=Average, fill = Selected.Option))+geom_bar(stat = "identity")


        	})

        output$wbimax <- renderPlot({
		Click_selected <- value(final)[input$wbittable_rows_selected,]
		#Click_data <- subset(final, ID == Click_selected$ID, select = c(Lat, Long))
		Click_call <- paste0("https://api.weatherbit.io/v2.0/history/hourly?lat=",Click_selected$Lat,"&lon=",Click_selected$Long,"&start_date=",P_time,"&end_date=",C_time,"&key=",API_KEY)
		Click_result <- fromJSON(file = Click_call)
		hours <- c(1:24)

		Click_table <- data.frame()
		#Click_Final = data.frame()
        
        if("rh" %in% input$wbitoptions)
        {	
        	RH <- c()
        	for (i in 1:24)
        		{
        			RH[i] <- Click_result[["data"]][[i]][["rh"]]
        		}

			RH_Table = data.frame("Selected Option" = "Relative Humidity", "Min" = min(RH), "Average" = mean(RH), "Max" = max(RH))
			Click_table <- rbind(Click_table, RH_Table)
        	}

        if("srad" %in% input$wbitoptions)
        {	
        	SRAD <- c()
        	for (i in 1:24)
        		{
        			SRAD[i] <- Click_result[["data"]][[i]][["solar_rad"]]
        
       		}
       		SRAD_Table = data.frame("Selected Option" = "Solar Radiance", "Min" = min(SRAD), "Average" = mean(SRAD), "Max" = max(SRAD))
			Click_table <- rbind(Click_table, SRAD_Table)
        }


        if("dpoint" %in% input$wbitoptions)
        {	
        	DPoint <- c()
        	for (i in 1:24)
        		{
        			DPoint[i] <- Click_result[["data"]][[i]][["dewpt"]]
        		}

        	DPoint_Table = data.frame("Selected Option" = "Dew Point", "Min" = min(DPoint), "Average" = mean(DPoint), "Max" = max(DPoint))
			Click_table <- rbind(Click_table, DPoint_Table)
        	}

        if("cloud" %in% input$wbitoptions)
        {	
        	CLOUD <- c()
        	for (i in 1:24)
        		{
        			CLOUD[i] <- Click_result[["data"]][[i]][["clouds"]]
        		}

        	Cloud_Table = data.frame("Selected Option" = "Cloud Coverage", "Min" = min(CLOUD), "Average" = mean(CLOUD), "Max" = max(CLOUD))
			Click_table <- rbind(Click_table, Cloud_Table)
        }

        if("vis" %in% input$wbitoptions)
        {	
        	VIS <- c()
        	for (i in 1:24)
        		{
        			VIS[i] <- Click_result[["data"]][[i]][["vis"]]
        		}


        	Vis_Table = data.frame("Selected Option" = "Visibility", "Min" = min(VIS), "Average" = mean(VIS), "Max" = max(VIS))
			Click_table <- rbind(Click_table, Vis_Table)
        	}

        if("preci" %in% input$wbitoptions)
        {	
        	PRECI <- c()
        	for (i in 1:24)
        		{
        			PRECI[i] <- Click_result[["data"]][[i]][["precip"]]
        		}


        	Preci_Table = data.frame("Selected Option" = "Precipitation", "Min" = min(PRECI), "Average" = mean(PRECI), "Max" = max(PRECI))
			Click_table <- rbind(Click_table, Preci_Table)
        	}

        if("uvi" %in% input$wbitoptions)
        {	
        	UVI <- c()
        	for (i in 1:24)
        		{
        			UVI[i] <- Click_result[["data"]][[i]][["uv"]]
        		}


        	UVI_Table = data.frame("Selected Option" = "UV Index", "Min" = min(UVI), "Average" = mean(UVI), "Max" = max(UVI))
			Click_table <- rbind(Click_table, UVI_Table)
        	}


        	ggplot(data=Click_table, aes(x=Selected.Option, y=Max, fill = Selected.Option))+geom_bar(stat = "identity")


        	})

output$dailyleafletline <- renderPlot({

		sensorVec <- c()
		pollutantsVec <- c()
		finalFrame = data.frame(matrix(ncol=6, nrow=0))
		colnames(finalFrame) <- c("time", "value", "nodeLabel", "node", "chosenParameter", "linetype")
		if (length(RV$Clicks)) 
		{
			if("temp"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.pr103j2.temperature")
                pollutantsVec <- rbind(pollutantsVec, "TEMPERATURE")
            }
            if("light"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "lightsense.tsl250rd.intensity")
                pollutantsVec <- rbind(pollutantsVec, "LIGHT")
            }
            if("humidity" %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.htu21d.humidity")
                pollutantsVec <- rbind(pollutantsVec, "HUMIDITY")
            }
            if("REDUCING"  %in% input$pollutantscheckbox) 
            {
                sensorVec <- rbind(sensorVec, "chemsense.reducing_gases.concentration")
                pollutantsVec <- rbind(pollutantsVec, "REDUCING")
            }
            if("OXIDIZING"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.oxidizing_gases.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "OXIDIZING")
            }
            if("SO2"  %in% input$pollutantscheckbox)
            {
                sensorVec <- rbind(sensorVec, "chemsense.so2.concentration")
                pollutantsVec <- rbind(pollutantsVec, "SO2")
            }
            if("O3"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.o3.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "O3")
            }
            if("NO2"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.no2.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "NO2")
            }
            if("H2S"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.h2s.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "H2S")
            }
            if("CO"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.co.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "CO")
            }

			plot = ggplot() + scale_x_datetime(date_labels = "%H:%M" , date_breaks = "1 hour") +
			labs(x = NULL, y = "Values", title = NULL)+ 
			theme_light(18)+
			theme(legend.title = element_text(color = "white"), panel.grid = element_blank(),legend.text=element_text(size=12),legend.key = element_rect(fill = "grey30"),panel.background = element_rect(fill = 'grey30'))+
            guides(color=guide_legend(keywidth = 3, keyheight = 2))

	        for(index in 1:length(sensorVec)){
		        node1Label = paste0(RV$Clicks[1], " - ", pollutantsVec[index])

				statsDaily1 <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_day, sensor = toString(sensorVec[index]), size = 10000))
				statsDaily1 <- conversion(statsDaily1,toString(sensorVec[index]))
        statsDaily1$time <- as.POSIXct(strptime(statsDaily1$timestamp, "%Y-%m-%dT%H:%M"), tz = "GMT")
				statsDaily1Aggregate <- aggregate(value~time,statsDaily1,mean)
				finalDaily1 <- left_join(all_times_daily, statsDaily1Aggregate, by = c("time"))
				finalFrame <- rbind(finalFrame, data.frame(time=(finalDaily1$time), value=(finalDaily1$value), nodeLabel=(node1Label), node=(RV$Clicks[1]), chosenParameter=(pollutantsVec[index]), linetype="solid"))
				
				#plot = plot + geom_line(aes(x=finalDaily1$time, y=finalDaily1$value, colour = node1Label))
				if(length(RV$Clicks) > 1)
				{
					node2Label = paste0(RV$Clicks[2], " - ", pollutantsVec[index])
	    
					statsDaily2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_day, sensor = toString(sensorVec[index]), size = 10000))
					statsDaily2 <- conversion(statsDaily2,toString(sensorVec[index]))
          statsDaily2$time <- as.POSIXct(strptime(statsDaily2$timestamp, "%Y-%m-%dT%H:%M"), tz = "GMT")
					statsDaily2Aggregate <- aggregate(value~time,statsDaily2,mean)
					finalDaily2 <- left_join(all_times_daily, statsDaily2Aggregate, by = c("time"))
					finalFrame <- rbind(finalFrame, data.frame(time=(finalDaily2$time), value=(finalDaily2$value), nodeLabel=(node2Label), node=(RV$Clicks[2]), chosenParameter=(pollutantsVec[index]), linetype="dotted"))	
				#	plot = plot + geom_line(aes(x=finalDaily2$time, y=finalDaily2$value, colour = node2Label))

				}
			}
			
			output$vText <- renderText({
				paste(RV$Clicks[1], RV$Clicks[2], unique(finalFrame$node))
				})
			output$finalFrame <- renderDataTable({
				finalFrame
				})
			plot=plot+geom_line(aes(x=finalFrame$time, y=finalFrame$value, linetype=finalFrame$linetype, colour=finalFrame$chosenParameter))
			plot
		}


})

output$dailyleafletbar <- renderPlot({

		
		data = data.frame()
		sensorVec <- c()
		pollutantsVec <- c()
		if (length(RV$Clicks) > 1) 
		{
            if("temp"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.pr103j2.temperature")
                pollutantsVec <- rbind(pollutantsVec, "TEMPERATURE")
            }
            if("light"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "lightsense.tsl250rd.intensity")
                pollutantsVec <- rbind(pollutantsVec, "LIGHT")
            }
            if("humidity" %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.htu21d.humidity")
                pollutantsVec <- rbind(pollutantsVec, "HUMIDITY")
            }
            if("REDUCING"  %in% input$pollutantscheckbox) 
            {
                sensorVec <- rbind(sensorVec, "chemsense.reducing_gases.concentration")
                pollutantsVec <- rbind(pollutantsVec, "REDUCING")
            }
            if("OXIDIZING"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.oxidizing_gases.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "OXIDIZING")
            }
            if("SO2"  %in% input$pollutantscheckbox)
            {
                sensorVec <- rbind(sensorVec, "chemsense.so2.concentration")
                pollutantsVec <- rbind(pollutantsVec, "SO2")
            }
            if("O3"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.o3.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "O3")
            }
            if("NO2"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.no2.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "NO2")
            }
            if("H2S"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.h2s.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "H2S")
            }
            if("CO"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.co.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "CO")
            }
            for(index in 1:length(sensorVec)){

	        	node1Label = paste0(RV$Clicks[1], " - ", pollutantsVec[index])
	        	node2Label = paste0(RV$Clicks[2], " - ", pollutantsVec[index])
				statsDaily1 <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensorVec[index], size = 10000))
				statsDaily1 <- conversion(statsDaily1,sensorVec[index])  
        statsDaily2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_day, sensor = sensorVec[index], size = 10000))
				statsDaily2 <- conversion(statsDaily2,sensorVec[index])
				data <- rbind(data, data.frame(node = c(node1Label, node1Label, node1Label, node2Label, node2Label, node2Label), 
	                           stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
	                           value= c(min(statsDaily1$value), mean(statsDaily1$value), max(statsDaily1$value), min(statsDaily2$value), mean(statsDaily2$value), max(statsDaily2$value))))
			}

			ggplot(data, aes(y=value, x=node, color=node, fill=node)) + 
			geom_bar( stat="identity") +
			facet_wrap(~stat)

		}

})

output$weeklyleafletbar <- renderPlot({

		data <- data.frame()
		pollutantsVec <- c()
		sensorVec <- c()
		
		if (length(RV$Clicks) > 1) 
		{
			if("temp"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.pr103j2.temperature")
                pollutantsVec <- rbind(pollutantsVec, "TEMPERATURE")
            }
            if("light"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "lightsense.tsl250rd.intensity")
                pollutantsVec <- rbind(pollutantsVec, "LIGHT")
            }
            if("humidity" %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.htu21d.humidity")
                pollutantsVec <- rbind(pollutantsVec, "HUMIDITY")
            }
            if("REDUCING"  %in% input$pollutantscheckbox) 
            {
                sensorVec <- rbind(sensorVec, "chemsense.reducing_gases.concentration")
                pollutantsVec <- rbind(pollutantsVec, "REDUCING")
            }
            if("OXIDIZING"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.oxidizing_gases.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "OXIDIZING")
            }
            if("SO2"  %in% input$pollutantscheckbox)
            {
                sensorVec <- rbind(sensorVec, "chemsense.so2.concentration")
                pollutantsVec <- rbind(pollutantsVec, "SO2")
            }
            if("O3"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.o3.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "O3")
            }
            if("NO2"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.no2.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "NO2")
            }
            if("H2S"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.h2s.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "H2S")
            }
            if("CO"  %in% input$pollutantscheckbox)
            {
            	sensorVec <- rbind(sensorVec, "chemsense.co.concentration")
            	pollutantsVec <- rbind(pollutantsVec, "CO")
            }

            for(index in 1:length(sensorVec)){
				node1Label = paste0(RV$Clicks[1], " - ", pollutantsVec[index])
		        	
				node2Label = paste0(RV$Clicks[2], " - ", pollutantsVec[index])
				

				statsWeekly1 <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensorVec[index], size = 99999))
				statsWeekly2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensorVec[index], size = 99999))	
				statsWeekly1 <- conversion(statsWeekly1,sensorVec[index])
        statsWeekly2 <- conversion(statsWeekly2,sensorVec[index])
				data <- rbind(data, data.frame(node = c(node1Label, node1Label, node1Label, node2Label, node2Label, node2Label), 
	                            stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
	                            value= c(min(statsWeekly1$value), mean(statsWeekly1$value), max(statsWeekly1$value), min(statsWeekly2$value), mean(statsWeekly2$value), max(statsWeekly2$value))))

			}
			ggplot(data, aes(y=value, x=node, color=node, fill=node)) + 
			geom_bar( stat="identity") +
			facet_wrap(~stat)

		}

})

output$weeklyleafletline <- renderPlot({

    sensorVec <- c()
    pollutantsVec <- c()
    finalFrame = data.frame(matrix(ncol=6, nrow=0))
    colnames(finalFrame) <- c("time", "value", "nodeLabel", "node", "chosenParameter", "linetype")
    if (length(RV$Clicks)) 
    {
      if("temp"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.pr103j2.temperature")
                pollutantsVec <- rbind(pollutantsVec, "TEMPERATURE")
            }
            if("light"  %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "lightsense.tsl250rd.intensity")
                pollutantsVec <- rbind(pollutantsVec, "LIGHT")
            }
            if("humidity" %in% input$filter)
            {
                sensorVec <- rbind(sensorVec, "metsense.htu21d.humidity")
                pollutantsVec <- rbind(pollutantsVec, "HUMIDITY")
            }
            if("REDUCING"  %in% input$pollutantscheckbox) 
            {
                sensorVec <- rbind(sensorVec, "chemsense.reducing_gases.concentration")
                pollutantsVec <- rbind(pollutantsVec, "REDUCING")
            }
            if("OXIDIZING"  %in% input$pollutantscheckbox)
            {
              sensorVec <- rbind(sensorVec, "chemsense.oxidizing_gases.concentration")
              pollutantsVec <- rbind(pollutantsVec, "OXIDIZING")
            }
            if("SO2"  %in% input$pollutantscheckbox)
            {
                sensorVec <- rbind(sensorVec, "chemsense.so2.concentration")
                pollutantsVec <- rbind(pollutantsVec, "SO2")
            }
            if("O3"  %in% input$pollutantscheckbox)
            {
              sensorVec <- rbind(sensorVec, "chemsense.o3.concentration")
              pollutantsVec <- rbind(pollutantsVec, "O3")
            }
            if("NO2"  %in% input$pollutantscheckbox)
            {
              sensorVec <- rbind(sensorVec, "chemsense.no2.concentration")
              pollutantsVec <- rbind(pollutantsVec, "NO2")
            }
            if("H2S"  %in% input$pollutantscheckbox)
            {
              sensorVec <- rbind(sensorVec, "chemsense.h2s.concentration")
              pollutantsVec <- rbind(pollutantsVec, "H2S")
            }
            if("CO"  %in% input$pollutantscheckbox)
            {
              sensorVec <- rbind(sensorVec, "chemsense.co.concentration")
              pollutantsVec <- rbind(pollutantsVec, "CO")
            }

      plot = ggplot() + scale_x_datetime(date_labels = "%Y-%m-%d" , date_breaks = "1 day") +
      labs(x = NULL, y = "Values", title = NULL)+ 
      theme_light(18)+
      theme(legend.title = element_text(color = "white"), panel.grid = element_blank(),legend.text=element_text(size=12),legend.key = element_rect(fill = "grey30"),panel.background = element_rect(fill = 'grey30'))+
          guides(color=guide_legend(keywidth = 3, keyheight = 2))

          for(index in 1:length(sensorVec)){
            node1Label = paste0(RV$Clicks[1], " - ", pollutantsVec[index])

        statsWeekly1 <- ls.observations(filters = list(node = toString(RV$Clicks[1]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensorVec[index], size = 99999))
        statsWeekly1 <- conversion(statsWeekly1,sensorVec[index])
        statsWeekly1$time <- as.POSIXct(strptime(statsWeekly1$timestamp, "%Y-%m-%dT%H"), tz = "GMT")
        statsWeekly1Aggregate <- aggregate(value~time,statsWeekly1,mean)
        finalWeekly1 <- left_join(all_times_weekly, statsWeekly1Aggregate, by = c("time"))
        finalFrame <- rbind(finalFrame, data.frame(time=(finalWeekly1$time), value=(finalWeekly1$value), nodeLabel=(node1Label), node=(RV$Clicks[1]), chosenParameter=(pollutantsVec[index]), linetype="solid"))
        
        #plot = plot + geom_line(aes(x=finalDaily1$time, y=finalDaily1$value, colour = node1Label))
        if(length(RV$Clicks) > 1)
        {
          node2Label = paste0(RV$Clicks[2], " - ", pollutantsVec[index])
      
          statsWeekly2 <- ls.observations(filters = list(node = toString(RV$Clicks[2]), order = "desc:timestamp",  timestamp = time_end_week, sensor = sensorVec[index], size = 99999))
          statsWeekly2 <- conversion(statsWeekly2,sensorVec[index])
          statsWeekly2$time <- as.POSIXct(strptime(statsWeekly2$timestamp, "%Y-%m-%dT%H"), tz = "GMT")
          statsWeekly2Aggregate <- aggregate(value~time,statsWeekly2,mean)
          finalWeekly2 <- left_join(all_times_weekly, statsWeekly2Aggregate, by = c("time"))
          finalFrame <- rbind(finalFrame, data.frame(time=(finalWeekly2$time), value=(finalWeekly2$value), nodeLabel=(node2Label), node=(RV$Clicks[2]), chosenParameter=(pollutantsVec[index]), linetype="dotted")) 
        # plot = plot + geom_line(aes(x=finalDaily2$time, y=finalDaily2$value, colour = node2Label))

        }
      }
   
      plot=plot+geom_line(aes(x=finalFrame$time, y=finalFrame$value, linetype=finalFrame$linetype, colour=finalFrame$chosenParameter))
      plot
    }


})

output$openaqleaf <- renderLeaflet({

      OpenAqtable <- OpenAqtable()
      if("all" %in% input$alivenodes)
      {
          OpenAqtableLatest <- OpenAqtable
      }

      if("alive" %in% input$alivenodes)
      {

        OpenAqtableLatest <- subset(OpenAqtable, date(lastUpdated) == C_date_openaq)
      }

      final <- data.frame()

      if("O3" %in% input$aqipollutantcheckbox)
      {
        o3 <- subset(OpenAqtableLatest, parameter == "o3", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if ("metric" %in% input$unitconv)
        {
            o3$value <- round(((o3$value * 48) / 24.45), 2)
        } 
        if(nrow(final)==0){
        final=o3
        } 
        else{
        final = rbind(final, o3)
      }
      }

      if("PM25" %in% input$aqipollutantcheckbox)
      {
        pm25 <- subset(OpenAqtableLatest, parameter == "pm25", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if ("imp" %in% input$unitconv)
        {
            pm25$value <- round((pm25_daily$value * 24.45) / 52.66,digits=2)
        } 
        if(nrow(final)==0){
        final=pm25
        } 
        else{
        final = rbind(final, pm25)
      }
      }

      if("CO" %in% input$aqipollutantcheckbox)
      {
        CO <- subset(OpenAqtableLatest, parameter == "co", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if(nrow(final)==0){
        final = CO
        } 
        else{
        final = rbind(final, CO)
      }
      }

      if("PM10" %in% input$aqipollutantcheckbox)
      {
        pm10 <- subset(OpenAqtableLatest, parameter == "pm10", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if(nrow(final)==0){
        final=pm10
        } 
        else{
        final = rbind(final, pm10)
      }
      }

      if("SO2" %in% input$aqipollutantcheckbox)
      {
        SO2 <- subset(OpenAqtableLatest, parameter == "so2", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if(nrow(final)==0){
        final=SO2
        } 
        else{
        final = rbind(final, SO2)
      }
      }

      if("NO2" %in% input$aqipollutantcheckbox)
      {
        NO2 <- subset(OpenAqtableLatest, parameter == "no2", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if(nrow(final)==0){
        final=NO2
        } 
        else{
        final = rbind(final, NO2)
      }
      }

      if("BC" %in% input$aqipollutantcheckbox)
      {
        BC <- subset(OpenAqtableLatest, parameter == "bc", select = c(location, parameter, latitude, longitude, value, unit, lastUpdated))
        if(nrow(final)==0){
        final=BC
        } 
        else{
        final = rbind(final, BC)
      }
      }

      locations <- subset(final, select = c(latitude, longitude))
      locations <- unique(locations)
      
      if(nrow(final != 0)){
      final$popup <- paste0("<B>",final$parameter, "</B>:",final$value)
      final$popString <- rep("NA", nrow(final)) #popup string to display on leaflet
      for(node in unique(final$location)){
      popDF <- subset(final, location == node) #DF for every single node
      val = paste0(toupper(popDF$popup), sep = "/n", collapse="<br>") #concatenates all pollutant values to a string
      final$popString[final$location == node] <- val
            #final$popString<-ifelse(final$node_vsn == node, val)
          }
      }

      map <- leaflet()
      map <- addTiles(map)
      map <- setView(map, lng = locations$longitude[1], lat = locations$latitude[1], zoom = 8)
      map <- addMarkers(map, layerId=final$location, lng = locations$longitude, lat = locations$latitude, label = paste0(final$location), popup = paste0(final$popString))
      map
})
    output$OpenaqgetPollutants <- renderUI({

      OpenAqtable <- OpenAqtable()

      pollutants <- subset(OpenAqtable, select = c(parameter))
      pollutants_list = toupper(unique(pollutants$parameter))
      checkboxGroupInput("aqipollutantcheckbox", label = "Choose Pollutant", choices = pollutants_list, selected = pollutants_list[1], inline=TRUE)

})


    output$Currentnodes = renderUI({
      radioButtons("alivenodes", "Select Node Type", c("All nodes" = "all","Nodes active last 24 hours" = "alive"),inline=TRUE, selected = c("all"))
    })


# output$dailydarkskytableline = renderPlot({

# 	dark_variable =  input$darkskytableradio
# 	dframe = dataframefortable()

# 	s = input$table_rows_selected
# 	if (length(s)) 
# 	{

# 		plot = ggplot()+ scale_x_datetime(date_labels = "%H:%M" , date_breaks = "1 hour")+
# 		labs(x = NULL, y = dark_variable, title = NULL)+ 
# 		theme_light(18)+theme(legend.title = element_text(color = "white"),  
#         panel.grid = element_blank(),
#         legend.text=element_text(size=12),
#         legend.key = element_rect(fill = "grey30"),
#         panel.background = element_rect(fill = 'grey30'))+
#         guides(color=guide_legend(keywidth = 3, keyheight = 2))

# 		selected_node = toString(dframe$node[s[length(s)]])
#         data = subset(Observations, Observations$node_vsn == selected_node)
#         data = distinct(data, node_vsn, .keep_all = TRUE)
#         lat = data$Latitude
#         lng = data$Longitude
#         darksky_lastday <- get_forecast_for(lat, lng, paste0(format(as.Date(time_day), "%Y-%m-%d"), "T", format(time_day, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		darksky_lastday_df <- data.frame(time = `attr<-`(darksky_lastday[["hourly"]][["time"]],"tzone","GMT"), value = darksky_lastday[["hourly"]][["temperature"]])
# 		plot = plot + geom_line(aes(x=darksky_lastday_df$time, y=darksky_lastday_df$value, colour = "Node 1"))
#         if(length(s)>1)
#         {
#         	selected_node_2 = toString(dframe$node[s[length(s)-1]])
#             data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
#             data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
#             lat_2 = data_2$Latitude
#             lng_2 = data_2$Longitude
#             darksky_lastday2 <- get_forecast_for(lat_2, lng_2, paste0(format(as.Date(time_day), "%Y-%m-%d"), "T", format(time_day, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 			darksky_lastday_df2 <- data.frame(time = `attr<-`(darksky_lastday2[["hourly"]][["time"]],"tzone","GMT"), value = darksky_lastday2[["hourly"]][["temperature"]])
#         	plot = plot + geom_line(aes(x=darksky_lastday_df2$time, y=darksky_lastday_df2$value, colour = "Node 2"))
#         } 
#         plot
# 	}

# })

# output$weeklydarkskytableline = renderPlot({

# 	dark_variable =  input$darkskytableradio
# 	dframe = dataframefortable()

# 	s = input$table_rows_selected
# 	if (length(s)) 
# 	{

# 		plot = ggplot()+ scale_x_datetime(date_labels = "%Y-%m-%d" , date_breaks = "1 day")+
# 		labs(x = NULL, y = dark_variable, title = NULL)+ 
# 		theme_light(18)+theme(legend.title = element_text(color = "white"),  
#         panel.grid = element_blank(),
#         legend.text=element_text(size=12),
#         legend.key = element_rect(fill = "grey30"),
#         panel.background = element_rect(fill = 'grey30'))+
#         guides(color=guide_legend(keywidth = 3, keyheight = 2))

# 		selected_node = toString(dframe$node[s[length(s)]])
#         data = subset(Observations, Observations$node_vsn == selected_node)
#         data = distinct(data, node_vsn, .keep_all = TRUE)
#         lat = data$Latitude
#         lng = data$Longitude
        
#         darksky_lastweek <- data.frame()
# 		for (i in c(1,2,3,4,5,6,7)) {
# 		  darksky_i <- get_forecast_for(lat, lng, paste0(format(as.Date(current_time - 86400*i), "%Y-%m-%d"), "T", format(current_time - 86400*0, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		  darksky_i_df <- data.frame(time = `attr<-`(darksky_i[["hourly"]][["time"]],"tzone","GMT"), value = darksky_i[["hourly"]][[dark_variable]])
# 		  darksky_lastweek <- rbind(darksky_lastweek, darksky_i_df)
# 		}
# 		plot = plot + geom_line(aes(x=darksky_lastweek$time, y=darksky_lastweek$value, colour = "Node 1"))
#         if(length(s)>1)
#         {
#         	selected_node_2 = toString(dframe$node[s[length(s)-1]])
#             data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
#             data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
#             lat_2 = data_2$Latitude
#             lng_2 = data_2$Longitude
            
#             darksky_lastweek2 <- data.frame()
# 			for (i in c(1,2,3,4,5,6,7)) {
# 			  darksky_i <- get_forecast_for(lat_2, lng_2, paste0(format(as.Date(current_time - 86400*i), "%Y-%m-%d"), "T", format(current_time - 86400*0, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 			  darksky_i_df <- data.frame(time = `attr<-`(darksky_i[["hourly"]][["time"]],"tzone","GMT"), value = darksky_i[["hourly"]][[dark_variable]])
# 			  darksky_lastweek2 <- rbind(darksky_lastweek2, darksky_i_df)
# 			}


#         	plot = plot + geom_line(aes(x=darksky_lastweek2$time, y=darksky_lastweek2$value, colour = "Node 2"))
#         } 
#         plot
# 	}

# })

# output$dailydarkskytablebar = renderPlot({

# 	dark_variable =  input$darkskytableradio
# 	dframe = dataframefortable()

# 	s = input$table_rows_selected
# 	if (length(s > 1)) 
# 	{

# 		selected_node = toString(dframe$node[s[length(s)]])
#         data = subset(Observations, Observations$node_vsn == selected_node)
#         data = distinct(data, node_vsn, .keep_all = TRUE)
#         lat = data$Latitude
#         lng = data$Longitude

#         darksky_lastday <- get_forecast_for(lat, lng, paste0(format(as.Date(time_day), "%Y-%m-%d"), "T", format(time_day, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		darksky_lastday_df <- data.frame(time = `attr<-`(darksky_lastday[["hourly"]][["time"]],"tzone","GMT"), value = darksky_lastday[["hourly"]][["temperature"]])

#         selected_node_2 = toString(dframe$node[s[length(s)-1]])
#         data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
#         data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
#         lat_2 = data_2$Latitude
#         lng_2 = data_2$Longitude

#         darksky_lastday2 <- get_forecast_for(lat_2, lng_2, paste0(format(as.Date(time_day), "%Y-%m-%d"), "T", format(time_day, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		darksky_lastday_df2 <- data.frame(time = `attr<-`(darksky_lastday2[["hourly"]][["time"]],"tzone","GMT"), value = darksky_lastday2[["hourly"]][["temperature"]])

# 		darkSkyDailySummary <- data.frame(node = c("Node 1", "Node 1", "Node 1", "Node 2", "Node 2", "Node 2"), 
#                                   stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
#                                   value= c(min(darksky_lastday_df$value), mean(darksky_lastday_df$value), max(darksky_lastday_df$value), min(darksky_lastday_df2$value), mean(darksky_lastday_df2$value), max(darksky_lastday_df2$value)))

# 		ggplot(darkSkyDailySummary, aes(y=value, x=node, color=node, fill=node)) + 
# 		  geom_bar( stat="identity") +
# 		  facet_wrap(~stat)


# 	}

# })

# output$weeklydarkskytablebar = renderPlot({

# 	dark_variable =  input$darkskytableradio
# 	dframe = dataframefortable()

# 	s = input$table_rows_selected
# 	if (length(s > 1)) 
# 	{

# 		selected_node = toString(dframe$node[s[length(s)]])
#         data = subset(Observations, Observations$node_vsn == selected_node)
#         data = distinct(data, node_vsn, .keep_all = TRUE)
#         lat = data$Latitude
#         lng = data$Longitude

#         selected_node_2 = toString(dframe$node[s[length(s)-1]])
#         data_2 = subset(Observations, Observations$node_vsn == selected_node_2)
#         data_2 = distinct(data_2, node_vsn, .keep_all = TRUE)
#         lat_2 = data_2$Latitude
#         lng_2 = data_2$Longitude

#         darksky_lastweek <- data.frame()
# 		for (i in c(1,2,3,4,5,6,7)) {
# 		  darksky_i <- get_forecast_for(lat, lng, paste0(format(as.Date(current_time - 86400*i), "%Y-%m-%d"), "T", format(current_time - 86400*0, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		  darksky_i_df <- data.frame(time = `attr<-`(darksky_i[["hourly"]][["time"]],"tzone","GMT"), value = darksky_i[["hourly"]][[dark_variable]])
# 		  darksky_lastweek <- rbind(darksky_lastweek, darksky_i_df)
# 		}

# 		darksky_lastweek2 <- data.frame()
# 		for (i in c(1,2,3,4,5,6,7)) {
# 		  darksky_i <- get_forecast_for(lat_2, lng_2, paste0(format(as.Date(current_time - 86400*i), "%Y-%m-%d"), "T", format(current_time - 86400*0, format = "%H:%M:%S"), "-0600"), add_headers=TRUE)
# 		  darksky_i_df <- data.frame(time = `attr<-`(darksky_i[["hourly"]][["time"]],"tzone","GMT"), value = darksky_i[["hourly"]][[dark_variable]])
# 		  darksky_lastweek2 <- rbind(darksky_lastweek2, darksky_i_df)
# 		}
		
# 		darkSkyWeeklySummary <- data.frame(node = c("Node 1", "Node 1", "Node 1", "Node 2", "Node 2", "Node 2"), 
#                                    stat = c("Min", "Mean", "Max", "Min", "Mean", "Max"),
#                                    value= c(min(darksky_lastweek$value), mean(darksky_lastweek$value), max(darksky_lastweek$value), min(darksky_lastweek2$value), mean(darksky_lastweek2$value), max(darksky_lastweek2$value)))

# 		ggplot(darkSkyWeeklySummary, aes(y=value, x=node, color=node, fill=node)) + 
#   		geom_bar( stat="identity") +
#   		facet_wrap(~stat)
# 	}

# })

output$openaqtable <- DT::renderDataTable(
        DT::datatable({
        clickId <- input$openaqleaf_marker_click$id
        final <- data.frame()
      if("O3" %in% input$aqipollutantcheckbox)
      {
        o3_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "o3", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
        o3_daily <- subset(o3_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        o3_weekly <- subset(o3_table, location == clickId, select = c(value))
        o3_string <- "O3 (ppm)"
        if ("metric" %in% input$unitconv)
        {
            o3_daily$value <- round(((o3_daily$value * 48) / 24.45), 2)
            o3_weekly$value <- round(((o3_weekly$value * 48) / 24.45), 2)
            o3_string <- "O3 (µg/m³)"  
        } 

        o3 <- data.frame("Pollutant" = o3_string,"Location" = clickId, "Min(Daily)" = min(o3_daily$value), "Avg(Daily)" = mean(o3_daily$value), "Max(Daily)" = max(o3_daily$value), "Min(Weekly)" = min(o3_weekly$value), "Avg(Weekly)" = mean(o3_weekly$value), "Max(Weekly)" = max(o3_weekly$value))
        if(nrow(final)==0){
        final=o3
        } 
        else{
        final = rbind(final, o3)
      }
      }

      if("PM25" %in% input$aqipollutantcheckbox)
      {
 pm25_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "pm25", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
        pm25_daily <- subset(pm25_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        pm25_weekly <- subset(pm25_table, location == clickId, select = c(value))
        pm25_string <- "PM2.5 (µg/m³)"
        if ("imp" %in% input$unitconv)
        {
            pm25_daily$value <- (pm25_daily$value * 24.45) / 52.66
            pm25_weekly$value <- (pm25_weekly$value * 24.45) / 52.66
            pm25_string <- "PM2.5 (ppm)"
        }
        pm25 <- data.frame("Pollutant" = pm25_string, "Location" = clickId, "Min(Daily)" = min(pm25_daily$value), "Avg(Daily)" = mean(pm25_daily$value), "Max(Daily)" = max(pm25_daily$value), "Min(Weekly)" = min(pm25_weekly$value), "Avg(Weekly)" = mean(pm25_weekly$value), "Max(Weekly)" = max(pm25_weekly$value))
        if(nrow(final)==0){
        final=pm25
        } 
        else{
        final = rbind(final, pm25)
      }
      }

      if("CO" %in% input$aqipollutantcheckbox)
      {
 co_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "co", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
        co_daily <- subset(co_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        co_weekly <- subset(co_table, location == clickId, select = c(value))
        co_string <- "CO (ppm)"
        if ("metric" %in% input$unitconv)
        {
            co_daily$value <- co_daily$value * 28.01 / 24.45
            co_weekly$value <- co_weekly$value * 28.01 / 24.45
            co_string <- "CO (µg/m³)"  
        }
        co <- data.frame("Pollutant" = co_string,"Location" = clickId, "Min(Daily)" = min(co_daily$value), "Avg(Daily)" = mean(co_daily$value), "Max(Daily)" = max(co_daily$value), "Min(Weekly)" = min(co_weekly$value), "Avg(Weekly)" = mean(co_weekly$value), "Max(Weekly)" = max(co_weekly$value))
        if(nrow(final)==0){
        final=co
        } 
        else{
        final = rbind(final, co)
      }
      }

      if("PM10" %in% input$aqipollutantcheckbox)
      {
        pm10_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "pm10", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
 pm10_daily <- subset(pm10_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        pm10_weekly <- subset(pm10_table, location == clickId, select = c(value))
        pm10_string <- "PM10 (µg/m³)"
        if ("imp" %in% input$unitconv)
        {
            pm10_daily$value <- (pm10_daily$value * 24.45) / 52.66
            pm10_weekly$value <- (pm10_weekly$value * 24.45) / 52.66
            pm10_string <- "PM10 (ppm)"
        }
        pm10 <- data.frame("Pollutant" = pm10_string, "Location" = clickId, "Min(Daily)" = min(pm10_daily$value), "Avg(Daily)" = mean(pm10_daily$value), "Max(Daily)" = max(pm10_daily$value), "Min(Weekly)" = min(pm10_weekly$value), "Avg(Weekly)" = mean(pm10_weekly$value), "Max(Weekly)" = max(pm10_weekly$value))
        if(nrow(final)==0){
        final=pm10
        } 
        else{
        final = rbind(final, pm10)
      }
      }

      if("SO2" %in% input$aqipollutantcheckbox)
      {
 so2_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "so2", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
 so2_daily <- subset(so2_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        so2_weekly <- subset(so2_table, location == clickId, select = c(value))
        so2_string <- "SO2 (ppm)"
        if ("metric" %in% input$unitconv)
        {
            so2_daily$value <- (so2_daily$value * 64.066) / 24.45
            so2_weekly$value <- (so2_weekly$value * 64.066) / 24.45
            so2_string <- "SO2 (µg/m³)"

        }
        so2 <- data.frame("Pollutant" = so2_string, "Location" = clickId, "Min(Daily)" = min(so2_daily$value), "Avg(Daily)" = mean(so2_daily$value), "Max(Daily)" = max(so2_daily$value), "Min(Weekly)" = min(so2_weekly$value), "Avg(Weekly)" = mean(so2_weekly$value), "Max(Weekly)" = max(so2_weekly$value))
        if(nrow(final)==0){
        final=so2
        } 
        else{
        final = rbind(final, so2)
      }
      }

      if("NO2" %in% input$aqipollutantcheckbox)
      {
 no2_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "no2", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
 no2_daily <- subset(no2_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        no2_weekly <- subset(no2_table, location == clickId, select = c(value))
        no2_string <- "NO2 (ppm)"
        if ("metric" %in% input$unitconv)
        {
            no2_daily$value <- (no2_daily$PPM * 46.0055) / 24.45
            no2_daily$value <- (df_NO2_temp$PPM * 46.0055) / 24.45
            no2_string <- "NO2 (µg/m³)"
        }
        
        no2 <- data.frame("Pollutant" = no2_string, "Location" = clickId, "Min(Daily)" = min(no2_daily$value), "Avg(Daily)" = mean(no2_daily$value), "Max(Daily)" = max(no2_daily$value), "Min(Weekly)" = min(no2_weekly$value), "Avg(Weekly)" = mean(no2_weekly$value), "Max(Weekly)" = max(no2_weekly$value))
        if(nrow(final)==0){
        final=no2
        } 
        else{
        final = rbind(final, no2)
      }
      }

      if("BC" %in% input$aqipollutantcheckbox)
      {
bc_table <- aq_measurements(city = "Chicago-Naperville-Joliet", parameter = "bc", date_from = C_date_openaq_weekly, date_to = C_date_openaq)
 bc_daily <- subset(bc_table, location == clickId & date(dateLocal) == C_date_openaq_daily, select = c(value))
        bc_weekly <- subset(no2_table, location == clickId, select = c(value))
        bc_string <- "BC (µg/m³)"
        if ("imp" %in% input$unitconv)
        {
            bc_daily$value <- (bc_daily$value * 24.45) / 52.66
            bc_weekly$value <- (bc_weekly$value * 24.45) / 52.66
            bc_string <- "BC (ppm)"
        }
        bc <- data.frame("Pollutant" = bc_string, "Location" = clickId, "Min(Daily)" = min(bc_daily$value), "Avg(Daily)" = mean(bc_daily$value), "Max(Daily)" = max(bc_daily$value), "Min(Weekly)" = min(bc_weekly$value), "Avg(Weekly)" = mean(bc_weekly$value), "Max(Weekly)" = max(bc_weekly$value))
        if(nrow(final)==0){
        final=bc
        } 
        else{
        final = rbind(final, bc)
      }
      }

      final
      }))

output$UnitConversion = renderUI({
     radioButtons("unitconv", "Unit type", c("Imperial" = "imp","Metric" = "metric"),inline=TRUE, selected = "imp")
   })

#Necessary information about the project

output$content <- renderUI({
  text = "Project 2 - Every Breath You Take <br>
  Course - CS 424 Visualization Analytics - Spring'19 <br><br>
  
  Team(Group 3): <br>
  Sai Krishnan Thiruvarpu Neelakantan - sthiru5@uic.edu <br>
  Praveen Chandrasekaran - pchand34@uic.edu <br>
  Varsha Jayaraman - vjayar6@uic.edu <br>
  Abdullah Aleem - aaleem2@uic.edu <br><br>

  Libraries Used : <br>
  shiny, shinydashboard, ggplot2, lubridate, DT, grid, leaflet, scales, shinycssloaders, shinyWidgets
  tidyverse, tmap, tmaptools, sf, splitstackshape, cdlTools, plotly, AotClient, tidyr <br><br> 

  Data Source : <br>
  The data was collected from https://api.arrayofthings.org <br>
  There are different API calls to get different values. The focus is on the local and current data from the Array of Things in Chicago and 
  to integrate data from multiple sources to compare data and see the difference in values in cities like Chicago etc. <br><br>

  Description and Purpose : <br>
  Will you need your snow boots tonight? Should you bring an umbrella? Accurate weather predictions are important for planning our day-to-day activities. Weather forecasting helps us to make more informed daily decisions, and may even help keep us out of danger.
  When weather events happen, economic repercussions usually follow. Consumer behavior as well as supply and demand for a product or raw material can be affected. Energy demand soars during heatwaves; insurance claims rise after hailstorms; snow slows in-store shopping but can increase online sales; and grain prices spike during drought. Understanding how those impacts affect the U.S. economy has spurred a growing demand for analysis of real-time weather and climate data.
  An increasing number of companies are faced with the need to adapt their business models based on weather volatility. Weather alone can cause the gross domestic product (GDP) to fluctuate 3 to 6 percent a year, or as much as $1.3 trillion, based on a National Weather Service analysis. Demand for value-added weather services is projected to grow by 10–15 percent a year.
  Observations can provide forecasters real-time information that they are able to react to in order to accomplish things like issuing life-saving weather warnings, make critical adjustments to aviation forecasts, and much, much more. The collection of domestic and international observation systems add up to billions of observations of the Earth’s atmosphere measured each day.
As society becomes more sensitive to weather, the importance of instantaneous weather prediction for the protection of lives and property and continued economic growth increases. For example, the U.S. population that resides within 50 miles of the nation’s coastlines and is most threatened by hurricanes and flooding is growing rapidly. Such population growth in these and other high-risk areas significantly increases the need for improved weather predictions and warnings to minimize risks to life and property. Another consideration is that the new economic concept of “just-in-time manufacturing” uses computer-timed and -directed supply systems to eliminate the warehousing of parts and products at ports and factories. However, even minor weather disruptions of land, sea, and air-supply-system pathways caused by snow, ice, and high-wind weather systems can now have large, leveraged impacts on these production systems, whereas previously they had little effect.

Real-time weather-readings can be used to aid fleets in accident reconstruction and crash-related insurance claims as well – expanding the potential return on investment (ROI) for such technology.
Farmers need information to help them plan for the planting and harvesting of their crops. Airlines need to know about local weather conditions in order to schedule flights.

There is a wealth of environmental data used in product and service development as well, for instance:
Energy traders develop consumer-demand forecasts when weather is expected to impact a region
Insurance companies apply forensic analysis to weather-related accidents and claims
Transportation providers determine where to build facilities so that fog, snow, or other weather factors pose fewer challenges to logistics
Retailers analyze how seasonal patterns can affect merchandising and operations

Here’s another important thing we use the weather for, and it’s not something you’re probably thinking of. The weather is often the go-to ice-breaker when you’re meeting someone new and you’re not sure what you have in common. We all have the weather in common.
<br>
Other details can be found <a target='_BLANK' href='https://sthiru5.people.uic.edu/CS424/project3.html'>here</a>
  "


  HTML(text)

})

}

shinyApp(ui = ui, server = server)
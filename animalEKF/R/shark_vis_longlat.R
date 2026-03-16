#load the image files

shark_vis_longlat <- function() {

shinyApp(

ui=shinyUI(fluidPage(
  titlePanel("Bolsa Chica shark trajectory visualization"),

  sidebarLayout(position = "left",
                             sidebarPanel("Simulation parameters", width=4,
							   sliderInput("iter","Progress of simulation",value=1, min=1, max=467711 , round=TRUE, step=150,
				                 animate=animationOptions(interval=150, loop=FALSE)),
							   actionButton("run","Accept settings, press play above"),
							   #sliderInput('speed','Animation speed (seconds per frame)',value=0.15, min=0.01, max=5, round=TRUE, step=0.5),
                               sliderInput('step_range','Range of interpolated steps',value=c(32300, 34300), min=1, max=467711, round=TRUE, step=1),
							   sliderInput('spat_radius','Spatial radius (ft) around position for interaction',value=300, min=0, max=1000, round=TRUE, step=1)
							                         			  
                              ),
                mainPanel(plotOutput("plots", height="600px", width="600px"))
               )#end of layout
			 
  )
)#end of UI definition
,

server=shinyServer(function(input, output, session) 
{
#https://gist.github.com/wch/5436415/

   old_pars <- par(mfrow=par()$mfrow, mfcol=par()$mfcol, mar=par()$mar)
   on.exit(expr=par(old_pars))	
   
   par(mar=rep(0,4), mfcol=c(1,1))
   
   	#distance in latitude
		
	dist_LL <- function(center=c(0,0), otherXY) {
		   #X=lon, Y=lat
		   #print(apply(otherXY, 1, function(x) x-center))
		   d <- -1*t(apply(otherXY, 1, function(x) x-center))#matrix(center, ncol=2, nrow=nrow(otherXY), byrow=TRUE) - otherXY
		  
		  
		   #d[,"lat"] <- d[,"lat"]*pi/180
		   
		   #<- d[,1]*cos(center[2]*pi/180)
		   as.vector(sqrt(rowSums(d^2)))
		}
		 
	#convert feet to lat/long degrees approximately	 

	feet2deg <- function(feet) {
	  #feet/363899.7579477107
	  feet*90/(3280.4*10000)
	}  
		
	long_coords <- -118.039 + feet2deg(feet=seq(0, 3000, by=1000))
	npts <- length(long_coords)
   
    legend_feet <- function() {
		lines(x=long_coords[ c(1, npts) ], y=rep(33.704, 2))
		segments(x0=long_coords, x1=long_coords, y0=rep(33.704,npts)-0.0005, y1=rep(33.704,npts)+0.0005)
		text(x=long_coords, y=rep(33.704, npts)-0.001, labels=seq(0, 3000, by=1000))
		text(x=-118.041, y=33.704, labels="feet")
	}
   
   	
	# load the raster and array objects needed for the map
	# define them first as NULL to avoid Undefined global functions or variables in check
		
	bc_longlat_map <- NULL
	bc_longlat_map_img_ras <- NULL
	shark_data_longlat <- NULL
	
	objects_to_load <- c("bc_longlat_map", "bc_longlat_map_img_ras", "shark_data_longlat")
	# now remove them because sometimes utils::data doesn't seem to overwrite them properly
	rm(list=objects_to_load)
	utils::data(list=objects_to_load, package="animalEKF")
	
		
    observeEvent( input$run, {
	
		req(all(vapply(objects_to_load, exists, FUN.VALUE=c(TRUE))))
	
		
		date_as_sec <- as.integer(1217951746 + (shark_data_longlat[,"t_intervals"]-1)*90)
		shark_data_longlat <- cbind(shark_data_longlat, date_as_sec)
		d <- shark_data_longlat[ shark_data_longlat[,"t_intervals"] >= input$step_range[1] & shark_data_longlat[,"t_intervals"] <= input$step_range[2],]
		d[,"t_intervals"] <- d[,"t_intervals"] - (min(d[,"t_intervals"], na.rm=TRUE)) +1 
		#divide by 100000 for storage purposes
		d[,c("lat","lon")] <- d[,c("lat","lon")]/100000
		
	
		date_labels <- format(as.POSIXlt(seq(min(d[,"date_as_sec"], na.rm=TRUE), max(d[,"date_as_sec"], na.rm=TRUE), by=90), origin="1970-01-01"), "%b %d, %Y\n%I:%M %p")
		nsteps <- length(date_labels)
		 
        updateSliderInput(session, "iter",  label="Progress of simulation", value=1, min=1, max=length(date_labels), step=1)
		
       
	    bbox <- sapply(bc_longlat_map$BBOX, cbind)[2:1,]
		rownames(bbox) <- c("lon","lat")
		colnames(bbox) <- c("min","max")
       
        #do this here so don't have to call the plotting commands every time and waste time	
        plot(bbox["lon",1]-1000, bbox["lat",1]-1000, xlim=bbox["lon",], ylim=bbox["lat",], xlab="Longitude", ylab="Latitude", las=1) 
   	                lims <- par()$usr
   		            rasterImage(bc_longlat_map_img_ras, xleft=lims[1], ybottom=lims[3], xright=lims[2], ytop=lims[4])
                    legend("bottomleft", pt.cex=c(2.5,2.5,3.5), pt.lwd=c(3,3,2), col=c(1,1,2), pch=c(19,1,1), 
							legend=c("foraging (0)","transiting (1)","shark in\nspatial neighborhood"), bty="n")
	    plot_back <- grDevices::recordPlot()
		
		 
		legend_coords <- c(bbox["lon",1] + 0.8*(bbox["lon",2]-bbox["lon",1]), 	bbox["lat",2]-0.25*(bbox["lat",2]-bbox["lat",1]))
        spat_radius_deg <- feet2deg(input$spat_radius) 	
		if (input$spat_radius ==0 ) { spat_radius_deg <- 0 }	
	
		
		
        shark_names <- unique(d[,"tag"])
		nsharks <- length(shark_names)
		
		shark_col <- rep(NA, max(shark_names))
        shark_col[ shark_names ] <- colorspace::rainbow_hcl(n=nsharks+1, l=50, c=100)[1:nsharks]
		
			
		
			output$plots <- renderPlot({
				
				grDevices::replayPlot(plot_back)
				
				dtmp <- d[ d[,"t_intervals"] == input$iter,,drop=FALSE]
				
				
				curr_sharks <- dtmp[,"tag"]
				
				nsharks_curr <- length(curr_sharks)
				#print(nsharks_curr)
				text(x=legend_coords[1], y=legend_coords[2], labels=date_labels[ input$iter ], cex=1.3)
				legend_feet()
				#text(x=-118.04, y=33.7, labels=paste(curr_sharks, collapse="and"), cex=1.3)
				
				if (nsharks_curr > 0) {
				
					points(dtmp[,c("lon","lat")], pch=dtmp[,"pch"], lwd=3, cex=2.5, col=shark_col[ curr_sharks ]) 
					#plot neighborhoods
					if (nsharks_curr>1 & spat_radius_deg>0) {
				
						neib_plot_colors <- rep(NA, max(curr_sharks))
						neib_plot_colors[ curr_sharks ] <- 1
								
				
						for (shk in curr_sharks) {
							
								
							d_to_others <- dist_LL(center=dtmp[dtmp[,"tag"]== shk, c("lon","lat")], otherXY=dtmp[dtmp[,"tag"] != shk , c("lon","lat"), drop=FALSE])
								
							neibs <- c(curr_sharks[ curr_sharks != shk ])[ d_to_others <= spat_radius_deg ] 
						 
							#there are neighbors and at least one is close
							if (length(neibs)>0) {  neib_plot_colors[ c(shk, neibs) ] <- 2 }
						
						}	
						
	 
				      	symbols(dtmp[,c("lon","lat")], circles=rep(spat_radius_deg, nsharks_curr), add=TRUE, inches=FALSE, fg=neib_plot_colors[ curr_sharks ])
			
					}
				}
	   
			})#end of renderPlot
			
			
		})#end of observe event
# })

	})#end of server
	
 
)



}




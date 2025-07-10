#### ENTRY FIELDS FRAME ####

type <- "entryfields"

# Change variables accordingly:
frame.name <- "entryframe1"  
argument.names <- c("Argument 1","Argument 2","Argument 3") 
argument.types <- c("num","num","char") 
arguments <- c("arg1","arg2","arg3") 
initial.values <- c(1,2,"a")
title <- "A Title"
border <- FALSE
entry.width <- c("2","2","6")

# Do not change this line:
new.frames <- .add.frame(input=input,type=type
		,frame.name=frame.name,argument.names=argument.names
		,arguments=arguments,initial.values=initial.values
		,title=title,border=border,entry.width=entry.width
		,argument.types=argument.types  ,new.frames=new.frames)




#### RADIO BUTTONS FRAME 	####

type <- "radiobuttons"

# Change variables accordingly:
frame.name <- "radioframe1"
argument.names <- c("Button 1","Button 2","Button 3")  
arguments <- c("buttonarg")		
argument.types <- "char" 
argument.values <- c("b1","b2","b3") 
initial.values <- "b3"
title <- "Button Options"
border <- TRUE

# DO NOT CHANGE THIS LINE:
new.frames <- .add.frame(input=input,type=type
		,frame.name=frame.name,argument.names=argument.names
		,arguments=arguments,argument.values=argument.values
		,initial.values=initial.values,title=title,border=border
		,new.frames=new.frames,argument.types=argument.types)	


#### CHECK BOXES FRAME  ####

type <- "checkboxes"

# Change variables accordingly:
frame.name <-  "checkboxframe1"
argument.names <- c("Check 1","Check 2","Check 3")  
arguments <- c("checkarg1","checkarg2","checkarg3") 
initial.values <- c(0,1,1)  
title <- "title"
border <- FALSE

# DO NOT CHANGE THIS LINE:
new.frames <- .add.frame(input=input,type=type
		,frame.name=frame.name,argument.names=argument.names
		,arguments=arguments,initial.values=initial.values
		,title=title,border=border,new.frames=new.frames)


#### VALUE SLIDER FRAME  ####

type <- "valuesliders"

# Change variables accordingly:
frame.name <- "sliderframe1"
argument.names <- c("Slider 1  ","Slider 2  ","Slider 3  ")
arguments <- c("sliderarg1","sliderarg2","sliderarg3") 
initial.values <- c(1,5,10)
from <- c(1,1,1) 
to <- c(5,50,500) 
by <- c(1,10,50)  
length <- c(50,100,150) 
title <- "Title"
border <- TRUE

# DO NOT CHANGE THIS LINE:
new.frames <- .add.frame(input=input,type=type,
		title=title,border=border,frame.name=frame.name,
		argument.names=argument.names,arguments=arguments,
		initial.values=initial.values,from=from,to=to,by=by,
		length=length,new.frames=new.frames)


#### SPIN BOX FRAME  ####

type <- "spinboxes"

# Change variables accordingly:
frame.name <- "spinboxframe1"
argument.names <- c("Spin Box 1: ","Spin Box 2: ","Spin Box 3: ") 
arguments <- c("spinarg1","spingarg2","spingarg3") 
initial.values <- c(5,10,20)
from <- c(1,5,10)  
to <- c(10,20,30)
by <- c(1,1,1)
entry.width <- "2"  
title <- "Spin Box !"
border <- TRUE

# DO NOT CHANGE THIS LINE:
new.frames <- .add.frame(input=input,type=type,
		frame.name=frame.name,argument.names=argument.names,
		arguments=arguments,initial.values=initial.values,
		from=from,to=to,by=by,entry.width=entry.width,
		title=title,border=border,new.frames=new.frames)


#### MANUAL BUTTONS FRAME ####

type <- "buttons"

# Change variables accordingly:
frame.name <- "buttonframe1"  
button.name <- "Button 1"  
button.function <- "buttonfunction" 
button.data <- "d" 
button.biclust <-  "biclust" 
button.width <- "12"
button.data.transf <- "matrix"

arg.frames <- c("frame1","frame2")

save <- TRUE 
show <- TRUE
button.otherarg <- "" 

# Do not change this line: 
new.frames <- .add.frame(input=input,frame.name=frame.name,
		type=type,button.name=button.name,
		button.function=button.function,button.data=button.data,
		button.biclust=button.biclust,button.otherarg=button.otherarg,
		button.width=button.width,button.data.transf=button.data.transf,
		arg.frames=arg.frames,save=save,show=show,new.frames=new.frames)



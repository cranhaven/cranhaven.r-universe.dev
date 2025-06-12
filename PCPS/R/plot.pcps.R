#' @rdname pcps
#' @encoding UTF-8
#' @export
plot.pcps<-function(x, groups = NULL, choices = c(1, 2), display = "text", showlabel = TRUE, ...){
	sco <- summary(x, choices = choices)$scores
	if(is.null(groups)){
	  groups <- rownames(sco$scores.species)
	}
	if(length(groups)!=nrow(sco$scores.species)){
		stop(paste("\n groups must have length equal to number of species. Number of species:", nrow(sco$scores.species),"\n"))
	}
	graphics::plot(sco$scores.sites, type = "n", ylim = c(min(sco$scores.sites[,2], sco$scores.species[,2], na.rm = TRUE)-0.05, max(sco$scores.sites[,2], sco$scores.species[,2], na.rm = TRUE)+0.05), xlim = c(min(sco$scores.sites[,1], sco$scores.species[,1], na.rm = TRUE)-0.05, max(sco$scores.sites[,1], sco$sco[,1], na.rm = TRUE)+0.05), ...)
	DISPLAY <- c("text", "points")
	display <- pmatch(display, DISPLAY)
	if (length(display) != 1 | (is.na(display[1]))) {
	  stop("\n Invalid display. Only one argument is accepted in display \n")
	}
	if(display == 1){
		graphics::text(sco$scores.sites, labels = rownames(sco$scores.sites), ...) 
	}
	if(display == 2){
		graphics::points(sco$scores.sites, ...)
	}
	vegan::ordispider(sco$scores.species, groups = groups, label = showlabel, ...)
	if(showlabel){
		g1 <- ifelse(table(groups)==1,1,0)
		g1_groups <- names(g1)[g1==1]
		if(sum(g1)>0){
			for(i in 1:sum(g1)){
				position <- which(groups==g1_groups[i])
				vegan::ordilabel(sco$scores.species[position,,drop=FALSE], labels = groups[position], ...)
			}	
		}
	}
}
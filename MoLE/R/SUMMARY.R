SUMMARY <-
function () {
    summary = list()
    agent = population[[1]]
    nouns = agent$nouns
    summary$generation = agent$generation
    classes = grep("D\\d", names(graveyard$brains[[1]]$nouns))/length(grep("D\\d", 
        names(graveyard$brains[[1]]$nouns)))
    plot(graveyard$summary$generation, graveyard$summary$successRate, 
        ylim = c(0, 1), main = "Success", xlab = "generation", 
        ylab = "success")
    lines(lowess(graveyard$summary$generation, graveyard$summary$successRate))
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        order = graveyard$brains[[i]]$wordOrder
        if (TRUE %in% c((sum(graveyard$brains[[i]]$wordOrder$success) - 
            graveyard$brains[[i]]$wordOrder$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) | 
            c(sum(graveyard$brains[[i]]$wordOrder[grep("^A", 
                graveyard$brains[[i]]$wordOrder$order, invert = TRUE), 
                ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) | 
            c(sum(graveyard$brains[[i]]$wordOrder[grep("V$", 
                graveyard$brains[[i]]$wordOrder$order, invert = TRUE), 
                ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) | 
            c(sum(graveyard$brains[[i]]$wordOrder[grep("UV", 
                graveyard$brains[[i]]$wordOrder$order, invert = TRUE), 
                ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) | 
            c(sum(graveyard$brains[[i]]$wordOrder[grep("VU", 
                graveyard$brains[[i]]$wordOrder$order, invert = TRUE), 
                ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success))))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    order = generation
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        topic = graveyard$brains[[i]]$topicPosition
        if (graveyard$brains[[i]]$topicPosition$success[2] < 
            (sum(graveyard$brains[[i]]$topicPosition$success)/log(sum(graveyard$brains[[i]]$topicPosition$success)))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    topic = generation
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        index = graveyard$brains[[i]]$usageHistory$index
        if (TRUE %in% c(index$no < ((index$yes + index$no)/log(index$yes + 
            index$no)))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    index = generation
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        flag = graveyard$brains[[i]]$usageHistory$flag$person
        if (TRUE %in% c(flag$no < ((flag$yes + flag$no)/log(flag$yes + 
            flag$no)))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    person = generation
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        flag = graveyard$brains[[i]]$usageHistory$flag$actor
        if (TRUE %in% c(flag$no < ((flag$yes + flag$no)/log(flag$yes + 
            flag$no)))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    actor = generation
    generation = vector()
    for (i in seq(2, length(graveyard$brains), 2)) {
        flag = graveyard$brains[[i]]$usageHistory$flag$undergoer
        if (TRUE %in% c(flag$no < ((flag$yes + flag$no)/log(flag$yes + 
            flag$no)))) {
            generation = c(generation, graveyard$brains[[i]]$generation)
        }
    }
    undergoer = generation
    summary$order = list(order = 1:agent$generation, yang = data.frame())
    summary$order$order[!summary$order$order %in% order] = 0
    yang = data.frame(order = c("A first", "V final", "UV", "VU"), 
        generation = 0, stringsAsFactors = FALSE)
    for (i in seq(2, length(graveyard$brains), 2)) {
        generation = graveyard$brains[[i]]$generation
        order = graveyard$brains[[i]]$wordOrder
        if (length(order$order[(sum(order$success) - order$success) < 
            (sum(order$success)/log(sum(order$success)))]) != 
            0) {
            spec = order$order[(sum(order$success) - order$success) < 
                (sum(order$success)/log(sum(order$success)))]
            if (spec %in% yang$order) {
                yang[yang$order == spec, ]$generation = paste(yang[yang$order == 
                  spec, ]$generation, generation)
            }
            if (!spec %in% yang$order) {
                yang[nrow(yang) + 1, ]$order = spec
                yang[nrow(yang), ]$generation = generation
            }
        }
        if (sum(graveyard$brains[[i]]$wordOrder[grep("^A", graveyard$brains[[i]]$wordOrder$order, 
            invert = TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) {
            yang[yang$order == "A first", ]$generation = paste(yang[yang$order == 
                "A first", ]$generation, generation)
        }
        if (sum(graveyard$brains[[i]]$wordOrder[grep("V$", graveyard$brains[[i]]$wordOrder$order, 
            invert = TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) {
            yang[yang$order == "V final", ]$generation = paste(yang[yang$order == 
                "V final", ]$generation, generation)
        }
        if (sum(graveyard$brains[[i]]$wordOrder[grep("UV", graveyard$brains[[i]]$wordOrder$order, 
            invert = TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) {
            yang[yang$order == "UV", ]$generation = paste(yang[yang$order == 
                "UV", ]$generation, generation)
        }
        if (sum(graveyard$brains[[i]]$wordOrder[grep("VU", graveyard$brains[[i]]$wordOrder$order, 
            invert = TRUE), ]$success) < (sum(graveyard$brains[[i]]$wordOrder$success)/log(sum(graveyard$brains[[i]]$wordOrder$success)))) {
            yang[yang$order == "VU", ]$generation = paste(yang[yang$order == 
                "VU", ]$generation, generation)
        }
    }
    yang$generation = gsub("^0 ", "", yang$generation)
    summary$order$yang = yang
    summary$topic = 1:(length(graveyard$brains)/2)
    summary$topic[!summary$topic %in% topic] = 0
    summary$index = 1:(length(graveyard$brains)/2)
    summary$index[!summary$index %in% index] = 0
    summary$person = 1:(length(graveyard$brains)/2)
    summary$person[!summary$person %in% person] = 0
    summary$actor = 1:(length(graveyard$brains)/2)
    summary$actor[!summary$actor %in% actor] = 0
    summary$undergoer = 1:(length(graveyard$brains)/2)
    summary$undergoer[!summary$undergoer %in% undergoer] = 0
    markers=graveyard$brains[[length(graveyard$brains)]]$nouns
    markers=markers[markers$nounMarker>markers$argument,]
    markers=markers[markers$semanticWeight<1,]
    markers=markers[markers$productionEffort<8,]
    markers$roleScore=round(rowMeans(markers[, grep('^D', names(markers))], na.rm = TRUE),1)
    markers=markers[order(markers$nounMarker, decreasing=T),]
    summary$markers=markers[c('ID', 'form', 'nounMarker', 'semanticWeight', 'roleScore')] 
    summary$nounMarkerUse=round(sum(agent$nouns$nounMarker)/sum(agent$verbs$frequency),2)
    if(world$local==T){
	    local = graveyard$brains[[1]]$nouns
	    local = local[local$person != 3, ]
	    local$generation = 0
	    for (i in seq(2, length(graveyard$brains), 2)) {
			new = graveyard$brains[[i]]$nouns
			new = new[new$person != 3, ]
			new$generation = graveyard$brains[[i]]$generation
			local = rbind(local, new)
	    }
	    local = local[, c(ncol(local), 1:(ncol(local) - 1))]
		local$roleScore=round(rowMeans(local[, grep("^D", names(local))], na.rm = TRUE), 1)
	    local=local[local$argument>local$verbMarker,]
	    first = local[local$person == 1, ]
		ff=table(first$generation); ff=names(ff[ff>1]); first=first[first$generation%in%ff,]; 
		summary$first=first[,c('generation', 'ID', 'form', 'argument', 'roleScore', 'semanticWeight')]
		second = local[local$person == 2, ]
		ff=table(second$generation); ff=names(ff[ff>1]); second[second$generation%in%ff,]; 
		summary$second=second[,c('generation', 'ID', 'form', 'argument', 'roleScore', 'semanticWeight')]
    }
    summary
}

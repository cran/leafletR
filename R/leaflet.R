leaflet <-
function(data, dest, title="map", size, base.map="osm", center, zoom, style, popup) {	
	if(missing(data)) data <- NA
	if(missing(dest)) dest <- getwd()
	if(missing(size)) size <- NA
	if(missing(center)) center <- NA
	if(missing(zoom)) zoom <- NA
	if(missing(style)) style <- NA
	if(missing(popup)) popup <- NA
	
	if(length(data)>1 && !is.na(style)) if((length(style)<length(data) && is.list(style)) || !is.list(style)) stop("number of styles must correspond to number of data files")
	or <- "y"
	if(file.exists(file.path(dest, title))) {
		cat(file.path(dest, title), " already exists\n")
		or <- readline("override? [y/n]: ")
	}
	if(or=="y") {
		dir.create(file.path(dest, title), showWarnings=FALSE)
		if(any(!is.na(data))) {
			for(n in 1:length(data)) {
				file.copy(data[[n]], file.path(dest, title))
				#data[[n]] <- unlist(strsplit(data[[n]], "/"))[length(unlist(strsplit(data[[n]], "/")))]
			}
		}
		if(any(is.na(data))) {
			center <- c(0,0)
			zoom <- 2
		}
		filePath <- file.path(dest, title, paste(title, ".html", sep=""))
		leafletInt(data, filePath=filePath, title, size, base.map, center, zoom, style, popup)
		cat("\nYour leaflet map has been saved under ", file.path(dest, title, title), ".html\n", sep="")
		invisible(paste(file.path(dest, title, title), ".html", sep=""))
	}
}
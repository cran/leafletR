toGeoJSON <-
function(data, name, dest, lat.lon) {
	if(missing(data)) stop("'data' is mandatory")
	if(missing(dest)) dest <- getwd()
	if(substr(dest, nchar(dest), nchar(dest))=="/") dest <- substr(dest, 1, nchar(dest)-1)
	path <- NULL
	
	if(is.data.frame(data)) {
		if(missing(name)) name <- deparse(substitute(data))
		if(missing(lat.lon)) lat.lon <- c(1,2)
		path <- dfToGeoJSON(data=data, name=name, dest=dest, lat.lon=lat.lon)
	} else if(class(data)=="character") {
		if(missing(name)) name <- strsplit(tail(strsplit(data, "/")[[1]], 1), "[.]")[[1]][1]
		path <- fileToGeoJSON(data=data, name=name, dest=dest)
	} else {
		stop("Type of data not supported")
	}
	
	if(!is.null(path)) {
		cat("\nFile saved under", path)
		invisible(path)
	}
}
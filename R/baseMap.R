baseMap <-
function(url, attr, ...) {
	
	if(missing(url)) stop("tile url is required")
	if(missing(attr)) stop("attributation is required")
	arg <- list(...)
			
	base.map <- list(url=url)
	base.map[[2]] <- attr
	names(base.map)[2] <- "attribution"
	for(i in 1:length(arg)) {
		base.map[[2+i]] <- arg[[i]]
		names(base.map)[2+i] <- names(arg)[i]
	}
	
	class(base.map) <- "base.map"
	return(base.map)
}

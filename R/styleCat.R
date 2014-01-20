styleCat <-
function(prop, val, style.par, style.val, ...) {
	# style.par: only colors supported so far
	for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	
	cat.style <- paste("return x == \"", val[1], "\" ? \"", style.val[1], "\" :", sep="")
	for(n in 2:length(val)) cat.style <- append(cat.style, paste("       x == \"", val[n], "\" ? \"", style.val[n], "\" :", sep=""))
	if(length(style.val)>length(val)) cat.style <- append(cat.style, paste("       \"", style.val[n+1], "\";", sep=""))
	else cat.style <- append(cat.style, paste("       \"#808080\";", sep=""))
	
	s <- list(...)
	single.style <- NULL
	if(length(s)>0) {
		if(any(names(s)=="col")) {
			if(is.na(s$col)) single.style <- append(single.style, "\"stroke\": false")
			else single.style <- append(single.style, paste("\"color\": \"", s$col, "\"", sep=""))
		}
		if(any(names(s)=="lwd")) single.style <- append(single.style, paste("\"weight\":", s$lwd))
		#if(any(names(s)=="lty")) single.style <- append(single.style, paste("\"dashArray\": \"", paste(toString(substring(s$lty, seq(1,nchar(s$lty),1), seq(1,nchar(s$lty),1))), sep=", "), "\"", sep=""))
		if(any(names(s)=="alpha")) single.style <- append(single.style, paste("\"opacity\":", s$alpha))
		if(any(names(s)=="fill")) {
			if(is.na(s$fill)) single.style <- append(single.style, "\"fill\": false")
			else single.style <- append(single.style, paste("\"fillColor\": \"", s$fill, "\"", sep=""))
		}
		if(any(names(s)=="fill.alpha")) single.style <- append(single.style, paste("\"fillOpacity\":", s$fill.alpha))
		else single.style <- append(single.style, "\"fillOpacity\": 0.5")
		if(any(names(s)=="rad")) single.style <- append(single.style, paste("\"radius\":", s$rad))
	} else single.style <- append(single.style, "\"fillOpacity\": 0.5")
	
	cat.style <- list(cat.style, single.style)
	attr(cat.style, "style.type") <- "categorized"
	attr(cat.style, "property") <- prop
	attr(cat.style, "values") <- val
	return(cat.style)
}
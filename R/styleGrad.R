styleGrad <-
function(prop, breaks, right=TRUE, out=0, style.par, style.val, ...) {
	breaks <- rev(breaks)
	style.val <- rev(style.val)
	# style.par: only colors supported so far
	for(i in 1:length(style.val)) style.val[i] <- getHex(style.val[i])
	
	if(right) op <- ">= " else op <- "> "
	if(out==0) { # left and right closed
		grad.style <- paste("return x ", op, breaks[1], " ? \"#808080\" :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste("       x ", op, breaks[n], " ? \"", style.val[n-1], "\" :", sep=""))
		grad.style <- append(grad.style, paste("       \"#808080\";", sep=""))
	} else if(out==1) { # left closed right open
		grad.style <- paste("return x ", op, breaks[1], " ? \"", style.val[1], "\" :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste("       x ", op, breaks[n], " ? \"", style.val[n], "\" :", sep=""))
		grad.style <- append(grad.style, paste("       \"#808080\";", sep=""))
	} else if(out==2) {
		grad.style <- paste("return x ", op, breaks[1], " ? \"#808080\" :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste("       x ", op, breaks[n], " ? \"", style.val[n-1], "\" :", sep=""))
		grad.style <- append(grad.style, paste("       \"", style.val[n], "\";", sep=""))
	} else { # left and right open
		grad.style <- paste("return x ", op, breaks[1], " ? \"", style.val[1], "\" :", sep="")
		for(n in 2:length(breaks)) grad.style <- append(grad.style, paste("       x ", op, breaks[n], " ? \"", style.val[n], "\" :", sep=""))
		grad.style <- append(grad.style, paste("       \"", style.val[n+1], "\";", sep=""))
	}
	
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
	
	grad.style <- list(grad.style, single.style)
	attr(grad.style, "style.type") <- "graduated"
	attr(grad.style, "property") <- prop
	attr(grad.style, "breaks") <- rev(breaks)
	attr(grad.style, "right") <- right
	attr(grad.style, "out") <- out
	return(grad.style)
}
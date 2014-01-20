leafletInt <-
function(dat, filePath, title, size, base.map, center, zoom, style, popup) {
	# opening
	cat("<!DOCTYPE html>", file=filePath, sep="\n")
	cat("<html>", file=filePath, append=TRUE, sep="\n")
	cat("<head>", file=filePath, append=TRUE, sep="\n")
	
	# web site title
	cat(paste("\t<title>", title, "</title>", sep=""), file=filePath, append=TRUE, sep="\n")
	
	# meta
	cat("\t<meta charset=\"utf-8\" />", file=filePath, append=TRUE, sep="\n")
	cat("\t<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">", file=filePath, append=TRUE, sep="\n")
	
	# stylesheet
	cat("\t<link rel=\"stylesheet\" href=\"http://cdn.leafletjs.com/leaflet-0.7.1/leaflet.css\" />", file=filePath, append=TRUE, sep="\n")
	
	# leaflet lib
	cat("\t<script src=\"http://cdn.leafletjs.com/leaflet-0.7.1/leaflet.js\"></script>", file=filePath, append=TRUE, sep="\n")
	
	# data
	if(any(!is.na(dat))) {
		cat("\t<script src=\"http://code.jquery.com/jquery-1.10.2.min.js\"></script>", file=filePath, append=TRUE, sep="\n")
		for(n in 1:length(dat)) cat(paste("\t<link rel=\"", paste("dat", n, sep=""), "\" type=\"application/json\" href=\"", tail(strsplit(dat[[n]], "/")[[1]], 1), "\" />", sep=""), file=filePath, append=TRUE, sep="\n")
	}
		
	### extra style instructions start #########################################################
	cat("\t<style type=\"text/css\">", file=filePath, append=TRUE, sep="\n")
	cat("\t\tbody {", file=filePath, append=TRUE, sep="\n")
	cat("\t\t\tpadding: 0;", file=filePath, append=TRUE, sep="\n")
	cat("\t\t\tmargin: 0;", file=filePath, append=TRUE, sep="\n")
	cat("\t\t}", file=filePath, append=TRUE, sep="\n")
	
	# fullscreen
	if(any(is.na(size))) {
		cat("\t\thtml, body, #map {", file=filePath, append=TRUE, sep="\n")
		cat("\t\t\theight: 100%;", file=filePath, append=TRUE, sep="\n")
		cat("\t\t}", file=filePath, append=TRUE, sep="\n")
	
	# manual size
	} else {
		cat("\t\t#map {", file=filePath, append=TRUE, sep="\n")
		cat(paste("\t\t\twidth: ", size[1], "px;", sep=""), file=filePath, append=TRUE, sep="\n")
		cat(paste("\t\t\theight: ", size[2], "px;", sep=""), file=filePath, append=TRUE, sep="\n")
		cat("\t\t}", file=filePath, append=TRUE, sep="\n")
	}
	
	# legend
	if(!any(is.na(style))) {
		if(!is.null(attr(style, "style.type"))) {
			if(attr(style, "style.type")=="graduated" || attr(style, "style.type")=="categorized") {
				cat("\t\t.info {", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tpadding: 6px 8px;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tfont: 14px/16px Arial, Helvetica, sans-serif;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tbackground: white;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tbackground: rgba(255,255,255,0.8);", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tbox-shadow: 0 0 15px rgba(0,0,0,0.2);", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tborder-radius: 5px;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t}", file=filePath, append=TRUE, sep="\n")
				cat("\t\t.legend {", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\tline-height: 18px;", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\tcolor: #555;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t}", file=filePath, append=TRUE, sep="\n")
				cat("\t\t.legend i {", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\twidth: 18px;", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\theight: 18px;", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\tfloat: left;", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\tmargin-right: 8px;", file=filePath, append=TRUE, sep="\n")
			    opa <- style[[2]][grep("fillOpacity", style[[2]])]
			    cat(paste("\t\t\topacity: ", substr(opa, nchar(opa)-2, nchar(opa)), ";", sep=""), file=filePath, append=TRUE, sep="\n")
				cat("\t\t}", file=filePath, append=TRUE, sep="\n")
			}
		}
	}
	
	# extra style instructions end 
	cat("\t</style>", file=filePath, append=TRUE, sep="\n")
	#############################################################################################
	
	# end of head, start of body
	cat("</head>", file=filePath, append=TRUE, sep="\n")
	cat("<body>", file=filePath, append=TRUE, sep="\n")
	
	# map anchor
	cat("\t<div id=\"map\"></div>", file=filePath, append=TRUE, sep="\n")
	
	### map script start #######################################################################
	cat("\t<script type=\"text/javascript\">", file=filePath, append=TRUE, sep="\n")	
	
	# initialize the map
	if(is.na(center) || is.na(zoom)) cat("\t\tvar map = L.map('map')", file=filePath, append=TRUE, sep="\n")
	else cat(paste("\t\tvar map = L.map('map').setView([", center[1], ", ", center[2], "],", zoom, ");", sep=""), file=filePath, append=TRUE, sep="\n")
	
	# base layer	
	for(n in 1: length(base.map)) {
		if(base.map[[n]]=="osm") { # OpenStreetMap default
			cat(paste("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tattribution: '&copy; <a href=\"http://openstreetmap.org/copyright\">OpenStreetMap contributors</a>'", file=filePath, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="tls") { # Thunderforest Landscape
			cat(paste("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png', {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tattribution: '&copy; <a href=\"http://openstreetmap.org/copyright\">OpenStreetMap contributors</a>'", file=filePath, append=TRUE, sep="\n")
		} else if (base.map[[n]]=="cm") { # CloudMade
			cat(paste("\t\tvar baseMap", n, " = L.tileLayer('http://{s}.tile.cloudmade.com/BC9A493B41014CAABB98F0471D759707/997/256/{z}/{x}/{y}.png', {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tattribution: '&copy; <a href=\"http://openstreetmap.org/copyright\">OpenStreetMap contributors</a>'", file=filePath, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="mqosm") { # MapQuest OSM
			cat(paste("\t\tvar baseMap", n, " = L.tileLayer('http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png', {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tsubdomains: '1234',", file=filePath, append=TRUE, sep="\n")
			cat("\t\t\ttype: 'osm',", file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tattribution: '&copy; <a href=\"http://openstreetmap.org/copyright\">OpenStreetMap contributors</a>'", file=filePath, append=TRUE, sep="\n")
		} else if(base.map[[n]]=="mqsat") { # MapQuest Open Aerial
			cat(paste("\t\tvar baseMap", n, " = L.tileLayer('http://otile{s}.mqcdn.com/tiles/1.0.0/{type}/{z}/{x}/{y}.png', {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tsubdomains: '1234',", file=filePath, append=TRUE, sep="\n")
			cat("\t\t\ttype: 'sat',", file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tmaxZoom: 11,", file=filePath, append=TRUE, sep="\n")
			cat("\t\t\tattribution: 'Imagery &copy; NASA/JPL-Caltech and USDA Farm Service Agency'", file=filePath, append=TRUE, sep="\n")
		}
		cat("\t\t});", file=filePath, append=TRUE, sep="\n")
		cat(paste("\t\tbaseMap", n, ".addTo(map);", sep=""), file=filePath, append=TRUE, sep="\n") # add base layer
	}	
	
	# data layer
	if(any(!is.na(dat))) {
		# popup
		if(!is.na(popup)) {
			cat("\t\tfunction onEachFeature(feature, layer) {", file=filePath, append=TRUE, sep="\n")
			cat(paste("\t\t\tif (feature.properties && feature.properties.", popup, ") {", sep=""), file=filePath, append=TRUE, sep="\n")
			cat(paste("\t\t\t\tlayer.bindPopup(\"", popup, ": \" + ", "feature.properties.", popup, ");", sep=""), file=filePath, append=TRUE, sep="\n")
			cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
			cat("\t\t};", file=filePath, append=TRUE, sep="\n")
		}
		
		# styling
		if(any(!is.na(style))) {
			if(is.list(style) && is.null(attr(style, "style.type"))) {
				for(n in 1:length(style)) {
					if(any(!is.na(style[[n]]))) {	
						#if(attr(style[[n]], "style.type")=="single") {
						cat(paste("\t\tvar style", n, " = {", sep=""), file=filePath, append=TRUE, sep="\n")
						if(length(style[[n]])==1) cat(paste("\t\t\t", style[[n]], sep=""), file=filePath, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[n]])-1)) cat(paste("\t\t\t", style[[n]][i], ",", sep=""), file=filePath, append=TRUE, sep="\n")
							cat(paste("\t\t\t", style[[n]][length(style[[n]])], sep=""), file=filePath, append=TRUE, sep="\n")
						}
						cat("\t\t};", file=filePath, append=TRUE, sep="\n")
						#}
					}
				}
			} else {
				if(attr(style, "style.type")=="single") {
					cat(paste("\t\tvar style", n, "= {", sep=""), file=filePath, append=TRUE, sep="\n")
					if(length(style)==1) cat(paste("\t\t\t", style, sep=""), file=filePath, append=TRUE, sep="\n")
					else {
						for(i in 1:(length(style)-1)) cat(paste("\t\t\t", style[i], ",", sep=""), file=filePath, append=TRUE, sep="\n")
						cat(paste("\t\t\t", style[length(style)], sep=""), file=filePath, append=TRUE, sep="\n")
					}
					cat("\t\t};", file=filePath, append=TRUE, sep="\n")
				}
				if(attr(style, "style.type")=="graduated") {
					cat("\t\tfunction getValue(x) {", file=filePath, append=TRUE, sep="\n")
					for(n in 1:length(style[[1]])) cat(paste("\t\t\t", style[[1]][n], sep=""), file=filePath, append=TRUE, sep="\n")
					cat("\t\t}", file=filePath, append=TRUE, sep="\n")
					
					cat("\t\tfunction style(feature) {", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\treturn {", file=filePath, append=TRUE, sep="\n")
					if(is.null(style[[2]])) cat(paste("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), ")", sep=""), file=filePath, append=TRUE, sep="\n")
					else {
						cat(paste("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), "),", sep=""), file=filePath, append=TRUE, sep="\n")
						if(length(style[[2]])==1) cat(paste("\t\t\t\t", style[[2]], sep=""), file=filePath, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[2]])-1)) cat(paste("\t\t\t\t", style[[2]][i], ",", sep=""), file=filePath, append=TRUE, sep="\n")
							cat(paste("\t\t\t\t", style[[2]][length(style[[2]])], sep=""), file=filePath, append=TRUE, sep="\n")
						}
					}
					cat("\t\t\t};", file=filePath, append=TRUE, sep="\n")
					cat("\t\t}", file=filePath, append=TRUE, sep="\n")
				}
				if(attr(style, "style.type")=="categorized") {
					cat("\t\tfunction getValue(x) {", file=filePath, append=TRUE, sep="\n")
					for(n in 1:length(style[[1]])) cat(paste("\t\t\t", style[[1]][n], sep=""), file=filePath, append=TRUE, sep="\n")
					cat("\t\t}", file=filePath, append=TRUE, sep="\n")
					
					cat("\t\tfunction style(feature) {", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\treturn {", file=filePath, append=TRUE, sep="\n")
					if(is.null(style[[2]])) cat(paste("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), ")", sep=""), file=filePath, append=TRUE, sep="\n")
					else {
						cat(paste("\t\t\t\t\"color\": getValue(feature.properties.", attr(style, "property"), "),", sep=""), file=filePath, append=TRUE, sep="\n")
						if(length(style[[2]])==1) cat(paste("\t\t\t\t", style[[2]], sep=""), file=filePath, append=TRUE, sep="\n")
						else {
							for(i in 1:(length(style[[2]])-1)) cat(paste("\t\t\t\t", style[[2]][i], ",", sep=""), file=filePath, append=TRUE, sep="\n")
							cat(paste("\t\t\t\t", style[[2]][length(style[[2]])], sep=""), file=filePath, append=TRUE, sep="\n")
						}
					}
					cat("\t\t\t};", file=filePath, append=TRUE, sep="\n")
					cat("\t\t}", file=filePath, append=TRUE, sep="\n")
				}
			}
		}
		
		# data layer
		for(n in 1:length(dat)) {
			ft <- getFeatureType(dat[[n]])
			if(ft=="point") {
				cat(paste("\t\t$.getJSON($(\"link[rel=\'", paste("dat", n, sep=""), "\']\").attr(\"href\"), function(data) {", sep=""), file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tvar dat = L.geoJson(data, {", file=filePath, append=TRUE, sep="\n")
				if(!is.na(popup)) cat("\t\t\t\tonEachFeature: onEachFeature,", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t\tpointToLayer: function (feature, latlng) {", file=filePath, append=TRUE, sep="\n")
		        if(any(is.na(style))) cat("\t\t\t\t\treturn L.circleMarker(latlng);", file=filePath, append=TRUE, sep="\n")
		        else {
		        	if(is.null(attr(style, "style.type"))) {
			        	if(attr(style[[n]], "style.type")=="single") cat(paste("\t\t\t\t\treturn L.circleMarker(latlng, style", n, ");", sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=filePath, append=TRUE, sep="\n")
		        	} else {
			        	if(attr(style, "style.type")=="single") cat(paste("\t\t\t\t\treturn L.circleMarker(latlng, style", n, ");", sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\t\treturn L.circleMarker(latlng, style(feature));", file=filePath, append=TRUE, sep="\n")
		        	}
		        }
		    	cat("\t\t\t\t}", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t});", file=filePath, append=TRUE, sep="\n")
				if(is.na(center) || is.na(zoom)) cat("\t\t\tmap.fitBounds(dat.getBounds());", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tdat.addTo(map);", file=filePath, append=TRUE, sep="\n")
				cat("\t\t});", file=filePath, append=TRUE, sep="\n")
			} else if(ft=="line") {
				cat(paste("\t\t$.getJSON($(\"link[rel=\'", paste("dat", n, sep=""), "\']\").attr(\"href\"), function(data) {", sep=""), file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tvar dat = L.geoJson(data, {", file=filePath, append=TRUE, sep="\n")
				if(any(!is.na(style)))   {
					if(!is.na(popup)) cat("\t\t\t\tonEachFeature: onEachFeature,", file=filePath, append=TRUE, sep="\n")
			        if(is.null(attr(style, "style.type"))) {
			        	if(attr(style[[n]], "style.type")=="single") cat(paste("\t\t\t\tstyle: style", n, sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\tstyle: style", file=filePath, append=TRUE, sep="\n")
			        } else {
			        	if(attr(style, "style.type")=="single") cat(paste("\t\t\t\tstyle: style", n, sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\tstyle: style", file=filePath, append=TRUE, sep="\n")
					}
				} else if(!is.na(popup)) cat("\t\t\t\tonEachFeature: onEachFeature", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t});", file=filePath, append=TRUE, sep="\n")
				if(is.na(center) || is.na(zoom)) cat("\t\t\tmap.fitBounds(dat.getBounds());", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tdat.addTo(map);", file=filePath, append=TRUE, sep="\n")
				cat("\t\t});", file=filePath, append=TRUE, sep="\n")
			} else if(ft=="polygon") {
				cat(paste("\t\t$.getJSON($(\"link[rel=\'", paste("dat", n, sep=""), "\']\").attr(\"href\"), function(data) {", sep=""), file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tvar dat = L.geoJson(data, {", file=filePath, append=TRUE, sep="\n")
				if(any(!is.na(style)))   {
					if(!is.na(popup)) cat("\t\t\t\tonEachFeature: onEachFeature,", file=filePath, append=TRUE, sep="\n")
			        if(is.null(attr(style, "style.type"))) {
			        	cat(attr(style, "style.type"))
			        	if(attr(style[[n]], "style.type")=="single") cat(paste("\t\t\t\tstyle: style", n, sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\tstyle: style", file=filePath, append=TRUE, sep="\n")
			        } else {
			        	if(attr(style, "style.type")=="single") cat(paste("\t\t\t\tstyle: style", n, sep=""), file=filePath, append=TRUE, sep="\n")
			        	else cat("\t\t\t\tstyle: style", file=filePath, append=TRUE, sep="\n")
			        }
				} else if(!is.na(popup)) cat("\t\t\t\tonEachFeature: onEachFeature", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t});", file=filePath, append=TRUE, sep="\n")
				if(is.na(center) || is.na(zoom)) cat("\t\t\tmap.fitBounds(dat.getBounds());", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tdat.addTo(map);", file=filePath, append=TRUE, sep="\n")
				cat("\t\t});", file=filePath, append=TRUE, sep="\n")
			}
		}
	}
	
	# layer control
	if(length(base.map)>1) {
		cat("\t\tvar baseMaps = {", file=filePath, append=TRUE, sep="\n")
		for(n in 1:(length(base.map)-1)) {
			if(base.map[[n]]=="osm") cat(paste("\t\t\t\"OpenStreetMap\": baseMap", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
			if(base.map[[n]]=="tls") cat(paste("\t\t\t\"Thunderforest Landscape\": baseMap", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
			if(base.map[[n]]=="cm") cat(paste("\t\t\t\"CloudMade\": baseMap", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
			if(base.map[[n]]=="mqosm") cat(paste("\t\t\t\"MapQuest OSM\": baseMap", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
			if(base.map[[n]]=="mqsat") cat(paste("\t\t\t\"MapQuest Open Aerial\": baseMap", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
		}
		if(base.map[[length(base.map)]]=="osm") cat(paste("\t\t\t\"OpenStreetMap\": baseMap", length(base.map), sep=""), file=filePath, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="tls") cat(paste("\t\t\t\"Thunderforest Landscape\": baseMap", length(base.map), sep=""), file=filePath, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="cm") cat(paste("\t\t\t\"CloudMade\": baseMap", length(base.map), sep=""), file=filePath, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="mqosm") cat(paste("\t\t\t\"MapQuest OSM\": baseMap", length(base.map), sep=""), file=filePath, append=TRUE, sep="\n")
		if(base.map[[length(base.map)]]=="mqsat") cat(paste("\t\t\t\"MapQuest Open Aerial\": baseMap", length(base.map), sep=""), file=filePath, append=TRUE, sep="\n")
		cat("\t\t};", file=filePath, append=TRUE, sep="\n")
		cat("\t\tL.control.layers(baseMaps).addTo(map);", file=filePath, append=TRUE, sep="\n")
	}
	
	# problems with referencing geojson -> initialized only in callback function(s)
	#if(layer.switcher) {
	#	if(length(dat)>1) {
	#		cat("\t\tvar overlayMaps = {", file=filePath, append=TRUE, sep="\n")
	#		for(n in 1:(length(dat)-1)) {
	#			str(dat[[n]])
	#			cat("\n")
	#			cat(paste("\t\t\t\"", strsplit(dat[[n]], ".", fixed=TRUE)[[1]][1], "\": dat", n, ",", sep=""), file=filePath, append=TRUE, sep="\n")
	#		}
	#		cat(paste("\t\t\t\"", strsplit(dat[[length(dat)]], ".", fixed=TRUE)[[1]][1], "\": dat", length(dat), sep=""), file=filePath, append=TRUE, sep="\n")
	#		cat("\t\t};", file=filePath, append=TRUE, sep="\n")
	#	} else {
	#		cat("\t\tvar overlayMaps = {", file=filePath, append=TRUE, sep="\n")
	#		cat(paste("\t\t\t\"", strsplit(dat, ".", fixed=TRUE)[[1]][1], "\": dat", sep=""), file=filePath, append=TRUE, sep="\n")
	#		cat("\t\t};", file=filePath, append=TRUE, sep="\n")
	#	}
	#}
	
	#if(length(base.map)>1 && layer.switcher) cat("\t\tL.control.layers(baseMaps, overlayMaps).addTo(map);", file=filePath, append=TRUE, sep="\n")
	#if(length(base.map)>1 && !layer.switcher) cat("\t\tL.control.layers(baseMaps).addTo(map);", file=filePath, append=TRUE, sep="\n")
	#if(length(base.map)==1 && layer.switcher) cat("\t\tL.control.layers(overlayMaps).addTo(map);", file=filePath, append=TRUE, sep="\n")
	
	# add legend
	if(!any(is.na(style))) {
		if(!is.null(attr(style, "style.type"))) {
			if(attr(style, "style.type")=="graduated") {
				cat("\t\tvar legend = L.control({position: 'bottomright'});", file=filePath, append=TRUE, sep="\n")
				cat("\t\tlegend.onAdd = function(map) {", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tvar div = L.DomUtil.create('div', 'info legend'),", file=filePath, append=TRUE, sep="\n")
		        cat("\t\t\tlabels = [],", file=filePath, append=TRUE, sep="\n")
		        cat(paste("\t\t\tgrades = [", paste(attr(style, "breaks"), collapse=", "), "];", sep=""), file=filePath, append=TRUE, sep="\n")
			    if(attr(style, "out")==0) { # left and right closed
				    cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=filePath, append=TRUE, sep="\n")
			        cat("\t\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
			        if(attr(style, "right")) cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
			        else cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\t\t\tgrades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
				} else if(attr(style, "out")==1) { # left closed and right open
				    cat("\t\t\tfor (var i = 0; i < grades.length; i++) {", file=filePath, append=TRUE, sep="\n")
			        cat("\t\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
			        if(attr(style, "right")) {
			        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
			        	cat("\t\t\t\t\t(grades[i + 1] ? grades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\' : \'&ge;\' + grades[i]);", file=filePath, append=TRUE, sep="\n")
			        } else {
			        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
			        	cat("\t\t\t\t\t(grades[i + 1] ? grades[i] + \'&ndash;\' + grades[i + 1] + \'<br>\' : \'&gt;\' + grades[i]);", file=filePath, append=TRUE, sep="\n")
			        }
					cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
				} else if(attr(style, "out")==2) { # left open and right closed
					cat("\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
					if(attr(style, "right")) cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &lt;\' + grades[0] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
					else cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &le;\' + grades[0] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\tfor (var i = 0; i < grades.length-1; i++) {", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
					if(attr(style, "right")) cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
					else cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : grades[i] + \'&ndash;\' + grades[i+1]);", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
				} else { # left and right open
					cat("\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
					if(attr(style, "right")) cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &lt;\' + grades[0] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
					else cat("\t\t\t\t\'<i style=\"background:\' + getValue(grades[0]-(grades[1]-grades[0])*0.01) + \'\"></i> &le;\' + grades[0] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
					cat("\t\t\tfor (var i = 0; i < grades.length; i++) {", file=filePath, append=TRUE, sep="\n")
			        cat("\t\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
			        if(attr(style, "right")) {
			        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
			        	cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : \'&ge;\' + grades[i]);", file=filePath, append=TRUE, sep="\n")
					} else {
			        	cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(grades[i]+(grades[1]-grades[0])*0.01) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
						cat("\t\t\t\t\t(i<grades.length-1 ? grades[i] + \'&ndash;\' + grades[i+1] + \'<br>\' : \'&gt;\' + grades[i]);", file=filePath, append=TRUE, sep="\n")
			        }
					cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
				}
				
				cat("\t\t\treturn div;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t};", file=filePath, append=TRUE, sep="\n")
				cat("\t\tlegend.addTo(map);", file=filePath, append=TRUE, sep="\n")
			}
			if(attr(style, "style.type")=="categorized") {
				cat("\t\tvar legend = L.control({position: 'bottomright'});", file=filePath, append=TRUE, sep="\n")
				cat("\t\tlegend.onAdd = function(map) {", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tvar div = L.DomUtil.create('div', 'info legend'),", file=filePath, append=TRUE, sep="\n")
		        cat("\t\t\tlabels = [],", file=filePath, append=TRUE, sep="\n")
		        cat(paste("\t\t\tcats = [\"", paste(attr(style, "values"), collapse="\", \""), "\"];", sep=""), file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tfor (var i = 0; i < cats.length; i++) {", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\t\t\t\'<i style=\"background:\' + getValue(cats[i]) + \'\"></i> \' +", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t\t\tcats[i] + \'<br>\';", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\t}", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\tdiv.innerHTML +=", file=filePath, append=TRUE, sep="\n")
			    cat("\t\t\t\t\'<i style=\"background:\' + getValue() + \'\"></i> NA\'", file=filePath, append=TRUE, sep="\n")
				cat("\t\t\treturn div;", file=filePath, append=TRUE, sep="\n")
				cat("\t\t};", file=filePath, append=TRUE, sep="\n")
				cat("\t\tlegend.addTo(map);", file=filePath, append=TRUE, sep="\n")
			}
		}
	}
		
	# map script end 
	cat("\t</script>", file=filePath, append=TRUE, sep="\n")
	#############################################################################################
	
	# closing
	cat("</body>", file=filePath, append=TRUE, sep="\n")
	cat("</html>", file=filePath, append=TRUE)	
}
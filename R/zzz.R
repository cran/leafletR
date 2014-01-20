.onAttach <- 
function(libname, pkgname) {
    ver <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
    packageStartupMessage(" ")
    packageStartupMessage(" ")
    packageStartupMessage(paste("This is", pkgname, ver))
    packageStartupMessage(" ")
    packageStartupMessage("Type changes(\"leafletR\") to see changes/bug fixes, help(leafletR) for documentation")
    packageStartupMessage("or citation(\"leafletR\") for how to cite leafletR.")
}


changes <- 
function(pkg="leafletR") {
    if(pkg=="leafletR") file.show(file.path(system.file(package="leafletR"), "NEWS"))
}


### short name wrapper functions

cats <- function(prop, val, style.par, style.val, ...) {
	styleCat(prop, val, style.par, style.val, ...)
}

grads <- function(prop, breaks, right=TRUE, out=0, style.par, style.val, ...) {
	styleGrad(prop, breaks, right, out, style.par, style.val, ...)
}

leaf <- function(data, dest, title="map", size, base.map="osm", center, zoom, style, popup) {
	leaflet(data, dest, title, size, base.map, center, zoom, style, popup)
}

singles <- function(col, lwd, alpha, fill, fill.alpha, rad) {
	styleSingle(col, lwd, alpha, fill, fill.alpha, rad)
}

tg <- function(data, name, dest, lat.lon) {
	toGeoJSON(data, name, dest, lat.lon)
}
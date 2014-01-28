\encoding{UTF-8}
\name{styleCat}
\alias{styleCat}
\alias{cats}
\title{Categorized styling}
\description{Creates a categorized style based on an attribute}
\usage{
styleCat(prop, val, style.par, style.val, leg, ...)
cats(prop, val, style.par, style.val, leg, ...)
}
\arguments{
  \item{prop}{Property (attribute) of the data to be styled, as string.}
  \item{val}{A vector giving the data values to be used as categories.}
  \item{style.par}{\emph{Unimplemented}.}
  \item{style.val}{Styling values, a vector of colors applied to the categories given by \code{val}.}
  \item{leg}{Legend title as string. The line break sequence \code{\\n} may be used for line splitting.}
  \item{\dots}{Additional styling parameters, see \code{\link{styleSingle}} for details.}
}
\value{A categorized style object.}
\author{Christian Graul}
\seealso{\code{\link{styleSingle}}, \code{\link{styleGrad}}, \code{\link{leaflet}}}
\examples{
\dontrun{
# prepare data
dat <- toGeoJSON(data=system.file(package="leafletR", "files", 
  "park_sk.zip"), dest=tempdir())

# simple categorizing
sty <- styleCat(prop="lynx", val=c("yes", "no"), 
  style.val=c("green", "red"), leg="Lynx occurrence")
map <- leaflet(data=dat, dest=tempdir(), title="Lynx", 
  style=sty)
browseURL(map)

# just one category
sty <- styleCat(prop="wisent", val="yes", style.val="red", 
  leg="Wisent occurrence")
map <- leaflet(data=dat, dest=tempdir(), title="Wisent", 
  style=sty)
browseURL(map)

# change color for negatives (NA)
sty <- styleCat(prop="grey_wolf", val="yes", style.val=c("red", "white"), 
  leg="Grey wolf\noccurrence")
map <- leaflet(data=dat, dest=tempdir(), title="Grey wolf", 
  base.map="mqsat", style=sty)
browseURL(map)

# get nice colors using ColorBrewer
require(RColorBrewer)
pal <- brewer.pal(7, "Dark2")
sty <- styleCat(prop="year", val=c("1949", "1967", "1978", "1988", 
  "1997", "1998", "2002"), style.val=pal, leg="established:\n")
map <- leaflet(data=dat, dest=tempdir(), 
  title="National parks", style=sty)
browseURL(map)

# additional styling parameters
sty <- styleCat(prop="brown_bear", val=c("yes", "no"), 
  style.val=c("darkgreen", "red"), leg="Brown bear\noccurrence", 
  alpha=1, lwd=4, fill=NA)
map <- leaflet(data=dat, dest=tempdir(), title="Brown bear", 
  style=sty)
browseURL(map)
}
}
\keyword{methods}
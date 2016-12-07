library('ggplot2')
library('grid')
library('RColorBrewer')

########### Example plots #################

####Bar
##1 color
#print(ggplot(mtcars, aes(factor(cyl))) + geom_bar() + coord_cartesian(ylim = c(0, 100))+ggtitle("Title"))

##3 colors
#print(qplot(factor(cyl), data=mtcars, geom="bar", fill=factor(cyl))+ggtitle("Title"))

##5 colors
#print(ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar() +ggtitle("Title") + coord_cartesian(ylim = c(0, 15000)))


####Scatter
##3 colors
#print(ggplot(mtcars, aes(wt, mpg))+geom_point(aes(colour = factor(cyl)))+ggtitle("Title"))

##9 colors
 # dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
 # d <- qplot(carat, price, data=dsamp, colour=clarity, size = 3)
 # print(d+ggtitle("Title"))

###Line
##3 colors
# mtcars.long <- melt(mtcars, id = "mpg", measure = c("disp", "hp", "wt"))
# print(ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line()+ggtitle("Title"))

###Facet Grid
# p <- ggplot(mtcars, aes(mpg, wt)) + geom_point() + ggtitle("Title")
# print(p + facet_grid(vs ~ am, margins=TRUE))

###Histogram
##22 colors
# print(ggplot(mtcars) + scale_fill_manual(values = extendedPalette(22)) + geom_histogram(aes(factor(hp), fill=factor(hp))))


####################################


#resize window to 650 px width
#quartz.options(width = 8.33333333333333, height = 5.55555555555556, dpi = 72)

# For windows, uncomment below line (and comment out above line)
windows.options(width = 8.33333333333333, height = 5.55555555555556)

#################### redefine default ggplot theme ###################
theme_new <- theme_set(theme_bw())
theme_new <- theme_update(
	line = element_line(colour = "#000000", 
	                    size = 0.5, 
	                    linetype = 1L, 
	                    lineend = "butt"), 
	rect = element_rect(fill = "#FFFFFF", 
	                    colour = "#000000", 
	                    size = 0.5, 
	                    linetype = 1L), 
	text = element_text(family = "Lato", 
	                    face = "plain", 
	                    colour = "#000000", 
	                    size = 12L, 
	                    hjust = 0.5, 
	                    vjust = 0.5, 
	                    angle = 0, 
	                    lineheight = 0.9, 
	                    margin = structure(c(0, 0, 0, 0), 
	                                       unit = "pt", 
	                                       valid.unit = 8L, 
	                                       class = c("margin", "unit")), 
	                    debug = FALSE), 
	title = element_text(family = NULL, 
	                     face = NULL, 
	                     colour = NULL, 
	                     size = 18L, 
	                     hjust = NULL, 
	                     vjust = NULL, 
	                     angle = NULL, 
	                     lineheight = NULL,
	                     margin = structure(c(0, 0, 0, 0), 
	                                        unit = "pt", 
	                                        valid.unit = 8L, 
	                                        class = c("margin", "unit")),
	                     debug = FALSE), 
	axis.text = element_text(family = NULL, 
	                         face = NULL, 
	                         colour = NULL, 
	                         size = 12L, 
	                         hjust = NULL, 
	                         vjust = NULL, 
	                         angle = NULL, 
	                         lineheight = NULL, 
	                         margin = structure(c(0, 0, 0, 0), 
	                                            unit = "pt", 
	                                            valid.unit = 8L, 
	                                            class = c("margin", "unit")), 
	                         debug = FALSE),
  strip.text = element_text(family = NULL, 
                            face = "bold", 
                            colour = NULL, 
                            size = 14L, 
                            hjust = NULL, 
                            vjust = NULL, 
                            angle = NULL, 
                            lineheight = NULL, 
                            margin = structure(c(0, 0, 0, 0), 
                                               unit = "pt", 
                                               valid.unit = 8L, 
                                               class = c("margin", "unit")), 
                            debug = FALSE), 
	axis.line = element_line(colour = NULL, 
	                         size = NULL, 
	                         linetype = NULL, 
	                         lineend = NULL), 
	axis.text.x = element_text(family = NULL, 
	                           face = NULL, 
	                           colour = NULL, 
	                           size = NULL, 
	                           hjust = NULL, 
	                           vjust = NULL, 
	                           angle = NULL, 
	                           lineheight = NULL, 
	                           margin = structure(c(0, 0, 0, 0), 
	                                              unit = "pt", 
	                                              valid.unit = 8L, 
	                                              class = c("margin", "unit")), 
	                           debug = FALSE), 
	axis.text.y = element_text(family = NULL, 
	                           face = NULL, 
	                           colour = NULL, 
	                           size = NULL, 
	                           hjust = NULL, 
	                           vjust = NULL, 
	                           angle = NULL, 
	                           lineheight = NULL, 
	                           margin = structure(c(0, 0, 0, 0), 
	                                              unit = "pt", 
	                                              valid.unit = 8L, 
	                                              class = c("margin", "unit")), 
	                           debug = FALSE), 
	axis.ticks = element_line(colour = NULL, 
	                          size = NULL, 
	                          linetype = NULL, 
	                          lineend = NULL), 
	axis.title.x = element_text(family = NULL, 
	                            face = NULL, 
	                            colour = NULL, 
	                            size = NULL, 
	                            hjust = NULL, 
	                            vjust = NULL, 
	                            angle = NULL, 
	                            lineheight = NULL, 
	                            margin = structure(c(10, 0, 0, 0), 
	                                               unit = "pt", 
	                                               valid.unit = 8L, 
	                                               class = c("margin", "unit")), 
	                            debug = FALSE), 
	axis.title.y = element_text(family = NULL, 
	                            face = NULL, 
	                            colour = NULL, 
	                            size = NULL, 
	                            hjust = NULL, 
	                            vjust = NULL, 
	                            angle = NULL, 
	                            lineheight = NULL, 
	                            margin = structure(c(0, 10, 0, 0), 
	                                               unit = "pt", 
	                                               valid.unit = 8L, 
	                                               class = c("margin", "unit")), 
	                            debug = FALSE), 
	axis.ticks.length = unit(4L, "points"), 
	legend.background = element_blank(), 
	legend.spacing = unit(8L, "points"), 
	legend.key = element_rect(fill = NULL, 
	                          colour = NULL, 
	                          size = 0L, 
	                          linetype = NULL), 
	legend.key.size = unit(10L, "points"), 
	legend.key.height = NULL, 
	legend.key.width = NULL, 
	legend.text = element_text(family = NULL, 
	                           face = NULL, 
	                           colour = NULL, 
	                           size = 12L, 
	                           hjust = NULL, 
	                           vjust = NULL, 
	                           angle = NULL, 
	                           lineheight = NULL, 
	                           margin = structure(c(0, 0, 0, 0), 
	                                              unit = "pt", 
	                                              valid.unit = 8L, 
	                                              class = c("margin", "unit")), 
	                           debug = FALSE), 
	legend.text.align = NULL, 
	legend.title = element_blank(), 
	legend.title.align = NULL, 
	legend.position = "top", 
	legend.direction = "horizontal", 
	legend.justification = NULL, 
	legend.box = "horizontal", 
	panel.background = element_blank(), 
	panel.border = element_blank(), 
	panel.grid.major = element_line(colour = NULL, 
	                                size = NULL, 
	                                linetype = NULL, 
	                                lineend = NULL), 
	panel.grid.minor = element_line(colour = NULL, 
	                                size = NULL, 
	                                linetype = NULL, 
	                                lineend = NULL), 
	panel.spacing = unit(2L, "points"))


theme_new <- theme_update(panel.spacing.x = unit(0,"lines"), 
                          panel.spacing.y = unit(0,"lines"), 
                          panel.ontop = FALSE, 
                          strip.background = element_rect(fill = "#dedddd", 
                                                          colour = NULL, 
                                                          size = NULL, 
                                                          linetype = 0L), 
                          strip.text.x = element_text(family = NULL, 
                                                      face = NULL, 
                                                      colour = NULL, 
                                                      size = NULL, 
                                                      hjust = NULL, 
                                                      vjust = NULL, 
                                                      angle = NULL, 
                                                      lineheight = NULL, 
                                                      margin = structure(c(0, 0, 0, 0), 
                                                                         unit = "pt", 
                                                                         valid.unit = 8L, 
                                                                         class = c("margin", "unit")), 
                                                      debug = FALSE), 
                          strip.text.y = element_text(family = NULL, 
                                                      face = NULL, 
                                                      colour = NULL, 
                                                      size = NULL, 
                                                      hjust = NULL, 
                                                      vjust = NULL, 
                                                      angle = NULL, 
                                                      lineheight = NULL, 
                                                      margin = structure(c(0, 0, 0, 0), 
                                                                         unit = "pt", 
                                                                         valid.unit = 8L, 
                                                                         class = c("margin", "unit")), 
                                                      debug = FALSE), 
                          strip.switch.pad.grid = unit(0,"lines"), 
                          strip.switch.pad.wrap = unit(0,"lines"), 
                          plot.background = element_rect(fill = NULL, 
                                                         colour = NULL, 
                                                         size = NULL, 
                                                         linetype = NULL), 
                          plot.title = element_text(family = NULL, 
                                                    face = NULL, 
                                                    colour = NULL, 
                                                    size = NULL, 
                                                    hjust = 0, 
                                                    vjust = NULL, 
                                                    angle = NULL, 
                                                    lineheight = NULL, 
                                                    margin = structure(c(0, 0, 0, 0), 
                                                                       unit = "pt", 
                                                                       valid.unit = 8L, 
                                                                       class = c("margin", "unit")), 
                                                    debug = FALSE), 
                          plot.margin = unit(c(10L, 10L, 10L, 10L), "points"), 
                          axis.line.x = element_line(colour = NULL, 
                                                     size = NULL, 
                                                     linetype = NULL, 
                                                     lineend = NULL), 
                          axis.line.y = element_blank(), axis.title = element_text(family = NULL, 
                                                                                   face = "italic", 
                                                                                   colour = NULL, 
                                                                                   size = 13L, 
                                                                                   hjust = NULL, 
                                                                                   vjust = NULL, 
                                                                                   angle = NULL, 
                                                                                   lineheight = NULL, 
                                                                                   margin = NULL, 
                                                                                   debug = NULL), 
                          axis.ticks.x = element_line(colour = NULL, 
                                                      size = NULL, 
                                                      linetype = NULL, 
                                                      lineend = NULL), 
                          axis.ticks.y = element_blank(), 
                          panel.grid = element_line(colour = NULL, 
                                                    size = NULL, 
                                                    linetype = NULL, 
                                                    lineend = NULL), 
                          panel.grid.major.x = element_blank(), 
                          panel.grid.major.y = element_line(colour = "#DEDDDD", 
                                                            size = NULL, 
                                                            linetype = NULL, 
                                                            lineend = NULL), 
                          panel.grid.minor.x = element_blank(), 
                          panel.grid.minor.y = element_blank())

#############################

#Redefine default discrete colors, up to 9 colors.
scale_colour_discrete <- function(...) scale_colour_custom(..., palette="Set1")
scale_fill_discrete <- function(...) scale_fill_custom(... , palette="Set1")

#################### Functions to Define custom colours #####################
divlist<-c("BrBG","PiYG","PRGn","PuOr","RdBu","RdGy","RdYlBu","RdYlGn","Spectral")
quallist<-c("Accent","Dark2","Paired","Pastel1","Pastel2","Set1","Set2","Set3")
seqlist<-c("Blues","BuGn","BuPu","GnBu","Greens","Greys","Oranges","OrRd",
"PuBu","PuBuGn","PuRd","Purples","RdPu","Reds","YlGn","YlGnBu","YlOrBr","YlOrRd")

divnum <- rep(11, length(divlist))
qualnum <- c( 8, 8, 12, 9, 8, 9, 8, 12)
seqnum <- rep(9, length(seqlist))

namelist<-c(divlist,quallist,seqlist)
maxcolours <- c(divnum,qualnum,seqnum)
catlist<-rep(c("div","qual","seq"),c(length(divlist),length(quallist),length(seqlist)))

custom.pal.info<-data.frame(maxcolours=maxcolours,category=catlist,row.names=namelist)

custom.pal<-function(n,name){
 if(!(name %in% namelist)){
 stop(paste(name,"is not a valid palette name for custom.pal\n"))
 }
 if(n<3){
 warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
 return(custom.pal(3,name))
 }
 if(n>maxcolours[which(name==namelist)]){
 warning(paste("n too large, allowed maximum for palette",name,"is",maxcolours[which(name==namelist)]),
"\nReturning the palette you asked for with that many colours\n")
 return(custom.pal(maxcolours[which(name==namelist)],name))
 }

c1 <- col2rgb("#1696d2")
c2 <- col2rgb("#fdbf11")
c3 <- col2rgb("#000000")
c4 <- col2rgb("#ec008b")
c5 <- col2rgb("#d2d2d2")
c6 <- col2rgb("#55B748")
c7 <- col2rgb("#5c5859")
c8 <- col2rgb("#db2b27")
c9 <- col2rgb("#761548")

 switch(name,

  Set1 =  switch(n,

rgb(c(c1[1]),
  c(c1[2]),
  c(c1[3]),maxColorValue=255),
rgb(c(c1[1],c2[1]),
  c(c1[2],c2[2]),
  c(c1[3],c2[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1]),
  c(c1[2],c2[2],c3[2]),
  c(c1[3],c2[3],c3[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1]),
  c(c1[2],c2[2],c3[2],c4[2]),
  c(c1[3],c2[3],c3[3],c4[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1]),
  c(c1[2],c2[2],c3[2],c4[2],c5[2]),
  c(c1[3],c2[3],c3[3],c4[3],c5[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1]),
  c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2]),
  c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1]),
  c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2]),
  c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1]),
  c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2]),
  c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3]),maxColorValue=255),
rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1],c9[1]),
  c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2],c9[2]),
  c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3],c9[3]),maxColorValue=255),
),
  Set2 =  switch(n,
rgb(c(154),
  c(62),
  c(37),maxColorValue=255),
rgb(c(154,21),
  c(62,107),
  c(37,144),maxColorValue=255),
rgb(c(154,21,112),
  c(62,107,130),
  c(37,144,89),maxColorValue=255)
)
)
}


pal_name <- function(palette, type) {
if (is.character(palette)) {
  if (!palette %in% RColorBrewer:::namelist) {
    warning("Unknown palette ", palette)
    palette <- "Set1"
  }
  return(palette)
}

switch(type,
  div = divlist,
  qual = quallist,
  seq = seqlist,
  stop("Unknown palette type. Should be 'div', 'qual' or 'seq'",
    call. = FALSE)
)[palette]
}

custom_pal <- function(type = "seq", palette = 1) {
pal <- pal_name(palette, type)

function(n) {
  if (n < 3)
    suppressWarnings(custom.pal(n, pal))[seq_len(n)]
  else
    custom.pal(n, pal)[seq_len(n)]
}
}

scale_colour_custom <- function(..., type = "seq", palette = 1) {
discrete_scale("colour", "custom", custom_pal(type, palette), ...)
}

#' @export
#' @rdname scale_custom
scale_fill_custom <- function(..., type = "seq", palette = 1) {
discrete_scale("fill", "custom", custom_pal(type, palette), ...)
}

library(extrafont)
library(tidyverse)
library(ggrepel)

# wrapper logic to source correct theme-----------------------------------------
if (.Platform$OS.type == 'windows') {
  message('Sourcing for Windows...')
  windows.options(width = 8.33333333333333, height = 5.55555555555556)
  windowsFonts(Lato = windowsFont("Lato"))
  } else {
  message('Sourcing for Mac/Linux...')
  quartz.options(width = 8.33333333333333, height = 5.55555555555556, dpi = 72)
  }

# create new complete ggplot2 theme ---------------------------------------

theme_urban <- function(base_size = 12L, 
                        base_family = "Lato",
                        base_line_size = base_size / 24L,
                        base_rect_size = base_size / 24L) {
  
  half_line <- base_size / 2L
  
  theme(
    
    # main attributes
    
    line = element_line(colour = "#000000", 
                        size = base_line_size, 
                        linetype = 1L, 
                        lineend = "butt"), 
    rect = element_rect(fill = "#FFFFFF", 
                        colour = "#000000", 
                        size = base_rect_size, 
                        linetype = 1L), 
    text = element_text(family = base_family, 
                        face = "plain", 
                        colour = "#000000", 
                        size = base_size, 
                        hjust = 0.5, 
                        vjust = 0.5, 
                        angle = 0, 
                        lineheight = 0.9, 
                        margin = margin(),
                        debug = FALSE),
    
    # Plot Attributes
    
    plot.title = element_text(size = base_size * 1.5,
                              hjust = 0L,
                              vjust = 0L,
                              margin = margin(b = 8L)), 
    plot.subtitle = element_text(size = base_size * 7L / 6L,
                                 hjust = 0L,
                                 vjust = 0L,
                                 margin = margin(b = 10L)),
    plot.caption = element_text(size = base_size * 2L / 3L,
                                hjust = 1L,
                                vjust = 1L,
                                margin = margin(t = half_line * 0.9)),
    plot.background = NULL, 
    
    plot.margin = margin(t = 10L, r = 10L, b = 10L, l = 10L), 
    
    # axis attributes
    
    axis.text = element_text(size = base_size),
    axis.text.x = element_text(margin = margin(t = 4L)),
    axis.text.y = NULL, 
    axis.text.x.top = NULL, 
    axis.text.y.right = NULL, 
    
    axis.title = element_text(face = "italic", 
                              size = base_size), 
    axis.title.x = element_text(margin = margin(t = 8L)), 
    axis.title.y = element_text(angle = 90L, 
                                margin = margin(r = 4L)),
    axis.title.x.top = NULL, 
    axis.title.y.right = NULL,
    
    axis.ticks = element_line(),     
    axis.ticks.length = unit(4L, "pt"),
    axis.ticks.x = element_line(colour = NULL, 
                                size = NULL, 
                                linetype = NULL, 
                                lineend = NULL), 
    axis.ticks.y = element_blank(), 
    
    axis.line = element_line(), 
    axis.line.x = element_line(colour = NULL, 
                               size = NULL, 
                               linetype = NULL, 
                               lineend = NULL), 
    axis.line.y = element_blank(), 
    
    # legend attributes
    
    legend.background = element_blank(), 
    
    legend.spacing = unit(20L, "pt"), 
    legend.spacing.x = NULL, 
    legend.spacing.y = NULL,
    
    legend.key = element_blank(), 
    legend.key.size = unit(10L, "pt"), 
    legend.key.height = NULL, 
    legend.key.width = NULL, 
    
    legend.text = NULL, 
    legend.text.align = NULL, 
    legend.title = element_blank(), 
    legend.title.align = NULL, 
    
    legend.position = "top", 
    legend.direction = "horizontal", 
    legend.justification = NULL, 
    legend.margin = margin(t = 6L, r = 0L, b = 6L, l = 0L, "pt"), 
    
    legend.box = "horizontal", 
    legend.box.margin = NULL, 
    legend.box.background = NULL, 
    legend.box.spacing = NULL, 
    
    # panel attributes
    
    panel.background = element_blank(), 
    panel.border = element_blank(), 
    panel.ontop = FALSE, 
    
    panel.spacing = unit(6L, "pt"),
    panel.spacing.x = NULL, 
    panel.spacing.y = NULL, 
    
    panel.grid = NULL,
    panel.grid.major = element_line(), 
    panel.grid.major.x = element_blank(), 
    panel.grid.major.y = element_line(colour = "#dedddd"), 
    panel.grid.minor = element_line(), 
    panel.grid.minor.x = element_blank(), 
    panel.grid.minor.y = element_blank(),
    
    # strip attributes (Faceting)
    
    strip.background = element_rect(fill = "#dedddd", 
                                    colour = NA,
                                    size = 10L), 
    strip.text = element_text(face = "bold", 
                              size = base_size * (2 / 3),
                              margin = margin(t = 0L, r = 0L, b = 0L, l = 0L)),
    
    strip.text.x = element_text(margin = margin(t = 4.5, b = 4.5)), 
    strip.text.y = element_text(angle = -90L, 
                                margin = margin(l = 4.5, r = 4.5)), 
    
    strip.placement = "inside",
    strip.placement.x =  NULL,
    strip.placement.y =  NULL,
    
    strip.switch.pad.grid = unit(0.1, "cm"), 
    strip.switch.pad.wrap = unit(0.1, "cm"), 
    
    # create a complete format
    complete = TRUE
    
  )
}

theme_set(theme_urban())


# add lato font to text and label geoms ---------------------------

update_geom_defaults("text", list(family = "Lato"))
update_geom_defaults("label", list(family = "Lato"))
update_geom_defaults("text_repel", list(family = "Lato"))
update_geom_defaults("label_repel", list(family = "Lato"))

# set default colours for monochromatic plots -----------------------------

update_geom_defaults("bar", list(fill = "#1696d2"))
update_geom_defaults("point", list(colour = "#1696d2"))
update_geom_defaults("line", list(colour = "#1696d2"))
update_geom_defaults("boxplot", list(fill = "#1696d2"))
update_geom_defaults("density", list(fill = "#1696d2"))
update_geom_defaults("violin", list(fill = "#1696d2"))

# set default colors for continuous color scales --------------------------

scale_colour_gradientn <- function(..., 
                                   colours = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"), 
                                   colors = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                   values = NULL, 
                                   space = "Lab", 
                                   na.value = "grey50", 
                                   guide = "colourbar") {
  
  colours <- if (missing(colours)) colors else colours
  
  continuous_scale("colour", "gradientn",
                   scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

scale_color_gradientn <- scale_colour_gradientn

scale_fill_gradientn <- function(..., 
                                 colours = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"), 
                                 colors = c("#CFE8F3","#A2D4EC","#73BFE2","#46ABDB", "#1696D2","#12719E","#0A4C6A","#062635"),
                                 values = NULL, 
                                 space = "Lab", 
                                 na.value = "grey50", 
                                 guide = "colourbar") {
  
  colours <- if (missing(colours)) colors else colours
  
  continuous_scale("fill", "gradientn",
                   scales::gradient_n_pal(colours, values, space), na.value = na.value, guide = guide, ...)
}

# set default colors for discrete color scales ----------------------------

# redefine default discrete colours, up to 9 colours.
scale_colour_discrete <- function(...) scale_colour_custom(..., palette = "Set1")
scale_fill_discrete <- function(...) scale_fill_custom(... , palette = "Set1")

.divlist <- c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")
.quallist <- c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3")
.seqlist <- c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd",
              "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd")

.divnum <- rep(11, length(.divlist))
.qualnum <- c(8, 8, 12, 9, 8, 9, 8, 12)
.seqnum <- rep(9, length(.seqlist))

.namelist <- c(.divlist, .quallist, .seqlist)
.maxcolours <- c(.divnum, .qualnum, .seqnum)
.catlist <- rep(c("div", "qual", "seq"), c(length(.divlist), length(.quallist), length(.seqlist)))

.custom.pal.info <- data.frame(maxcolours = .maxcolours, category = .catlist, row.names = .namelist)

.custom.pal <- function(n, name){
  if (!(name %in% .namelist)) {
    stop(paste(name, "is not a valid palette name for .custom.pal\n"))
  }
  
  if (n < 3) {
    warning("minimal value for n is 3, returning requested palette with 3 different levels\n")
    return(.custom.pal(3, name))
  }
  
  if (n > .maxcolours[which(name == .namelist)]) {
    warning(paste("n too large, allowed maximum for palette",name,"is", .maxcolours[which(name == .namelist)]),
            "\nReturning the palette you asked for with that many colours\n")
    return(.custom.pal(.maxcolours[which(name == .namelist)], name))
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
                            c(c1[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1]),
                            c(c1[2],c2[2]),
                            c(c1[3],c2[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1]),
                            c(c1[2],c2[2],c3[2]),
                            c(c1[3],c2[3],c3[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1]),
                            c(c1[2],c2[2],c3[2],c4[2]),
                            c(c1[3],c2[3],c3[3],c4[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1]),
                            c(c1[2],c2[2],c3[2],c4[2],c5[2]),
                            c(c1[3],c2[3],c3[3],c4[3],c5[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1]),
                            c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2]),
                            c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1]),
                            c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2]),
                            c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1]),
                            c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2]),
                            c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3]), maxColorValue = 255),
                        rgb(c(c1[1],c2[1],c3[1],c4[1],c5[1],c6[1],c7[1],c8[1],c9[1]),
                            c(c1[2],c2[2],c3[2],c4[2],c5[2],c6[2],c7[2],c8[2],c9[2]),
                            c(c1[3],c2[3],c3[3],c4[3],c5[3],c6[3],c7[3],c8[3],c9[3]), maxColorValue = 255),
         ),
         
         Set2 = switch(n,
                       rgb(c(154),
                           c(62),
                           c(37), maxColorValue = 255),
                       rgb(c(154,21),
                           c(62,107),
                           c(37,144), maxColorValue = 255),
                       rgb(c(154,21,112),
                           c(62,107,130),
                           c(37,144,89), maxColorValue = 255)
         )
  )
}

.pal_name <- function(palette, type) {
  if (is.character(palette)) {
    if (!palette %in% RColorBrewer:::namelist) {
      warning("Unknown palette ", palette)
      palette <- "Set1"
    }
    return(palette)
  }
  
  switch(type,
         div = .divlist,
         qual = .quallist,
         seq = .seqlist,
         stop("Unknown palette type. Should be 'div', 'qual' or 'seq'",
              call. = FALSE)
  )[palette]
}

.custom_pal <- function(type = "seq", palette = 1) {
  pal <- .pal_name(palette, type)
  
  function(n) {
    if (n < 3 | length(n) > 1)
      suppressWarnings(.custom.pal(n, pal))[seq_len(n)]
    else
      .custom.pal(n, pal)[seq_len(n)]
  }
}

scale_colour_custom <- function(..., type = "seq", palette = 1) {
  discrete_scale("colour", "custom", .custom_pal(type, palette), ...)
}

#' @export
#' @rdname scale_custom
scale_fill_custom <- function(..., type = "seq", palette = 1) {
  discrete_scale("fill", "custom", .custom_pal(type, palette), ...)
}

# Urban Institute ggplot2 theme map add-on --------------------------------
urban_map <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_blank()
)
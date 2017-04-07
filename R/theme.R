# #----------------------------------------------------------------------------#

# # Purpose:     Specify themes
# # Project:     Cowbell
# # Author:      Clara Marquardt
# # Date:        Sept 2016
# # Notes:       /        

# #----------------------------------------------------------------------------#

# #----------------------------------------------------------------------------#
# #                                    Code                                    #
# #----------------------------------------------------------------------------#

# # themes
# #----------------------------------------------------------------------------#

# # general settings
# #-------------------------------------------------#
# font_gen <- "URWHelvetica"
# col_gen <- "grey50"

# # specific themes
# #-------------------------------------------------#
# theme_basic <- function(axis_size=0.5, title_size=8, subtitle_size=6) {
#   theme_bw() +
#   theme(
#   axis.text.x = element_text(size=rel(axis_size), colour = col_gen,
#     family=font_gen),
#   axis.text.y = element_text(size=rel(axis_size), colour = col_gen,
#     family=font_gen), 
#   axis.title.x = element_text(size=rel(axis_size), colour = col_gen,
#   family=font_gen),
#     axis.title.y = element_text(size=rel(axis_size), colour = col_gen,
#   family=font_gen),
#   plot.title = element_text(size = title_size, colour = col_gen, face = "bold",
#     family=font_gen),
#   plot.subtitle = element_text(size = subtitle_size, colour = col_gen, 
#     face = "plain",family=font_gen),
#   plot.caption = element_text(size = (subtitle_size-1), colour = col_gen, 
#     face = "plain",family=font_gen),
#   )
# }

# theme_legend_bottom <- function(title_size=0.5, text_size=0.4, tick_size=0.08,
#   legend_width=0.5, legend_height=0.2, hjust_title=0.5) { 
#   theme(
#    legend.position="bottom", 
#    legend.key.height=unit(legend_height,"cm"),
#    legend.key.width=unit(legend_width,"cm"),
#    axis.ticks.length=unit(tick_size,"cm"),
#    legend.title=element_text(size=rel(title_size), colour=col_gen, family=font_gen, 
#       hjust=hjust_title, face="plain"),
#    legend.text=element_text(size=rel(text_size), colour=col_gen, family=font_gen)
#   )
# }

# theme_remove_all <- theme(axis.text = element_blank(),
#   axis.title = element_blank(),
#   axis.ticks =  element_blank(),
#   # axis.text = element_text(margin=margin(0,0,0,0, "lines")),
#   axis.ticks.length = unit(0, "cm"), 
#   panel.background = element_blank())

# # colour schemes
# #----------------------------------------------------------------------------#

# #----------------------------------------------------------------------------#
# #                                    End                                     #
# #----------------------------------------------------------------------------#

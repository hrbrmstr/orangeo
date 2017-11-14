library(rtweet)
library(hrbrthemes)
library(stringi)
library(gridExtra)
library(viridis)
library(rprojroot)
library(tidyverse)

# This is a custom "tableGrob()" theme
ttheme_rc <- function (base_size = 8, base_colour = "#2b2b2b", base_family = hrbrthemes::font_rc,
                       parse = FALSE, padding = unit(c(2, 2), "mm"), ...) {

  core <- list(
    fg_fun = gridExtra:::text_grob,
    fg_params = list(
      parse = parse, col = base_colour, fontsize = base_size,
      fontfamily = base_family
    ),
    bg_fun = gridExtra:::rect_grob,
    bg_params = list(fill = "white", col = NA),
    padding = padding
  )
  colhead <- list(
    fg_fun = gridExtra:::text_grob,
    fg_params = list(
      parse = parse,  col = base_colour, fontface = 2L,
      fontsize = base_size, fontfamily = base_family),
    bg_fun = gridExtra:::rect_grob,
    bg_params = list(fill = NA,col = NA), padding = padding
  )
  rowhead <- list(
    fg_fun = gridExtra:::text_grob,
    fg_params = list(
      parse = parse, col = base_colour, fontface = 3L,
      fontsize = base_size, fontfamily = base_family, hjust = 1, x = 0.95
    ),
    bg_fun = gridExtra:::rect_grob,
    bg_params = list(fill = NA, col = NA), padding = padding
  )

  default <- list(core = core, colhead = colhead, rowhead = rowhead)

  modifyList(default, list(...))

}

rt <- rprojroot::find_rstudio_root_file()
orange_dat_file <- file.path(rt, "data", "orange.rds")

if (!file.exists(orange_dat_file)) {
  orange <- get_timeline("realDonaldTrump", n=3000) # setting up rtweet is an exercise left to the reader
  write_rds(orange, orange_dat_file)
} else {
  orange <- read_rds(orange_dat_file)
}

# this is a shortcut function to make it easier to play with text widths
# in long ggplot2 subtitles (or ggplot2 text in-general)
forsub <- function(x, width = 100) {
  paste0(stri_wrap(x, width = width, simplify = TRUE), collapse="\n")
}

# need the data by day and we don't care about non-geotagged tweets
mutate(orange, day=as.Date(created_at)) %>%
  count(day, place_full_name) %>%
  filter(!is.na(place_full_name)) %>%
  arrange(day) -> orange_places

# now we need to find the tweets that are geotagged recently
# we'll build the annotation table from this
filter(orange, created_at > as.Date("2017-10-01")) %>%
  filter(!is.na(place_full_name)) %>%
  mutate(day=as.Date(created_at)) %>%
  count(day, place_full_name) %>%
  select("Day"=1, "Place"=2, "n"=3) %>%
  arrange(Day) %>%
  tableGrob(rows=NULL, theme=ttheme_rc()) -> gg

# and here we are building said annotation table
annotation_custom(
  gg,
  xmin = as.numeric(as.Date("2017-06-01")),
  xmax = as.numeric(as.Date("2017-11-01")),
  ymin = 11,
  ymax = 42
) -> gann

# these are for the arrows. I could have made them one dat frame
# with a factor to separate them but I made them at separate times
# and they're just as easy to manage this way.
data_frame(
  x = as.Date(c("2017-11-05", "2017-09-01")),
  y = c(4, 15)
) -> lann1

data_frame(
  x = as.Date(c("2017-01-01", "2016-11-01")),
  y = c(25, 11)
) -> lann2

# annotation labels. It's my personal preference to use
# geom_text/geom_label vs annotation functions things get tricky
# with facets either way but there are no facets here so s'all gd
data_frame(
  x = as.Date(c("2017-01-02", "2017-09-01")),
  y = c(25, 13),
  label = c("Trump's campaign travel", "(He's been touring Asia during this time) "),
  face = c("plain", "italic"),
  hjust=c(0, 1)
) -> tann1

# I'm keeping this despite not needing it. I had originally wanted to do
# a bar chart annotation layer but there are too many factor levels and I
# didn't want to write some custom color code just for this exercise so
# we've got a table instead. This may come in handy for future projects, tho.
plt_df <- mutate(orange_places, place_full_name = factor(place_full_name, levels=unique(orange_places$place_full_name)))

# the graph part is easy now
# saving it at 1000x500 keeps the annotations adjusted well
ggplot() +
  geom_col(data=plt_df, aes(day, n, group=place_full_name, fill=place_full_name), position="stack") +
  geom_path(data=lann1, aes(x, y), color="#b2b2b2", arrow = arrow(length = unit(4, "pt"))) +
  geom_path(data=lann2, aes(x, y), color="#b2b2b2", arrow = arrow(length = unit(4, "pt"))) +
  geom_text(data=tann1, aes(x, y, label=label, hjust=hjust, fontface=face), size=3, family=font_rc) +
  gann +
  scale_y_comma(limits=c(0,45)) +
  labs(
    x=NULL, y="Tweet count",
    title="Geolocated Trump Tweets",
    subtitle=forsub("During his campaign, had *tons* of location data. It's likely his POTUS cybersecurity handlers forced him to disable location sharing on his devices. It's interesting to see how some of his POTUS Twitter account handlers have setup their devices incorrectly."),
    caption="NOTE: Only showing tweets with location data"
  ) +
  theme_ipsum_rc(grid="Y") +
  theme(legend.position="none")


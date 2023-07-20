library(Rcpp)
library(cairocore)
library(cairobasic)


# parameters
seed <- 39

sys_name <- "boxes"
sys_id <- 4 
fname <- paste0(sys_name, "_", sys_id, "_", seed, ".png")
fpath <- here::here("image", fname)

sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp"))) 


# fixed
layers <- 25
scheme <- seed
iter <- 10000000
million <- 1000000
adjust <- function(x) {x}
transparency <- "60"
filter_x <- c(-2,2)
filter_y <- c(-2,2)
brd <- 0
col_trans <- rank
bg <- "grey10"

# palette from scheme/seed
pl <- switch(
  as.character(scheme),
  "19" = "gameofthrones::baratheon",
  "20" = "gameofthrones::lannister",
  "21" = "gameofthrones::stark",
  "22" = "gameofthrones::martell",
  "23" = "gameofthrones::tyrell",
  "24" = "gameofthrones::greyjoy",
  "25" = "grDevices::Purples",
  "26" = "grDevices::Oranges",
  "27" = "gameofthrones::white_walkers",
  "28" = "ggthemes::Sunset-Sunrise Diverging",
  "29" = "viridis::viridis",
  "30" = "viridis::magma",
  "31" = "grDevices::rainbow",
  "32" = "grDevices::PuRd",
  "33" = "scico::tokyo",
  "34" = "scico::bamako",
  "35" = "ggthemes::Gold-Purple Diverging",
  "36" = "scico::lajolla",
  "37" = "grDevices::Burg",
  "38" = "grDevices::Peach",
  "39" = "grDevices::Purple-Blue"
)

if(scheme == 37) layers <- 200
if(scheme == 38) layers <- 200
if(scheme == 39) {
  layers <- 10; 
  adjust <- function(x) adjustcolor(x, 1, 1.5, 1.5, 1.5)
  bg <- "ghostwhite";
  transparency <- "ff"
  iter <- 20*million
}

#   pl <- "scico::grayC"
#   pl <- "scico::oslo"
#   pl <- "scico::bilbao"
#   pl <- "grDevices::TealRose"
#   pl <- "scico::bamako"  
#   pl <- "scico::berlin"  
#   pl <- "scico::batlow"


cat("seed", seed, "\n")
set.seed(seed)

cat("generating...\n")

df <- boxes(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c", "l", "v")
df <- df[-(1:100),]

# filter observations outside the range
if(!is.null(filter_x)) {
  x_ok <- df$x > filter_x[1] & df$x < filter_x[2] 
  y_ok <- df$y > filter_y[1] & df$y < filter_y[2]
  df <- df[x_ok & y_ok, ]
#  df$c[df$c < -1] <- -1
#  df$c[df$c > 1] <- 1
}

if(!is.null(col_trans)){
  df$c <- col_trans(df$c)
}


# Manually scale the co-ordinates to the image size
px <- 5000
xrng <- max(df[,1]) - min(df[,1])
yrng <- max(df[,2]) - min(df[,2])
rng <- max(c(xrng, yrng))

xdiff <- max(c(yrng - xrng, 0))/2
ydiff <- max(c(xrng - yrng, 0))/2

df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (px - 2*brd)
df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (px - 2*brd)


# Manually create a vector of colour
cc <- 4
col_idx <- as.integer((df[,cc] - min(df[,cc])) / (max(df[,cc]) - min(df[,cc])) * 255) + 1L
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- adjust(pal)
pal <- gsub("FF$", transparency, pal)
col <- pal[col_idx]

cat("rendering...\n")

cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r = 3, fill = col, colour = NA)
cb$write_png(fpath)

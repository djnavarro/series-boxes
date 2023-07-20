library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_d.cpp"))

# parameters
seed <- 26
layers <- 100
scheme <- 26

# fixed
iter <- 10000000
adjust <- function(x) {x}
transparency <- "30"
filter_x <- NULL
filter_y <- NULL
brd <- 100
col_trans <- NULL


if(scheme == 1) {
  bg <- "white"
  pl <- "scico::grayC"
}
if(scheme == 2) {
  bg <- "hotpink4"
  pl <- "ggthemes::Gold-Purple Diverging"
}
if(scheme == 3) {
  bg <- "ghostwhite"
  pl <- "scico::oslo"
}
if(scheme == 4) {
  bg <- "bisque"
  pl <- "scico::bilbao"
}
if(scheme == 5) {
  bg <- "slateblue4"
  pl <- "grDevices::TealRose"
}
if(scheme == 6) {
  bg <- "black"
  pl <- "viridis::viridis"
}
if(scheme == 7) {
  bg <- "azure"
  pl <- "viridis::magma"
}
if(scheme == 8) {
  bg <- "lavender"
  pl <- "scico::tokyo"
}
if(scheme == 9) {
  bg <- "white"
  pl <- "viridis::magma"
}
if(scheme == 10) {
  bg <- "lemonchiffon3"
  pl <- "scico::bamako"  
}
if(scheme == 11) {
  bg <- "grey60"
  pl <- "scico::berlin"  
}
if(scheme == 12) {
  bg <- "grey20"
  pl <- "scico::lajolla"  
}
if(scheme == 13) {
  bg <- "midnightblue"
  pl <- "ggthemes::Sunset-Sunrise Diverging"
}
if(scheme == 14) {
  bg <- "mediumpurple4"
  pl <- "grDevices::PuRd"
}
if(scheme == 15) {
  bg <- "mediumpurple4"
  pl <- "grDevices::Purples"
}
if(scheme == 16) {
  bg <- "#612B21"
  pl <- "scico::batlow"
}
if(scheme == 17) {
  bg <- "grey10"
  pl <- "grDevices::Purple-Blue"
}
if(scheme == 0) {
  bg <- "grey10"
  pl <- "scico::grayC"
  adjust <- function(x) {
    x <- rep("#FFFFFFFF", length(x))
    #x <- x[length(x):1]
    return(x)
  }
  transparency <- "20"
}
if(scheme == 18) {
  bg <- "grey10"
  pl <- "gameofthrones::baratheon"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 19) {
  bg <- "grey10"
  pl <- "gameofthrones::lannister"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 20) {
  bg <- "grey10"
  pl <- "gameofthrones::stark"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 21) {
  bg <- "grey10"
  pl <- "gameofthrones::martell"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 22) {
  bg <- "grey10"
  pl <- "gameofthrones::tyrell"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}

if(scheme == 23) {
  bg <- "grey10"
  pl <- "gameofthrones::greyjoy"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 24) {
  bg <- "grey10"
  pl <- "gameofthrones::white_walkers"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 25) {
  bg <- "grey10"
  pl <- "grDevices::Purples"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}
if(scheme == 26) {
  bg <- "grey10"
  pl <- "grDevices::Oranges"
  filter_x <- c(-2,2)
  filter_y <- c(-2,2)
  brd <- 0
  col_trans <- rank
}


set.seed(seed)

cat("generating...\n")

df <- flame(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]

# filter observations outside the range
if(!is.null(filter_x)) {
  keep <- df$y > filter_y[1] & df$y < filter_y[2] & 
    df$x > filter_x[1] & df$x < filter_x[2]
  df <- df[keep, ]
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


# Manually create a vector of colours
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
pal <- paletteer::paletteer_c(palette = pl, n = 256)
pal <- adjust(pal)
pal <- gsub("FF$", transparency, pal)
col <- pal[col_idx]

fname <- paste0("ff_05_", seed, "_", layers, "_", scheme, ".png")
fpath <- here::here("image", fname)

cat("rendering...\n")

cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
cb$add_circles(x=df[,1], y = df[,2], r = 3, fill = col, colour = NA)
cb$write_png(fpath)

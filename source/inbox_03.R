library(Rcpp)
library(cairocore)
library(cairobasic)

sourceCpp(here::here("source", "ff_d.cpp"))

# parameters
seed <- 10

# fixed
prefix <- "box_03_"
layers <- 100
scheme <- seed
iter <- 10000000
adjust <- function(x) {x}
f <- 2
filter_x <- c(-f,f)
filter_y <- c(-f,f)
lw <- 3
brd <- 0
col_trans <- rank
bg <- "grey10"
pf <- function(n, palette, alpha = 1) {
  cols <- paletteer::paletteer_c(palette = palette, n = n)
  cols <- adjustcolor(cols, alpha.f = alpha)
  return(cols)
}

# palette function from scheme/seed
pal_fun <- switch(
  as.character(scheme),
  "1" = function(n) {scico::scico(n, palette = "bilbao")},
  "2" = function(n) {c(viridis::viridis(n/2, alpha = .3), viridis::magma(n/2, alpha = .3))},
  "3" = function(n) {c(viridis::viridis(n/2, alpha = .5), rep("#ffffff22", n/2))},
  "4" = function(n) {c(rainbow(n/2, s = .4, v = .4, alpha = 0), rep("#ffffff22", n/2))},
  "5" = function(n) {c(rep("#ff000022", n/2), rep("#ffff0022", n/2))},
  "6" = function(n) {f <- jasmines::palette_named("base"); c(f(31 * n/32, alpha = .02), f(n/32, alpha = 1))},
  "7" = function(n) {c(scico::scico(n/4, alpha = 1, palette = "oleron"), rep("#1a1a1aff", 3*n/4))},
  "8" = function(n) {c(rep("#000000ff", 4*n/8), scico::scico(3*n/8, palette = "imola"), scico::scico(n/8, alpha = 1, palette = "lajolla"))},
  "9" = function(n) {c(rep("#000000ff", 5*n/8), pf(n/8, "grDevices::OrYel"), pf(n/4, "grDevices::Reds"))},
  "10" = function(n) {c(rep("#000000ff", 5*n/8), pf(n/8, "grDevices::PuRd"), pf(n/4, "grDevices::PuRd"))},
  "x" = NULL
)

if(scheme == 9) lw <- 1

# construct the palette
pal <- pal_fun(256)
pal <- adjust(pal)
#bg <- adjustcolor(pal[128], 1, .5, .5, .5)

cat("seed", seed, "\n")
set.seed(seed)

cat("generating...\n")

df <- flame(iter, layers)
df <- as.data.frame(df)
names(df) <- c("x","y","c")
df <- df[-(1:100),]

# filter spatial observations outside the range
y_ok <- df$y > filter_y[1] & df$y < filter_y[2]
x_ok <- df$x > filter_x[1] & df$x < filter_x[2]
df <- df[y_ok & x_ok, ]

# apply colour transformation 
df$c <- col_trans(df$c)


# manually scale the co-ordinates to the image size
px <- 5000
xrng <- max(df[,1]) - min(df[,1])
yrng <- max(df[,2]) - min(df[,2])
rng <- max(c(xrng, yrng))

xdiff <- max(c(yrng - xrng, 0))/2
ydiff <- max(c(xrng - yrng, 0))/2

df[,1] <- brd + (df[,1] - min(df[,1]) + xdiff) / rng * (px - 2*brd)
df[,2] <- brd + (df[,2] - min(df[,2]) + ydiff) / rng * (px - 2*brd)


# manually create a vector of colours
col_idx <- as.integer((df[,3] - min(df[,3])) / (max(df[,3]) - min(df[,3])) * 255) + 1L
col <- pal[col_idx]

fname <- paste0(prefix, seed, ".png")
fpath <- here::here("image", fname)

# write the image
cat("rendering...\n")
cb <- cairobasic::CairoBasic$new(width = px, height = px, bg = bg, antialias = TRUE)
cb$add_line_segments(
  x1 = df$x, 
  y1 = df$y, 
  x2 = df$x + rnorm(length(df$x), 0, 10), 
  y2 = df$y + rnorm(length(df$y), 0, 10), 
  linewidth = lw, 
  colour = col
)
cb$write_png(fpath)

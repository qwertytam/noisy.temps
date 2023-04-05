### Environment set up and package installs
## Note, need `library(package_name)` and restart R for `renv` to pick up
## change in environment
# renv::init()
# renv::install("tidyverse")
# renv::install("devtools")
# renv::install("extrafont")
# devtools::install_github("tylermorganwall/rayshader")


### Load libraries
library(tidyverse)
library(usethis)
library(devtools)
library(extrafont)
library(rayshader)


### Function Definitions
#### Calculate temperature height matrix
create_mat <- function(outside_radius, inside_radius, temps){

  # Create empty matrix
  mat_dim <- outside_radius*2.05
  mat <- matrix(NA, nrow=mat_dim, ncol=mat_dim)
  
  # Origin or mid-point for rectangular coordinates
  x0 <- mat_dim / 2
  y0 <- mat_dim / 2
  
  # Calculate mid-point offset for rectangular coordinates
  offset0 <- 0.5
  if(mat_dim %% 2 == 0) { 
    offset0 <- 0
  }
  
  temp_len <- length(temps)
  base_lvl <- min(temps)
  z_scaler <- 100
  
  # Iterate through each matrix point to set the z point
  for (x in seq(1, mat_dim, 1)) {
    for (y in seq(1, mat_dim, 1)) {
      x_rct <- x - x0 - offset0
      y_rct <- y - y0 - offset0
      
      # Length of hypotenuse to x, y from matrix mid-point
      xyl <- sqrt(x_rct^2 + y_rct^2)
      
      # If point is in the circle rim, then set z
      if (xyl >= inside_radius & xyl <= outside_radius) {
        theta <- atan2(y_rct, x_rct) + pi  # add pi so on scale 0 to 2pi
        theta <- 2*pi - theta  # invert so temps go clockwise
        temp_num <- round(theta/(2*pi) * (temp_len - 1), 0) + 1
        z <- (temps[temp_num] - base_lvl) * z_scaler
        mat[x, y] <- z
      }
    }
  }
  return(mat)
}

#### Convert height matrix into data frame
df_from_mat <- function(mat){
  rownames(mat) <- seq(1, dim(mat)[1], 1)
  colnames(mat) <- seq(1, dim(mat)[1], 1)

  df_out <- mat %>%
    as.table(.) %>%
    as.data.frame(.) %>%
    rename(all_of(c(X = "Var1", Y = "Var2", Z = "Freq"))) %>%
    mutate(X = as.integer(X),
           Y = as.integer(Y))
  
  return(df_out)
}


#### Draw matrix using rayshader::sphere_shade()
draw_sphere_shade <- function(mat, map_text){
  mat %>%
    sphere_shade(sunangle = 10,
                 colorintensity=10,
                 texture=map_text)  %>%
    plot_3d(mat,
            zscale = 1, fov = 0, theta = 135, zoom = 0.75, phi = 45,
            water = TRUE,
            waterdepth = 1,
            watercolor = "white",
            wateralpha = 1,
            soil = FALSE,
            background = "white",
            windowsize = c(800, 800))
  Sys.sleep(0.2)
  render_snapshot()
}


### Plot textures and colour spaces
#### Create texture for sphere shading
# map_texture <- create_texture(
#   lightcolor = "red", # main high light color, top center of the map
#   shadowcolor = "blue", # main shadow color, bottom center of the map
#   leftcolor = "pink", # left fill color, left center of the map
#   rightcolor = "lightblue",
#   centercolor ="white" # center of the map, represents flat areas
# )
# plot_map(map_texture)

#### Colorspace for ggplot
colorspace::diverging_hcl(n = 7,
                          h = c(260, 7), c = 100, l = c(37, 100), power = 1.2,
                          register = "temp-wheel")
tw_cs <- colorspace::diverging_hcl(7, palette = "temp-wheel")
# par(mfrow = c(1, 2))
# colorspace::swatchplot(tw_cs)
# colorspace::hclplot(tw_cs)


### Gather data
fp <- "data"
fn <- "HadCRUT.5.0.1.0.analysis.summary_series.global.annual.csv"
url <- paste(
  "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/",
  fn, sep="")
download.file(url, file.path(fp, fn))

df_temps <- file.path(fp, fn) %>%
  read_csv(col_types="in__") %>% # integer, numeric, skip x 2
  select(1:2) %>%
  rename(yr = Time) %>%
  rename(temp = starts_with("Ano"))

# Calculate temperature height matrix
base_rad <- 100
base_rad_w <- 20
scaler <- 0.5 #9.99
mat_temps <- create_mat(base_rad * scaler,
                   (base_rad - base_rad_w) * scaler,
                   df_temps$temp)

# # Draw sphere shade
# par(mfrow = c(1, 1))
# draw_sphere_shade(mat_temps, map_texture)

# Convert matrix into data frame for ggplot
df_temps_plt <- df_from_mat(mat_temps)

# Draw ggplot and rayshader version
white_transp <- scales::alpha("white", 0)

p <- df_temps_plt %>% ggplot() +
  geom_tile(aes(x = X, y = Y, fill = Z), show.legend = FALSE) +
  # Scales
  scale_x_continuous(expand = expansion(mult = 0.1, add = 0), labels=NULL) +
  scale_y_continuous(expand = expansion(mult = 0.1, add = 0), labels=NULL) +
  scale_fill_gradientn("Z",
                       colors = tw_cs,
                       guide="none",
                       na.value="white",
                       ) +
  coord_fixed() +
  # Styling
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_line(colour = "white"),
    panel.background=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank()
    ) +
  xlab(element_blank()) +
  ylab(element_blank())

p

hm <- plot_gg(p,
        # plot_gg args
        # width = 6, height = 6, scale = 300,
        shadow = FALSE,
        # shadowdepth = 0,
        # shadowcolor = "greenwhite",
        multicore = TRUE,
        lambert = TRUE,
        raytrace = TRUE,
        triangulate = TRUE,
        sunangle = 180,
        anglebreaks = seq(30, 65, 0.1),
        # triangulate = FALSE,
        save_height_matrix = TRUE,
        # plot_3d args
        # baseshape = "circle",
        solid = FALSE,
        # solidcolor = "white",
        # solidlinecolor = "white",
        water = FALSE,
        # waterdepth = 0.01,
        # watercolor = "green",
        # wateralpha = 0.99,
        theta = 90,
        phi = 45,
        zoom = 0.6,
        windowsize = 600)

render_label(hm,
             text = "Santa Cruz2",
             x = 450, y = 450,
             # extent = c(0, dim(mat_temps)[1], 0, dim(mat_temps)[1]),
             altitude=100,
             # zscale=50,
             freetype = FALSE,
             family="Georgia",
             textalpha = 0.5,
             clear_previous = TRUE,
             relativez = FALSE,
             textcolor = "black",
            )

Sys.sleep(0.2)
render_snapshot(clear = TRUE)

# For MacOS, see if X11 is available
x11()

# Fonts available with extrafont
# Vector of font family names
extrafont::fonts()

# Show entire table
extrafont::fonttable()

# Import available fonts
extrafont::font_import()

# See list of available 3d fonts
rgl::par3d()
names(X11Fonts())


# Try installing fonts
# install.packages("rgl", type="source")

# First using rglFonts
rgl::rglFonts(belltopo = c(
  system.file("fonts/BellTopoSans-Regular.otf", package = "rgl"),
  system.file("fonts/BellTopoSans-Bold.otf", package = "rgl"),
  system.file("fonts/BellTopoSans-Italic.otf", package = "rgl"),
  system.file("fonts/BellTopoSans-BoldItalic.otf", package = "rgl")))

# And try using rglExtrafonts
newfamily <- rgl::rglExtrafonts(newsans = c("Comic Sans MS", "Impact", 
                                       "Verdana", "Tahoma"))

# Test fonts
rgl::open3d()
rgl::text3d(1,1,1, "Default", family = "sans", cex = 2)  
newfamily <- rgl::rglExtrafonts(newsans = c("Comic Sans MS", "Impact", 
                                       "Verdana", "Tahoma"))

rgl::text3d(2,2,2, newfamily, family = "newsans", cex = 2)


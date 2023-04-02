### Set up environment
  
# install.packages("tidyverse")
# devtools::install_github("tylermorganwall/rayshader")

library(tidyverse)
library(rayshader)

### Gather data
fp <- "data"
fn <- "HadCRUT.5.0.1.0.analysis.summary_series.global.monthly.csv"
url <- paste(
  "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/current/analysis/diagnostics/",
  fn, sep="")

download.file(url, file.path(fp, fn))

dfm <- file.path(fp, fn) %>%
  read_csv(col_types=list(col_date("%Y-%m"), col_number())) %>%
  select(1:2) %>%
  rename(Date = Time) %>%
  rename(temp = starts_with("Ano"))



#### calculation of yearly data
# Count number of observations per year
num_obs_yr <- dfm %>%
  mutate(Year = format(ymd(Date), "%Y")) %>%
  group_by(Year) %>%
  summarise(num_obs = n())

# Drop years that don't have 12 months
m_per_yr <- 12
drop_yrs <- num_obs_yr %>% filter(num_obs != m_per_yr)

# Calculate the yearly average
dfy <- dfm %>%
  mutate(Year = format(ymd(Date), "%Y")) %>%
  filter(!Year %in% drop_yrs) %>%
  group_by(Year) %>%
  summarise(temp = mean(temp)) %>%
  mutate(Year = as.integer(Year))

# Ensure data is in ascending order
dfy <- arrange(dfy, Year)

### Calculate height matrix

n_yrs <- length(dfy$Year)

# rad_circle <- 2*pi
# circle_intv <- rad_circle / n_yrs

create_mat <- function(outside_radius, inside_radius){
  mat_dim <- outside_radius*2
  mat <- matrix(0, nrow=mat_dim, ncol=mat_dim)
  
  x0 <- mat_dim / 2
  y0 <- mat_dim / 2
  
  thetas <- seq(0, 2*pi, length.out=2000)

  base_lvl = min(dfy$temp)

  for (x in seq(1, mat_dim, 1)) {
    for (y in seq(1, mat_dim, 1)) {
      x_rct = x - x0 #- 1
      y_rct = y - y0 - 1
      xyl <- sqrt(x_rct^2 + y_rct^2)
      
      if (xyl >= inside_radius & xyl <= outside_radius) {
        theta <- atan(y_rct/x_rct)
        if(is.na(theta)) {theta <- 0}
        if (theta < 0) {theta <- theta + 2*pi}
        
        yr_num <- round(theta/(2*pi) * (n_yrs - 1)) + 1
        z <- (dfy$temp[yr_num] - base_lvl) * 100
        mat[x, y] <- z
      }
    }
  }
  
  return(mat)
}


draw_mat <- function(mat, map_text){
  mat %>%
    sphere_shade(sunangle = 10, colorintensity=10, texture=map_text)  %>%
    plot_3d(mat,
            zscale = 1, fov = 0, theta = 135, zoom = 0.75, phi = 45,
            water = TRUE,
            waterdepth = 1,
            watercolor = "white",
            wateralpha = 1,
            soil = FALSE,
            background = "white",
            windowsize = c(1000, 800))
  Sys.sleep(0.2)
  render_snapshot()
}


# print(matx)


# for (theta in thetas) {
#   for (radi in seq(inside_radius, outside_radius, 1)) {
#     yr_num <- round(theta/max(thetas) * (n_yrs - 1)) + 1
#     z <- dfy$temp[yr_num] * 100
#     x <- round(radi * cos(theta)) + x0
#     y <- round(radi * sin(theta)) + y0
#     mat[x, y] <- z
#   
#     # print(paste("theta", format(theta, digits=3),
#     #             "yr_num", yr_num,
#     #             "z", format(z, digits=3),
#     #             "x", x,
#     #             "y", y,
#     #             "mat[x, y]", format(mat[x, y], digits=3)))
#   }
# }
# 
#         # if(abs(x) < 250 & abs(y) < 250) {
#     print(paste("x", x,
#                  "  x-x0", x_rct,
#                  "  y", y,
#                  "  y-y0", y_rct,
#                  "  xyl", format(xyl, digits=5)))
# 
#      print(paste("theta", format(theta*180/pi, digits=1)))
# 
#      print(paste("yr_num", yr_num))
# 
#      print(paste("z", z))
# 
#      print(paste(#"theta", format(theta, digits=3),
#        #"yr_num", yr_num,
#        #"z", format(z, digits=3),
#        # "x - x0", (x0 - x),
#        # "y - y0", (y0 - y),
#        "mat[x, y]", format(mat[x, y], digits=3)))
#      status = "exit"
#      break
#      
#    }
#    

map_texture <- create_texture(
  lightcolor = "red", # main high light color, top center of the map
  shadowcolor = "blue", # main shadow color, bottom center of the map
  leftcolor = "pink", # left fill color, left center of the map
  rightcolor = "lightblue",
  centercolor ="white" # center of the map, represents flat areas
  )
plot_map(map_texture)

base_rad <- 100
base_w <- 20
scaler <- 1 #9.99
matx <- create_mat(base_rad * scaler, (base_rad - base_w) * scaler)
draw_mat(matx, map_texture)




rownames(matx) <- seq(1, dim(matx)[1], 1)
colnames(matx) <- seq(1, dim(matx)[1], 1)

matx[1:5, 1:5]

df <- as.data.frame(as.table(matx))
colnames(df) <- c("X", "Y", "Z")
df <- df %>%
  mutate(X = as.integer(X),
         Y = as.integer(Y))

head(df)
str(df)
summary(df)

p <- df %>% ggplot() +
  geom_tile(aes(x = X, y = Y, fill = Z)) +
  scale_x_continuous("X", expand = c(0, 0)) +
  scale_y_continuous("Y", expand = c(0, 0)) +
  scale_fill_gradientn("Z", colors = terrain.colors(10)) +
  coord_fixed()

p

par(mfrow = c(1, 2))
rayshader::plot_gg(p, width = 7, height = 4, raytrace = FALSE, preview = TRUE)

rayshader::plot_gg(p, multicore = TRUE, raytrace = TRUE,
                   width = 7, height = 4, scale = 300,
                   windowsize = c(200, 200),
                   zoom = 0.6, phi = 30, theta = 30)

Sys.sleep(0.2)
render_snapshot(clear = TRUE)





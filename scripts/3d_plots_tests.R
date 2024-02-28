
raster_test <- all.group.rasters[[1]][[1]]
terra::plot(raster_test)
min(terra::values(raster_test), na.rm = TRUE)
max(terra::values(raster_test), na.rm = TRUE)

m1<-
  as.matrix(
    data.frame(
      x = c(#min(terra::values(raster_test), na.rm = TRUE)
        0,2,4,6),
      y = c(2,4,6,10
            #max(terra::values(raster_test), na.rm = TRUE)
            ),
      z = c(1,2,3,4)
    )  
    
  )

m1
rr1 <- terra::classify(raster_test, m1, others=0) ### check the reclassification
# missing the lowest level for some reason!

## check reclassified values!!!!!!!!!!!!!!!!!!!!!!!!!!!

terra::plot(rr1)



cols <- c(
  "red", 
  "yellow",
  "green", 
  "blue"
)

from <- c(1:4)
to <- t(col2rgb(
  cols
))


rr1 <- na.omit(
  rr1
)

rr1.new <- terra::subst(
  rr1,
  from,
  to,
  names = cols
)


terra::plotRGB(rr1.new)
terra::plot(rr1.new)
#png if the classified raster

img_file <- "data/test_3d_01.png"

terra::writeRaster(
  rr1.new,
  img_file,
  overwrite = TRUE#,
  #NAflag = 255
)

img <- png::readPNG(img_file)

terra::plot(terra::rast(img))
#elevation raster


elev <- elevatr::get_elev_raster(
  locations = bcr, #warn about locations
  z = 7, clip = "locations" # 10 as writen by source was too high
)

#elevation raster  converted to terra and re projected
elevation <- elev |>
  terra::rast() |>
  terra::project(rast.crs)

terra::plot(elevation)

{
  #load folder with elevation raster
  topo.folder <- "data/YT Boreal Refugia Drive/YK Refugia Code and material/PresentDayNormals/Topography/"
  
  
  #load elevation raster
  elevation <- terra::rast(paste0(topo.folder,"elevation_1KMmd_GMTEDmd.tif"))
  
  elevation <-  terra::crop(elevation, terra::project(NA_rast.crs, "EPSG:4326" ))
  
  elevation <-  terra::project(elevation, NA_rast.crs )
  
  elevation <-  terra::crop(elevation, raster_test) 
  
  terra::plot(elevation)
}

#matrix of the elevation raster
elmat <- rayshader::raster_to_matrix(
  elevation
)

# 6. RENDER SCENE
#----------------

h <- nrow(elevation)
w <- ncol(elevation)
{
  
  

begin_time <- Sys.time()
elmat |>
  rayshader::height_shade(
    texture = colorRampPalette(
      "white"
    )(512) #any number of values you actually want????
  ) |>
  rayshader::add_overlay(
    img,
    alphalayer = .9, # how transparent the raster image is
    alphacolor = "white" # should correspond to height_shade above
  ) |>
  rayshader::add_shadow(
    rayshader::lamb_shade(
      elmat,
      zscale = 50,
      sunaltitude = 90,
      sunangle = 315,
    ), max_darken = .25
  ) |>
  rayshader::add_shadow(
    rayshader::texture_shade(
      elmat,
      detail = .95, ### may reduce this closer to 0 and it may reduce computation time
      brightness = 90, 
      contrast = 80,
    ), max_darken = .1
  ) |>
  rayshader::plot_3d(
    elmat,
    zscale = 10, ###Default `1`. The ratio between the x and y spacing (which are assumed to be equal) and the z axis. For example, if the elevation levels are in units of 1 meter and the grid values are separated by 10 meters, `zscale` would be 10. Adjust the zscale down to exaggerate elevation features.
    solid = F,
    shadow = T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w / 5, h / 5
    ),
    zoom = .5,
    phi = 85,
    theta = 0 
  )
end_time <- Sys.time()
print(paste0("Rayshader process 1 time ", round(difftime(end_time, begin_time, units = "mins"),2), " minutes"))
}

{
begin.time <- Sys.time()

rayshader::render_highquality(
  samples = 32, #def 128
  filename = "plots/test-refugia-3d.png",
  preview = FALSE,
  light = FALSE,
  environment_light = "air_museum_playground_4k.hdr",
  intensity_env = 2,
  rotate_env = 45,
  interactive = FALSE,
  parallel = FALSE, #if false uses all available cores to render
  width = w, height = h
)
end_time <- Sys.time()

print(paste0("Rendering process time ", round(difftime(end_time, begin_time, units = "mins"),2), " minutes" ))
}

library(svgchop)
source('../website/static/script/_lib/rayrender.R')

steps <- 12
chopped <- lapply(lapply(seq_len(steps), chop, file=R_logo()), flatten)
starts <- lapply(chopped, lapply, attr, 'starts')
ext <- attr(chopped[[length(chopped)]], 'extents')
norm <- lapply(
  chopped,
  function(svg) {
    norm <- lapply(
      svg,
      function(x) (x[,] - vapply(ext, '[', 0, 1)) / vapply(ext, diff, 0) - .5
    )
    lapply(norm, '*', vapply(ext, diff, 0) / diff(ext$x) * c(-1, -1))
} )
bottom <- min(unlist(lapply(norm[[length(norm)]], '[', 2, )))
svg <- chopped[[1]]
url <- attr(svg, 'url')

## Compute colors
hoop.col <- attr(svg[[1]], 'style-computed')[['fill']]
hoop.col <- approximate_color(hoop.col, url)
rrr.col <- attr(svg[[2]], 'style-computed')[['fill']]
rrr.col <- approximate_color(rrr.col, url)

## Darken and saturate (gave up on this) the colors
col.num <- t(col2rgb(c(rrr.col, hoop.col)) * .7)
# col.num[1,] <-
#   col.num[1,] - mean(col.num[1,]) * c(1.1, .7, 1.2) + mean(col.num[1,])
cols <- rgb(col.num, maxColorValue=255)

library(rayrender)

frames <- 21
stopifnot(frames%%2 != 0)
frames.start <- floor(frames/2)
frames.end <- floor(frames/2)
point.pwr <- 5
point.x <- 1
points.start <- c(0, diff(seq(0, point.x, length.out=frames.start)^point.pwr))
points.end <- rev(diff(seq(0, point.x, length.out=frames.end + 1)^point.pwr))
points.end <- points.end / points.end[1] * points.start[length(points.start)]
frame.points <- cumsum(
  c(points.start, points.start[length(points.start)], points.end)
)
x <- frame.points / max(frame.points)

# x <- (2-(cos(seq(0, pi, length.out=frames)) + 1)) * .5
depths <- x * .5
angles <- x * 90
radii <- approxfun(0:1, c(1e3, 10))(x)
# radii <- rep(1e3, 5)
start <- Sys.time()
fovs <- approxfun(0:1, c(11, 11))(x)
svgs <- round(
  approxfun(c(0,1, 1), c(1, steps, steps))(seq(0, 1, length.out=frames))
)

for(i in seq_along(x)[length(x)]) {
  cat(sprintf("\rFrame %03d ellapsed %f", i, Sys.time() - start))
  depth <- depths[i]
  angle <- angles[i]
  radius <- radii[i]
  fov <- fovs[i]
  nudge <- 1e-3
  svgi <- svgs[i]
  hoop <- t(norm[[svgi]][[1]])
  rrr <- t(norm[[svgi]][[2]])

  ray.r <- extruded_polygon(
    rrr,
    top=depth / 2 + 2 * nudge, bottom=-depth / 2 - 2 * nudge,
    material=diffuse(color=cols[1]),
    holes=starts[[svgi]][[2]][-1]
  )
  ray.hoop <- extruded_polygon(
    hoop,
    top=depth / 4 + nudge, bottom=-depth / 4 - nudge,
    material=diffuse(color=cols[2]),
    holes=starts[[svgi]][[1]][-1]
  )
  ray.logo <- group_objects(
    dplyr::bind_rows(ray.hoop, ray.r),
    group_angle=c(90, 180, 0)
  )
  wall.xz <- xz_rect(
    xwidth=10, zwidth=10, material=diffuse(),
    y=bottom,
  )
  wall.xy <- xy_rect(
    xwidth=3, ywidth=3, material=diffuse(), z=-depth / 2
  )
  floor <- sphere(
    z=-radius, radius=radius
  )
  walls <- group_objects(
    dplyr::bind_rows(
      floor
      # wall.xz,
      # wall.xy
    ),
    group_angle=c(angle, 0, 0),
    # pivot_point=c(0, bottom, -depth / 2)
    pivot_point=c(0, bottom, 0)
  )
  # light <- sphere(x=5, y=5, z=5, material=light(intensity=100))
  light <- group_objects(
    sphere(x=0, y=0, z=sqrt(75), material=light(intensity=80)),
    group_angle=c(angle * .7, angle * .3, 0),
    pivot_point=numeric(3)
  )

  scene <- dplyr::bind_rows(
    ray.logo,
    walls,
    light,
    # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
  )
  out <- next_file('~/Downloads/svgchop/test1/img-000.png')
  if(0) {
    render_preview(
      scene, fov=fov,
      width=600, height=600,
      # width=1200, height=1200,
      lookfrom=c(0, .5, 6),
      # lookfrom=c(-3, 0, .2),
      # lookat=c(0, -.38, 0),
      light_direction=c(0, -1, -.25),
      filename=out
    )
  } else {
    render_scene(
      scene,
      fov=fov,
      # fov=11,
      width=720, height=400, samples=500,
      lookfrom=c(0, .5, 6),
      lookat=c(0, 0, 0),
      filename=out,
      clamp_value=5,
      # debug_channel="normals"
    )
  }
}
cat(paste0(c("\r", rep(" ", getOption('width')), "\r"), collapse=""))

stop("done")

# side by side for the logo.  Assumes frames = 23

cat(sprintf("\rFrame %03d ellapsed %f", i, Sys.time() - start))
depth <- depths[i]
angle <- angles[i]
radius <- radii[i]
fov <- fovs[i]
nudge <- 1e-3
svgi <- svgs[i]

hoop1 <- t(norm[[2]][[1]])
rrr1 <- t(norm[[2]][[2]])

hoop <- t(norm[[svgi]][[1]])
rrr <- t(norm[[svgi]][[2]])

ray.r <- extruded_polygon(
  rrr,
  top=depth / 2 + 2 * nudge, bottom=-depth / 2 - 2 * nudge,
  material=diffuse(color=cols[1]),
  holes=starts[[svgi]][[2]][-1]
)
ray.hoop <- extruded_polygon(
  hoop,
  top=depth / 4 + nudge, bottom=-depth / 4 - nudge,
  material=diffuse(color=cols[2]),
  holes=starts[[svgi]][[1]][-1]
)
ray.r1 <- extruded_polygon(
  rrr1,
  top=depth / 2 + 2 * nudge, bottom=-depth / 2 - 2 * nudge,
  material=diffuse(color=cols[1]),
  holes=starts[[2]][[2]][-1]
)
ray.hoop1 <- extruded_polygon(
  hoop1,
  top=depth / 4 + nudge, bottom=-depth / 4 - nudge,
  material=diffuse(color=cols[2]),
  holes=starts[[2]][[1]][-1]
)
ray.logo <- group_objects(
  dplyr::bind_rows(ray.hoop, ray.r),
  group_angle=c(90, 180, 0),
  group_translate=c(+.55, 0, 0)
)
ray.logo1 <- group_objects(
  dplyr::bind_rows(ray.hoop1, ray.r1),
  group_angle=c(90, 180, 0),
  group_translate=c(-.55, 0, 0)
)
radius <- 50
floor <- sphere(
  z=-radius, radius=radius
)
walls <- group_objects(
  floor,
  group_angle=c(angle, 0, 0),
  # pivot_point=c(0, bottom, -depth / 2)
  pivot_point=c(0, bottom, 0)
)
# light <- sphere(x=5, y=5, z=5, material=light(intensity=100))
light <- group_objects(
  sphere(x=0, y=0, z=sqrt(75), material=light(intensity=80)),
  group_angle=c(angle * .45, angle * .15, 0),
  pivot_point=numeric(3)
)

scene <- dplyr::bind_rows(
  ray.logo,
  ray.logo1,
  walls,
  light,
  # cube(x=.4, y=.4, xwidth=.1, ywidth=.1)
)
out <- next_file('~/Downloads/svgchop/test1/img-000.png')
render_scene(
  scene,
  fov=fov,
  # fov=11,
  width=1280, height=640, samples=500,
  lookfrom=c(0, .5, 6),
  lookat=c(0, 0, 0),
  filename=out,
  clamp_value=5,
  # debug_channel="normals"
)

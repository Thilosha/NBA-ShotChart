# shot richness
# access shot chart data from the API
library(nbastatR)

# team shot chart stats
mavs <- teams_shots(
  teams = "Dallas Mavericks",
  seasons = 2019
)
hawks <- teams_shots(
  teams = "Atlanta Hawks",
  seasons = 2019
)
sixers <- teams_shots(
  teams = "Philadelphia 76ers",
  seasons = 2019
)
knicks <- teams_shots(
  teams = "New York Knicks",
  seasons = 1999
)
LAL <- teams_shots(
  teams = "Los Angeles Lakers",
  seasons = 2019
)
nets <- teams_shots(
  teams = "Brooklyn Nets",
  seasons = 2019
)
bos <- teams_shots(
  teams = "Boston Celtics",
  seasons = 2019
)
library(dplyr)
# subset players
luka <- mavs %>% filter(namePlayer == "Luka Doncic")
trae <- hawks %>% filter(namePlayer == "Trae Young")
bens <- sixers %>% filter(namePlayer == "Ben Simmons")
joel <- sixers %>% filter(namePlayer == "Joel Embiid")
Jimmy <- sixers %>% filter(namePlayer == "Jimmy Butler")
AHou <- knicks %>% filter(namePlayer == "Allan Houston")
Spree <- knicks %>% filter(namePlayer == "Latrell Sprewell")
Larry <- knicks %>% filter(namePlayer == "Larry Johnson")
Leb <- LAL %>% filter(namePlayer == "LeBron James")
DRuss <- nets %>% filter(namePlayer == "D'Angelo Russell")
Tat <- bos %>% filter(namePlayer == "Jayson Tatum")
kyr <- bos %>% filter(namePlayer == "Kyrie Irving")

# plotting packages
library(ggplot2)
library(artyfarty)
library(scico)
library(extrafont)
library(patchwork)
# to plot a half court, using data from the ballR shiny app
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/plot_court.R")
source("https://raw.githubusercontent.com/toddwschneider/ballr/master/court_themes.R")
plot_court() # created the court_points object we need
court_points <- court_points %>% mutate_if(is.numeric, ~ . * 10)

library(sf)
# to simple feature
courtsf <- sf::st_as_sfc(sf::st_bbox(c(xmin = -250, xmax = 250, ymax = 470, ymin = 0)))
# grid
courtgrid <- sf::st_make_grid(courtsf, what = "polygons", cellsize = 20) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  sf::st_sf() %>%
  dplyr::mutate(cellid = dplyr::row_number())

# court and grid
ggplot() + geom_path(
  data = court_points,
  aes(x = x, y = y, group = desc),
  color = "black"
) + coord_equal() + theme_empty() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  ggplot(courtgrid) + geom_sf() + geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc),
    color = "black"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  )

# calculate n per grid square for points
get_richness <- function(playersSpatialDFs) {
  shot_richness <- courtgrid %>%
    sf::st_join(playersSpatialDFs) %>%
    dplyr::group_by(cellid) %>%
    dplyr::summarize(num_shots = n())
}

# iterate across list of player DFs
get_top_shots <- function(playerDFs, n) {
  playersSpatial <- purrr::map(playerDFs, sf::st_as_sf, coords = c("locationX", "locationY"))
  playersShotRich <- purrr::map(playersSpatial, get_richness)
  richness_centroids <- purrr::map(playersShotRich, sf::st_centroid)
  top_shots <- purrr::map(richness_centroids, dplyr::top_n, n, num_shots)
  top_shotsXY <- purrr::map(top_shots, sf::st_coordinates)
  top_shotsXYtib <- purrr::map(top_shotsXY, dplyr::as_tibble)
  top_shots_DFs <- purrr::map2(top_shots, top_shotsXYtib, dplyr::bind_cols)
  top_shots_DFs <- purrr::set_names(top_shots_DFs, names(playerDFs))
  centcoords <- purrr::map(top_shots_DFs, sf::st_coordinates)
  centmat <- purrr::reduce(centcoords, rbind)
  mat_labels <- rep(names(top_shots_DFs), purrr::map(top_shots_DFs, nrow))
  player_labels <- unique(purrr::simplify(purrr::map(playerDFs, "namePlayer")))
  fnoutput <- list(top_shots_DFs, centmat, mat_labels, player_labels, playersSpatial, playersShotRich)
  return(fnoutput)
}
# named list of players and labels
playerDFsList <- list(
  LD = luka, TY = trae, BS = bens,
  LBJ = Leb, AH = AHou, LS = Spree,
  JE = joel, JB = Jimmy, LJ = Larry, DR = DRuss,
  KI = kyr, JT = Tat
)

# density-based top shot locations
shootingOv24 <- get_top_shots(playerDFsList, 24)


# for reporting intermediate steps in grid-based process
monok_new <- theme_monokai_full() %+replace%
  theme(text = element_text(family = "NanumGothic", size = 18))
JTpoints <- shootingOv24[[5]]$JT %>% st_coordinates() %>% as_tibble()

# all shots and gridded shots
ggplot(court_points) + geom_path(
  aes(x = x, y = y, group = desc),
  color = "black"
) + coord_equal() + theme_monokai_full() +
  theme(
    text = element_text(size = 18, family = "NanumGothic"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
  ) + labs(x = "", y = "", title = "all shots") +
  geom_point(
    data = JTpoints, aes(X, Y + 45),
    pch = 21,
    color = "#007A33", fill = "#BA9653", size = 1, alpha = 0.7
  ) +
  
  shootingOv24[[6]]$JT %>%
  ggplot() +
  geom_sf(aes(fill = log(num_shots))) +
  scale_fill_scico(direction = -1, palette = "bamako", guide = FALSE) +
  theme_monokai_full() + labs(title = "shot density") +
  theme(text = element_text(size = 16, family = "NanumGothic")) +
  plot_annotation(
    title = "Jayson Tatum - 2019 season",
    theme = monok_new
  )

# top shots and cell centroids
jt24cent <- shootingOv24[[6]]$JT %>% top_n(24, num_shots) %>% st_centroid()
shootingOv24[[6]]$JT %>%
  top_n(24, num_shots) %>%
  ggplot() +
  geom_sf(aes(fill = log(num_shots))) +
  scale_fill_scico(direction = -1, palette = "bamako", guide = FALSE) +
  theme(text = element_text(size = 16, family = "NanumGothic")) +
  theme_monokai_full() + labs(title = "top 24 grid cells \n (includes ties)") +
  shootingOv24[[6]]$JT %>%
  top_n(24, num_shots) %>%
  ggplot() +
  geom_sf(aes(fill = log(num_shots))) +
  scale_fill_scico(direction = -1, palette = "bamako", guide = FALSE) +
  geom_sf(data = jt24cent) +
  theme_monokai_full() + labs(title = "cell centroids") +
  theme(text = element_text(size = 16, family = "NanumGothic")) +
  plot_annotation(title = "Jayson Tatum - 2019 season", theme = monok_new)

# source function to calculate pp overlap
source("https://raw.githubusercontent.com/danlwarren/arc-extensions/master/nncluster.R")

# fn to calculate O, fortify data for plotting, & summarize min-max values
get_shots_ppo <- function(topShotsout) {
  # point proximity overlap
  xymat <- purrr::pluck(topShotsout, 2)
  player_labs <- purrr::pluck(topShotsout, 3)
  O_stat <- nncluster(xymat, player_labs)
  
  # fortify and label shooting data
  xy_topshots <- bind_cols(as_tibble(xymat), tibble(label = player_labs))
  playList <- tibble(label = names(purrr::pluck(topShotsout, 1)), namePlayer = purrr::pluck(topShotsout, 4))
  xy_topshots <- left_join(xy_topshots, playList)
  
  # min and max values
  min_O_val <- min(O_stat, na.rm = T)
  max_O_val <- max(O_stat, na.rm = T)
  O_mins <- as.vector(which(O_stat == min_O_val, arr.ind = TRUE))
  O_maxes <- as.vector(which(O_stat == max_O_val, arr.ind = TRUE))
  xy_minO <- filter(xy_topshots, label %in% names(O_stat)[O_mins]) %>%
    mutate(val = paste0("least overlap ", "(", round(min_O_val, 2), ")"))
  xy_maxO <- filter(xy_topshots, label %in% names(O_stat)[O_maxes]) %>%
    mutate(val = paste0("most overlap ", "(", round(max_O_val, 2), ")"))
  xy_summary <- bind_rows(xy_minO, xy_maxO)
  
  fnoutput <- list(O_stat, xy_summary, xy_topshots)
  return(fnoutput)
}

# calculate O
shotSim24 <- get_shots_ppo(shootingOv24)

# y coordinates are shifted to account for the backboard+rim in the ballr data

ggplot(shotSim24[[2]], aes(x = X, y = Y + 45)) +
  scale_fill_scico_d(name = "player", palette = "batlow") +
  geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc),
    color = "black"
  ) +
  coord_equal() +
  geom_point(
    pch = 21, size = 3, color = "white",
    aes(fill = namePlayer), position = "jitter"
  ) +
  xlim(-260, 260) +
  theme_monokai_full() +
  labs(title = "Most frequent shot locations (density-based)", x = "", y = "") +
  theme(
    text = element_text(size = 18, family = "NanumGothic"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_text(color = "white"),
    strip.background = element_blank(),
    strip.text = element_text(color = "white"),
    legend.position = "bottom"
  ) +
  facet_wrap(~val)

library(gt)
# show all O values
shotSim24[[1]] %>%
  tibble::rownames_to_column() %>%
  mutate_if(is.numeric, round, 2) %>%
  gt() %>%
  data_color(
    columns = everything(),
    colors = scales::col_numeric(
      palette = scico(10, palette = "imola", direction = -1),
      domain = NULL
    )
  )

# for reporting
shotSim24[[3]] %>%
  select(label, namePlayer) %>%
  distinct() %>%
  arrange(namePlayer) %>%
  knitr::kable()



# Packages ----------------------------------------------------------------

libs <- c("tidyverse", "sf",
          "giscoR", "geobr",
          "ggspatial", "ggrepel",
          "scales", "ggsci")

installed_libraries <- libs %in% rownames(installed.packages())

if (any(installed_libraries == F)) {
  install.packages(libs[!installed_libraries])
}

invisible(lapply(libs, library,
                 character.only = T))



# Brazilian counties ------------------------------------------------------

br_counties <- read.csv("Brazilian_Counties.csv")


# Specific counties -------------------------------------------------------

readxl::read_xlsx("new_data.xlsx") |>
  dplyr::select(LOCAL) |>
  pull() |>
  unique() |>
  str_to_lower() -> counties


# Selecting the specific counties -----------------------------------------

br_counties |>
  filter(codigo_uf %in% c(41, 50)) |>
  filter_at(
    vars(nome),
    \(x) stringi::stri_trans_general(str = str_to_lower(x),
                                     id = "Latin-ASCII") %in% counties
  ) -> selected_counties


# Downloading Brazilian states --------------------------------------------

all_states <- geobr::read_state(code_state = "all", year = 2019)


# Selecting specific states -----------------------------------------------

all_states |>
  dplyr::filter(abbrev_state %in% c("MS", "PR")) -> MS_PR


# Downloading biomes ------------------------------------------------------

biom_w <- geobr::read_biomes(year = 2019,
                             simplified = TRUE,
                             showProgress = T)


# Get rivers data ---------------------------------------------------------

url <-
  "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_sa_shp.zip"
file_name <- "sa-rivers.zip"

get_rivers <- function(url, file_name) {
  download.file(url = url,
                destfile = file_name,
                mode = "wb")

  unzip(file_name)
}

get_rivers(url, file_name)
print("Getting Rivers")
list.files()

load_rivers <- function() {
  filenames <- list.files(path = "HydroRIVERS_v10_sa_shp",
                          pattern = "\\.shp$",
                          full.names = T)
  print(filenames)
  samerica_rivers <- sf::st_read(filenames)

  return(samerica_rivers)
}

# It's timing consuming
samerica_rivers <- load_rivers()

# saveRDS(samerica_rivers, file = "samerica_rivers.rds")

samerica_rivers <- readRDS(file = "samerica_rivers.rds")


# Intersection with biomes ------------------------------------------------

biom_w |>
  sf::st_transform(sf::st_crs(MS_PR)) |>
  sf::st_intersection(MS_PR) -> states_biomes


# Intersection with rivers ------------------------------------------------

# It's time consuming. Just read the RDS file.
samerica_rivers |>
  dplyr::select(ORD_FLOW) |>
  sf::st_transform(sf::st_crs(states_biomes)) |>
  sf::st_intersection(states_biomes) -> states_biomes_rivers


saveRDS(states_biomes_rivers, file = "states_biomes_rivers.rds")
states_biomes_rivers <- readRDS(file = "states_biomes_rivers.rds")

# Check the possibilities
states_biomes_rivers$ORD_FLOW |> unique()

states_biomes_rivers_width <- states_biomes_rivers |>
  dplyr::mutate(
    width = as.numeric(ORD_FLOW),
    width = dplyr::case_when(
      width == 1 ~ 1,
      width == 2 ~ 1,
      width == 3 ~ .9,
      width == 4 ~ .8,
      width == 4 ~ .7,
      width == 5 ~ .6,
      TRUE ~ 0
    )
  ) |>
  sf::st_as_sf()


# Colors ------------------------------------------------------------------

unique(states_biomes_rivers_width$name_biome) |> length()

hcl.pals("qualitative")

hcl.colors(4, "Set 2",
           alpha = 1) -> colors_vec


colors_vec

scales::show_col(colors_vec)

viridis::viridis(
  10,
  alpha = 1,
  begin = 0,
  end = 1,
  option = "D"
) -> colors_vec_2


scales::show_col(colors_vec_2[c(3, 5, 7, 9)])



# Plot --------------------------------------------------------------------

MS_PR |>
  mutate(centroids = st_centroid(geom)) -> MS_PR


ggplot() +
  geom_sf(data = states_biomes,
          aes(fill = name_biome)) +
  geom_sf(
    data = states_biomes_rivers_width |>
      dplyr::filter(ORD_FLOW %in% 1:6),
    aes(size = width,
        alpha = width),
    color = "blue4",
    show.legend = FALSE
  ) +
  scale_fill_manual(name = "Biomes",
                    values = colors_vec_2[c(3, 5, 7, 9)]) +
  geom_sf(data = MS_PR,
          alpha = 0,
          lwd = 0.9) +
  geom_sf_text(data = MS_PR,
               aes(label = abbrev_state,
                   geometry = centroids),
               colour = "#333333",
               size = 5) +
#  scale_alpha(range = c(.01, .8)) +
  geom_point(
    data = selected_counties,
    aes(x = longitude, y = latitude),
    color = 'black',
    size = 1.5
  )  +
  ggrepel::geom_text_repel(
    data = selected_counties,
    aes(x = longitude, y = latitude, label = nome),
    size = 3.55,
    force = 0.5,

  ) +
  xlab('Latitude') +
  ylab('Longitude')  +
  ggspatial::annotation_scale(location = "br", width_hint = 0.25) +
  ggspatial::annotation_north_arrow(location = "bl",
                                    style = ggspatial::north_arrow_fancy_orienteering) +
  theme_minimal() + # deve estar acima do theme, por isso, não mudava o tamanho das letras
  theme(
    axis.text = element_text(size = 10, color = 'black'),
    axis.title = element_text(
      size = 14,
      color = 'black',
      face = 'bold'
    ),
    legend.text = element_text(size = 8, color = 'black'),
    legend.title = element_text(
      size = 10,
      color = 'black',
      face = 'bold'
    )

  ) -> p

p

ggsave(
  plot = p,
  filename = "MS_PR_river_biomes.jpg",
  width = 8.35,
  height = 6.70,
  dpi = 600,
  bg = "white",
  device = "jpg"
)

# Usefull links -----------------------------------------------------------

# https://carpentries-incubator.github.io/r-tidyverse-4-datasets/instructor/aio.html

# geom_text_repel
# https://ggrepel.slowkow.com/articles/examples.html

# string manipulation
# https://rpubs.com/iPhuoc/stringr_manipulation

# borders
# https://github.com/tidyverse/ggplot2/issues/2827

# book
# https://modernstatisticswithr.com/index.html
# https://rpubs.com/Sergio_Garcia/string_manipulation_with_stringr_in_r
# https://stackoverflow.com/questions/10294284/remove-all-special-characters-from-a-string-in-r

# Script to get indicators from the RPG
# input:
#  RPG_2-2_2023_PARCELLES_GRAPHIQUES.gpkg
#     from https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_RPG
# output:
#   indicators_csv/MAILLE_RPG_2023.csv
#   indicators_csv/COMMUNE_RPG_2023.csv
#   figure/RPG_XX_2023_MAILLE.png
#   figure/RPG_XX_2023_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)
library(vegan)


data_folder <- here::here("data", "raw-data", "rpg")
ref_folder <- here::here("data", "derived-data", "ref")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

commune <- vect(file.path(ref_folder, "commune_2154.gpkg"))
commune$id <- commune$INSEE_COM
mailles <- vect(file.path(ref_folder, "mailles_10km_2154.gpkg"))
mailles$id <- mailles$cd_sig

# RPG categories
fallow <- c("J5M", "J6P", "J6S", "JAC", "JNO")
noag <- c("BFP", "BFS", "BOR", "BTA", "SNA", "SNE")
grass <- c("BOP", "SPH", "SPL", "PPH", "PRL", "PTR", "RGA")

# 2. Load and clean RPG data ------------------------------------
rpg <- vect(
  file.path(data_folder, "RPG_2-2_2023_PARCELLES_GRAPHIQUES.gpkg"),
)

# remove non agricultural fields
crops <- rpg[!rpg$CODE_CULTU %in% c(noag, grass)]

# select only fallow
jachere <- rpg[rpg$CODE_CULTU %in% fallow]


# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

# Same process for commune and maille
for (i in c("Commune", "Maille")) {
  print(i)
  if (i == "Commune") {
    shp <- commune
  } else {
    shp <- mailles
  }

  # fallow area
  intF <- intersect(jachere, shp)
  intF$calc_area <- expanse(intF) * 0.0001
  sum_areaF <- tapply(intF$calc_area, intF$id, sum, na.rm = TRUE)

  shp$FALLOW_AREA_HA <- ifelse(
    shp$id %in% names(sum_areaF),
    sum_areaF[match(shp$id, names(sum_areaF))],
    0
  )

  shp$FALLOW_AREA_PCT <- (shp$FALLOW_AREA_HA / shp$AREA_HA * 100) |>
    round(2)

  fi <- paste0("RPG_FALLOW_AREA_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "FALLOW_AREA_PCT",
    border = NA,
    main = paste0("Fallow area (%) - 2023 - ", i)
  )
  dev.off()

  # agricultural area - we want:
  # - the agricultural surface (SAU)
  # - the dominant crop
  # - the shanon diversity of crops
  # - average field size (weighted average, or median)

  intC <- intersect(crops, shp)
  intC$calc_area <- expanse(intC) * 0.0001

  # total cultivated area
  sum_areaC <- tapply(intC$calc_area, intC$id, sum, na.rm = TRUE)

  shp$CULTIVATED_AREA_HA <- ifelse(
    shp$id %in% names(sum_areaC),
    sum_areaC[match(shp$id, names(sum_areaC))],
    0
  )

  shp$CULTIVATED_AREA_PCT <- (shp$CULTIVATED_AREA_HA / shp$AREA_HA * 100) |>
    round(2)

  fi <- paste0("RPG_CULTIVATED_AREA_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "CULTIVATED_AREA_PCT",
    border = NA,
    main = paste0("Cultivated area (%) - 2023 - ", i)
  )
  dev.off()

  # crop diversity
  area_crop <- tapply(
    intC$calc_area,
    list(intC$id, intC$CODE_CULTU),
    sum,
    na.rm = TRUE
  )
  # dim(area_crop) # id, crop

  # dominant crop
  dom <- colnames(area_crop)[apply(area_crop, 1, which.max)]
  # table(dom)

  shp$DOMINANT_CROP <- ifelse(
    shp$id %in% row.names(area_crop),
    dom[match(shp$id, row.names(area_crop))],
    NA
  )

  fi <- paste0("RPG_DOMINANT_CROP_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "DOMINANT_CROP",
    border = NA,
    main = paste0("Dominant crop - 2023 - ", i)
  )
  dev.off()

  # crop diversity
  # replace NA by 0s
  area_crop[is.na(area_crop)] <- 0
  shannon <- vegan::diversity(area_crop, index = "shannon")

  shp$CROP_DIV_SHANNON <- ifelse(
    shp$id %in% row.names(area_crop),
    shannon[match(shp$id, row.names(area_crop))],
    NA
  )

  fi <- paste0("RPG_CROPDIV_SHANNON_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "CROP_DIV_SHANNON",
    border = NA,
    main = paste0("Crop diversity, Shannon - 2023 - ", i)
  )
  dev.off()

  # field size
  # weigthed average of field size
  wsum <- tapply(intC$SURF_PARC * intC$calc_area, intC$id, sum, na.rm = TRUE)
  field_size <- wsum / sum_areaC

  shp$MEAN_FIELD_SIZE_HA <- ifelse(
    shp$id %in% names(wsum),
    field_size[match(shp$id, names(wsum))],
    0
  )

  med <- tapply(intC$SURF_PARC, intC$id, median, na.rm = TRUE)
  shp$MEDIAN_FIELD_SIZE_HA <- ifelse(
    shp$id %in% names(med),
    med[match(shp$id, names(med))],
    0
  )

  # remove areas with no field
  shp$MEDIAN_FIELD_SIZE_HA[shp$CULTIVATED_AREA_HA == 0] <- NA
  shp$MEAN_FIELD_SIZE_HA[shp$CULTIVATED_AREA_HA == 0] <- NA

  shp$MEAN_FIELD_SIZE_LOG <- log1p(shp$MEAN_FIELD_SIZE_HA)
  shp$MEDIAN_FIELD_SIZE_LOG <- log1p(shp$MEDIAN_FIELD_SIZE_HA)

  fi <- paste0("RPG_MEAN_FIELDSIZE_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "MEAN_FIELD_SIZE_LOG",
    border = NA,
    main = paste0("Mean field size (log+1, ha) - 2023 - ", i)
  )
  dev.off()

  fi <- paste0("RPG_MEDIAN_FIELDSIZE_2023_", toupper(i), ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "MEDIAN_FIELD_SIZE_LOG",
    border = NA,
    main = paste0("Median field size (log+1, ha) - 2023 - ", i)
  )
  dev.off()

  # export
  out_fi <- paste0(toupper(i), "_RPG_2023.csv")
  write.csv(
    data.frame(shp),
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}

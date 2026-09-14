# Script to get the distance to closest AMAP

# input:
# AMAP list scrapped from https://www.avenir-bio.fr/annuaire_amap.php
#   amap/avenirbio.csv
#
# output:
#   indicators_csv/MAILLEXkm_HVE_N_2024.csv
#   indicators_csv/COMMUNE_AMAP_2026.csv
#   figure/AMAP_XX_2026_MAILLEXkm.png
#   figure/AMAP_XX_2026_COMMUNE.png

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(data.table) #for fast processing of large files
library(terra)

data_folder <- here::here("data", "raw-data", "amap")
ref_folder <- here::here("data", "derived-data", "ref")
out_folder <- here::here("data", "derived-data", "clean_data")
ind_folder <- here::here("data", "derived-data", "indicators_csv")
fig_folder <- here::here("figure")

# 1.bis SCRAPPING OF https://www.avenir-bio.fr/annuaire_amap.php
# library(rvest)
# page <- read_html("https://www.avenir-bio.fr/annuaire_amap.php")

# # Retrieve all links from the directory
# links <- page |>
#   html_elements("a") |>
#   html_attr("href") |>
#   na.omit()
# links <- links[grepl("^amap,", links)]

# splitlinks <- strsplit(links, ",")
# links <- links[sapply(splitlinks, length) == 4]
# splitlinks <- splitlinks[sapply(splitlinks, length) == 4]

# df <- data.frame(
#   "dep" = sapply(splitlinks, function(x) x[3]),
#   "city" = sapply(splitlinks, function(x) gsub(".html$", "", x[4])),
#   "url" = url_absolute(links, "https://www.avenir-bio.fr/annuaire_amap.php")
# )

# # remove dom-tom
# df <- df[!df$dep %in% c("973", "974"), ]

# #remove duplicates
# df <- df[!duplicated(df[, c("dep", "city")]), ]
# # table(is.na(df$url))

# get_postalcode <- function(page_url) {
#   page <- tryCatch(
#     read_html(page_url),
#     error = function(e) return(NULL)
#   )

#   if (is.null(page)) {
#     return(NA)
#   } else {
#     items <- page %>%
#       html_elements("li") %>%
#       html_text2()

#     code <- stringr::str_extract(items[length(items)], "(?<=\\()\\d{5}")
#     return(code)
#   }
# }
# df$code <- sapply(df$url, get_postalcode)
# df$name <- clean_city_names(df$city)
# write.csv(
#   df,
#   file = here::here("data", "raw-data", "amap", "avenirbio.csv"),
#   row.names = FALSE
# )

df <- read.csv(
  here::here("data", "raw-data", "amap", "avenirbio.csv")
)

commune <- terra::vect(file.path(ref_folder, "commune_2154.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles
synonyms <- read.csv(file.path(ref_folder, "commune_synonyms.csv"))

#format the list of reference
ref <- data.frame(
  "name" = clean_city_names(commune$NOM_M),
  "code" = check_postalcode(commune$POSTAL_CODE),
  "id" = commune$INSEE_COM
)

# df$name <- clean_city_names(df$name)
# m1 <- match_cities(
#   df,
#   ref,
#   dfsyn = synonyms,
#   dmax = 0.25,
#   file.out = file.path(out_folder, "fuzzy_Annuaire_AMAP.csv")
# )
# Number of simple match: 1422(89.32%)
# Number of verified synonyms: 87(5.46%)
# Number of fuzzy match: 59(3.71%)
# Number of non-matching elements: 24(1.51%)

# Once verified, load the additional list of synonyms
checked_fuzzy <- read.csv(file.path(out_folder, "verified_Annuaire_AMAP.csv"))
keepR <- checked_fuzzy$verified == "ok" & !is.na(checked_fuzzy$ref_id)
checked_fuzzy <- checked_fuzzy[keepR, ] # 82 rows
dfsyn <- data.frame(
  "name" = checked_fuzzy$ori_name,
  "code" = checked_fuzzy$ori_code,
  "id" = checked_fuzzy$ref_id
)

# merge with the synonmys from INSEE and Poste
newsynonyms <- rbind(synonyms, dfsyn)

# Step 4: Final match with verified synonyms and no fuzzy match ----------------
# make match without any fuzzy match : dmax=0 and no output
m2 <- match_cities(df, ref, newsynonyms, file.out = NULL, dmax = 0)
# Number of simple match: 1422(89.32%)
# Number of verified synonyms: 166(10.43%)
# Number of non-matching elements: 4(0.25%)

df$INSEE_COM <- m2$ref_id

# length(unique(df$INSEE_COM)) - 1553 communes
# number of amap mostly dependant on issue of commune names, arrondissement
# not informative
# n_amap <- table(df$INSEE_COM)

# export --------------------------------------------------
## per commune
commune$AMAP_2026 <- as.numeric(commune$INSEE_COM %in% df$INSEE_COM)

png(
  file = file.path(fig_folder, "AMAP_2026_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "AMAP_2026",
  border = NA,
  main = "Presence of AMAP - 2026 - Commune",
)
dev.off()

# Distance to amap
pt_amap <- centroids(commune[commune$INSEE_COM %in% df$INSEE_COM])

# Compute the distance among training centers and centroids
pt_com <- centroids(commune)
# takes a long time with terra
distance_matrix <- distance(pt_amap, pt_com)
# get the nearest distance
dist_amap <- apply(distance_matrix, 2, min)

# boxplot(dist_ea ~ shp_ea)
commune$AMAP_DIST_KM_2026 <- ifelse(
  commune$AMAP_2026 == 1,
  0,
  round(dist_amap / 1000, 2)
)

png(
  file = file.path(fig_folder, "AMAP_DIST_KM_2026_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "AMAP_DIST_KM_2026",
  border = NA,
  main = "Distance to nearest AMAP (km) - 2026 - Commune",
)
dev.off()

write.csv(
  data.frame(commune),
  file.path(ind_folder, "COMMUNE_AMAP_2026.csv"),
  row.names = FALSE
)


# 3. Overlay and calculate statistics -----------------------------
# the intersect() step takes a very long time to compute at the French scale

# Same process for commune and maille
for (i in scales) {
  cat(paste("Maille", i, "km \n"))
  # load the data
  shp <- terra::vect(
    file.path(ref_folder, paste0("mailles_", i, "km_2154.gpkg"))
  )

  labi <- paste0("MAILLES", i, "km")

  # Check if traning center in polygons
  shp_ea <- is.related(shp, pt_amap, "intersects")

  # Compute the distance among AMAP and centroids
  shp_pt <- centroids(shp)
  # takes a long time with terra
  distance_matrix <- distance(pt_amap, shp_pt)
  # get the nearest distance
  dist_amap <- apply(distance_matrix, 2, min)

  shp$AMAP_DIST_KM_2026 <- ifelse(shp_ea, 0, round(dist_amap / 1000, 2))

  fi <- paste0("AMAP_DIST_KM_2026_", labi, ".png")
  png(
    file = file.path(fig_folder, fi),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    shp,
    y = "AMAP_DIST_KM_2026",
    border = NA,
    main = paste0("Distance to nearest AMAP (km) - 2026 - mailles_", i, "km")
  )
  dev.off()

  # export
  out_fi <- paste0(gsub("S", "", labi), "_AMAP_2026.csv")

  write.csv(
    data.frame(shp)[, names(shp) != "id"],
    file.path(ind_folder, out_fi),
    row.names = FALSE
  )
}

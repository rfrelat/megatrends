# Script to fasten the cross among grid and municipality

# input: (derived-data/ref/)
#   commune_4326.gpkg
#   mailles_10km_4326.gpkg
# output:
#   mailles_communes.csv

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)
library(exactextractr)

ref_folder <- here::here("data", "derived-data", "ref")

commune <- terra::vect(file.path(ref_folder, "commune_4326.gpkg"))
mailles <- terra::vect(file.path(ref_folder, "mailles_10km_4326.gpkg"))

# 2. Cross mailles and commune -----------------------------------

int <- intersect(commune, mailles)
dim(int)
dim(mailles)
dim(commune)
int$NEW_AREA <- expanse(int) * 0.0001

int_ha <- tapply(
  int$NEW_AREA,
  list(int$cd_sig, int$INSEE_COM),
  sum,
  na.rm = TRUE
)

dim(int_ha) # 5877 mailles, 34746 communes
# int_ha <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))
# int_ha[int_ha == 0] <- NA

# make sure the intersection match the row data
# not the case for communes in column
table(row.names(int_ha) == mailles$cd_sig, useNA = "ifany")
int_ha <- int_ha[, match(commune$INSEE_COM, colnames(int_ha))]
table(colnames(int_ha) == commune$INSEE_COM, useNA = "ifany")


# table(apply(int_ha > 0, 1, sum))
# table(apply(int_ha > 0, 2, sum))

mailles$NB_COMMUNES <- apply(!is.na(int_ha), 1, sum, na.rm = TRUE)
# boxplot(mailles$NB_COMMUNES)

png(
  file = file.path(fig_folder, "NB_COMMUNES_MAILLE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  mailles,
  y = "NB_COMMUNES",
  border = NA,
  breaks = seq(0, 50, by = 10),
  main = "Number of municipalities per cell"
)
dev.off()


commune$NB_CELLS <- apply(!is.na(int_ha), 2, sum, na.rm = TRUE)
# boxplot(mailles$NB_COMMUNES)

png(
  file = file.path(fig_folder, "NB_CELLS_COMMUNE.png"),
  width = 1200,
  height = 1000,
  res = 200
)
plot(
  commune,
  y = "NB_CELLS",
  border = NA,
  breaks = c(1, 2, 5, 10, 15, 20),
  main = "Number of cells per municipalities"
)
dev.off()


# in csv the file is huge 400Mb
# write.csv(
#   int_ha,
#   file = file.path(ref_folder, "cross_mailles_commune.csv")
# )

# much smaller in rds (because moslty filled with 0)
saveRDS(
  int_ha,
  file = file.path(ref_folder, "cross_mailles_commune.rds")
)

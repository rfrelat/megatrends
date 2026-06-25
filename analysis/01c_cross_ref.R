# Script to fasten the cross among grid and municipality

# input: (derived-data/ref/)
#   commune_4326.gpkg
#   mailles_Xkm_4326.gpkg
# output:
#   cross_maillesXkm_commune.rds

# 1. Load and set parameters -------------------------------------
devtools::load_all()
library(terra)

ref_folder <- here::here("data", "derived-data", "ref")
fig_folder <- here::here("figure")

commune <- terra::vect(file.path(ref_folder, "commune_4326.gpkg"))
scales <- c(10, 5, 1) # in km, resolution of the mailles
# not working for 1km, computations are too big: 554252 x 37000 = 20507324000
# 2. Cross mailles and commune -----------------------------------
for (i in scales) {
  mailles <- terra::vect(
    file.path(ref_folder, paste0("mailles_", i, "km_4326.gpkg"))
  )

  int <- terra::intersect(commune, mailles)

  int$NEW_AREA <- terra::expanse(int) * 0.0001

  int_ha <- tapply(
    int$NEW_AREA,
    list(int$cd_sig, int$INSEE_COM),
    sum,
    na.rm = TRUE
  )

  dim(int_ha) # 5877 mailles, 34746 communes
  # or 22809 celles, 34746 communes

  # int_ha <- readRDS(file.path(ref_folder, "cross_mailles_commune.rds"))
  # int_ha[int_ha == 0] <- NA

  # make sure the intersection match the row data
  # not the case for communes in column
  int_ha <- int_ha[match(mailles$cd_sig, row.names(int_ha)), ]
  # table(row.names(int_ha) == mailles$cd_sig, useNA = "ifany")
  int_ha <- int_ha[, match(commune$INSEE_COM, colnames(int_ha))]
  # table(colnames(int_ha) == commune$INSEE_COM, useNA = "ifany")

  mailles$NB_COMMUNES <- apply(!is.na(int_ha), 1, sum, na.rm = TRUE)
  # boxplot(mailles$NB_COMMUNES)

  png(
    file = file.path(fig_folder, paste0("NB_COMMUNES_MAILLE", i, "km.png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    mailles,
    y = "NB_COMMUNES",
    border = NA,
    main = paste("Number of municipalities per cell", i, "km")
  )
  dev.off()

  commune$NB_CELLS <- apply(!is.na(int_ha), 2, sum, na.rm = TRUE)
  # boxplot(commune$NB_CELLS)
  png(
    file = file.path(fig_folder, paste0("NB_CELLS", i, "km_COMMUNE.png")),
    width = 1200,
    height = 1000,
    res = 200
  )
  plot(
    commune,
    y = "NB_CELLS",
    border = NA,
    main = paste("Number of cells", i, "km per municipalities")
  )
  dev.off()

  saveRDS(
    int_ha,
    file = file.path(ref_folder, paste0("cross_mailles", i, "km_commune.rds"))
  )
}

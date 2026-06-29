# Script to fasten the cross among grid and municipality

# input: (derived-data/ref/)
#   commune_4326.gpkg
#   mailles_Xkm_4326.gpkg
# output:
#   cross_maillesXkm_commune.rds (1Mb to 10Mb)

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

  # need to avoid the wide format: too many zero for 1km maille
  # int_ha <- tapply(
  #   int$NEW_AREA,
  #   list(int$cd_sig, int$INSEE_COM),
  #   sum,
  #   na.rm = TRUE
  # )
  # 5877 cells, 34746 communes
  # or 22809 cells, 34746 communes
  # or 554193 cellsx 34746 communes= 19255989978 => CRASH
  # make sure the intersection match the row data
  # not the case for communes in column
  # int_ha <- int_ha[match(mailles$cd_sig, row.names(int_ha)), ]
  # table(row.names(int_ha) == mailles$cd_sig, useNA = "ifany")
  # int_ha <- int_ha[, match(commune$INSEE_COM, colnames(int_ha))]
  # table(colnames(int_ha) == commune$INSEE_COM, useNA = "ifany")

  # keep long format instead
  long_df <- data.frame(
    "cd_sig" = int$cd_sig,
    "INSEE_COM" = int$INSEE_COM,
    "AREA_HA" = int$NEW_AREA
  )
  # table(duplicated(long_df[, c("cd_sig", "INSEE_COM")]))
  # sum(long_df$AREA_HA) # 54905036
  # table(long_df$AREA_HA > 0)

  npix <- table(long_df$cd_sig)

  mailles$NB_COMMUNES <- npix[match(mailles$cd_sig, names(npix))]
  # apply(!is.na(int_ha), 1, sum, na.rm = TRUE)
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

  ncom <- table(long_df$INSEE_COM)

  commune$NB_CELLS <- ncom[match(commune$INSEE_COM, names(ncom))]
  # commune$NB_CELLS <- apply(!is.na(int_ha), 2, sum, na.rm = TRUE)
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
    long_df,
    file = file.path(ref_folder, paste0("cross_mailles", i, "km_commune.rds"))
  )
}

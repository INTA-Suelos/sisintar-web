library(SISINTAR)

perfiles <- get_perfiles(buscar_perfiles(), dir = "datos/temp")

data.table::fwrite(perfiles, "datos/datos_perfiles.csv")


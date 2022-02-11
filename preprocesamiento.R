datos_todos <- data.table::fread("datos/datos_perfiles.csv")


perfiles <- unique(datos_todos[, .(perfil_id,
                                   fecha = s_fecha,
                                   numero = s_perfil_no,
                                   clase = s_grupo,
                                   lon = s_lon,
                                   lat = s_lat)])

data.table::fwrite(perfiles, "datos/perfiles.csv")



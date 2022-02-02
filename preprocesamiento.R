datos_todos <- data.table::fread("datos/datos_perfiles.csv")


perfiles <- unique(datos_todos[, .(perfil_id, fecha = perfil_fecha,
                                   numero = perfil_numero,
                                   clase = perfil_grupo_descripcion,
                                   lon = perfil_ubicacion_longitud,
                                   lat = perfil_ubicacion_latitud)])

data.table::fwrite(perfiles, "datos/perfiles.csv")

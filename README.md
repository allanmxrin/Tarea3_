# Tarea3_allanmxrin

---
título: "Tarea 3: Allan Marín Campos" 
salida: 
  flexdashboard::flex_dashboard:
 orientación: columnas 
 vertical_layout: rellenar 
---

'''{r setup, include=FALSE} 
biblioteca(flexdashboard) 
biblioteca(DT) 
biblioteca(ggplot2) 
biblioteca(plotly) 
biblioteca(dplyr) 
biblioteca(folleto) 
biblioteca(sf) 
biblioteca(readxl) 
biblioteca(tidyverse) 
biblioteca(ggthemes) 
biblioteca(terra) 
biblioteca(readr) 
biblioteca(stringi) 
biblioteca(ggdark) 
```

'''{r, include=FALSE} 
delitos <-
 read_excel(ruta = "C:/Users/marin/OneDrive/Escritorio/Datos3/datospoliciales.xlsx") 
```

'''{r, incluye FALSE} 
delitos <-
 read_excel(ruta = "C:/Users/marin/OneDrive/Escritorio/Datos3/datospoliciales.xlsx") 

cantones <-
  st_read(dsn = "C:/Users/marin/OneDrive/Escritorio/Datos3/cantones_simplificados.geojson",
 silencioso = VERDADERO 
  ) %>%
  st_transform(4326)

cantones <-
  cantones %>%
  st_transform(5367) %>%
 st_simplify(dTolerancia = 100) %>% 
  st_transform(4326)

cantones <-
  cantones %>%
 mutate(canton_normalizado = tolower(stri_trans_general(cantón, id = "Latín-ASCII"))) 

delitos <-
  delitos %>%
 mutate(canton_normalizado = tolower(stri_trans_general(Cantón, id = "Latín-ASCII"))) 

delitos <-
  delitos %>%
 mutate(Cantón = if_else(Cantón == "LEON CORTES", "LEON CORTES CASTRO", Cantón)) %>% 
 mutate(Cantón = if_else(Cantón == "VÁSQUEZ DE CORONADO", "VÁZQUEZ DE CORONADO", Cantón)) 

delitos <-
  delitos %>%
 mutate(canton_normalizado = tolower(stri_trans_general(Cantón, id = "Latín-ASCII"))) 

delitos <-
  delitos %>%
  left_join(
    dplyr::select(
      st_drop_geometry(cantones),
      cod_canton,
      canton_normalizado
    ),
    by = "canton_normalizado",
    copy = FALSE,
    keep = FALSE
  )

delitos_x_canton <-
  delitos %>%
  count(cod_canton, name = "delitos")

cantones_delitos <-
  cantones %>%
  left_join(
    delitos_x_canton,
    by = "cod_canton",
    copy = FALSE,
    keep = FALSE
  )

colores_cantones_delitos <-
  colorNumeric(palette = "Reds",
               domain = cantones_delitos$delitos,
               na.color = "transparent")

```


Column {data-width=575}
-----------------------------------------------------------------------

### Mapa de Coropletas

```{r}
leaflet() %>%
  setView(
    lng = -84.19452,
    lat = 9.572735,
    zoom = 7) %>%
  addTiles(group = "OpenStreetMap") %>% 
  addPolygons(
    data = cantones_delitos,
    fillColor = ~ colores_cantones_delitos(cantones_delitos$delitos),
    fillOpacity = 0.6,
    color = "black",
    stroke = TRUE,
    weight = 1.0,
    popup = paste(
      paste(
        "<strong>Cantón:</strong>",
        cantones_delitos$canton
      ),
      paste(
        "<strong>Delitos:</strong>",
        cantones_delitos$delitos
      ),
      sep = '<br/>'
    ),
    group = "Delitos en cantones"
  ) %>%
  addLayersControl(
    baseGroups = c("OpenStreetMap"),
    overlayGroups = c("Delitos en cantones")
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = colores_cantones_delitos,
    values = cantones_delitos$delitos,
    group = "Delitos",
    title = "Cantidad de delitos"
  )
```


### Tabla DT
```{r}
delitos %>%
  dplyr::select(Delito,
                Fecha,
                Victima,
                Edad,
                Genero,
                Provincia,
                Canton) %>%
  mutate(Fecha = as.Date(Fecha, format = "%d/%m/%Y")) %>%
  datatable(
    options = list(
      pageLength = 20,
      language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
    ),
    colnames = c(
      # encabezados de las columnas
      "Delito",
      "Fecha",
      "Víctima",
      "Edad",
      "Género",
      "Provincia",
      "Cantón"
    )
  )
```


Columna {data-width=425, .tabset} 
-----------------------------------------------------------------------

### Gráfico sobre Delitos por tipo de delito

'''{r} 
Cantidad_de_delito_por_tipo <-
  delitos %>%
 conde(Delito) %>% 
 ggplot(aes(x = reorden(Delito, n), y = n)) + 
 geom_bar(stat = "identidad") + 
  ggtitle("Delitos por tipo de delito") +
  xlab("Patrón de Delito") +
  ylab("Cantidad") +
  coord_flip() +
  theme_classic() +
 theme(legend.position = "arriba") 

Cantidad_de_delito_por_tipo %>%
  ggplotly() %>%
  config(locale = "es")
```

### Gráfico sobre Delitos por Género
'''{r} 
Proporcion_de_Delito_por_Genero <-
  delitos %>%
  ggplot(aes(x = Delito, fill = Genero)) +
 geom_bar(posición = "relleno") + 
  ggtitle("Proporción de delito por Género") +
  xlab("Delito") +
  ylab("Proporción") +
  labs(fill = "Género") +
  coord_flip() +
  theme_few() +
 theme(legend.position = "arriba") 

ggplotly(Proporcion_de_Delito_por_Genero) %>% config(locale = 'es')
```


### Gráfico sobre Delitos por mes
'''{r} 
ggplot2_delitos_por_mes <-
  delitos %>%
  mutate(fecha = lubridate::month(Fecha))
mes <-
  c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
  )
ggplot2_delitos_por_mes <- ggplot2_delitos_por_mes %>%
  count(fecha) %>%
  ggplot(aes(x = fecha, y = n)) +
 geom_bar(stat = "identidad") + 
  ggtitle("Cantidad de delitos por mes") +
  xlab("Mes") +
  ylab("Cantidad") +
  theme_grey() +
 theme(legend.position = "arriba") 

ggplotly(ggplot2_delitos_por_mes) %>% config(locale = 'es')
```



### Gráfico sobre Delitos por victima
'''{r} 
delitos_por_victima <-
  delitos %>%
  count(Victima) %>%
 ggplot(aes(x = reorden(Victima, n), y = n)) + 
 geom_bar(stat = "identidad") + 
  ggtitle("Delitos por víctima") +
  xlab("Tipo de Víctima") +
  ylab("Cantidad") +
  coord_flip() +
  theme_grey() +
 theme(legend.position = "arriba") 

delitos_por_victima %>%
  ggplotly() %>%
  config(locale = "es")
```

  library(ggplot2)
  library(sf)
  library(readxl)
  library(dplyr)
  library(gridExtra)
  library(writexl)
  library(stringr)
  library(tidyr)
  library(psych)
  library(scales)
  library(cowplot)

  setwd("~/consulta_externa")

  # shape map
  peru_shp <- st_read("peru.shp")

  # atenciones
atenciones <- read_excel("raw/maps.xlsx", sheet = "atenciones_2", skip = 1)
glimpse(atenciones)
atenciones <- atenciones %>%
mutate(departamento_EESS = toupper(trimws(departamento_EESS)))

write_xlsx(atenciones, "raw/atenciones_region.xlsx")
atenciones <- read_excel("raw/atenciones_region.xlsx")

  # atendidos
atendidos <- read_excel("raw/maps.xlsx", sheet = "atendidos_2", skip = 1)
glimpse(atendidos)
atendidos <- atendidos %>%
mutate(departamento_EESS = toupper(trimws(departamento_EESS)))
write_xlsx(atendidos, "raw/atendidos_region.xlsx")
atendidos <- read_excel("raw/atendidos_region.xlsx")

# importar atenciones y atendidos
atenciones <- read_xlsx("raw/atenciones_region.xlsx")
atendidos <- read_xlsx("raw/atendidos_region.xlsx")

# join atenciones y atendidos
data <- atenciones %>%
    left_join(atendidos, by = "departamento_EESS")

write_xlsx(data, "raw/full_atenciones_atendidos.xlsx")
data <- read_xlsx("raw/full_atenciones_atendidos.xlsx") #empezar desde aqui

glimpse(data)
# crear indicador atenciones/atendidos por año 2021, 2022, 2023
data <- data %>%
    mutate(ind21 = atenciones2021 / atendidos2021,
           ind22 = atenciones2022 / atendidos2022,
           ind23 = atenciones2023 / atendidos2023)
glimpse(data)
getwd()
hist(data$ind21)
#ver curtosis y sesgo
describe(data$ind21)
hist(data$ind22)
describe(data$ind22)
hist(data$ind23) #no normal
describe(data$ind23)
quantiles_ind23 <- quantile(data$ind23, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
print(quantiles_ind23)

write_xlsx(data, "raw/full_atenciones_atendidos.xlsx")

  # formateando peru shape
  peru_shp <- peru_shp %>%
      mutate(DEPARTAMENTO = toupper(trimws(departamen))) %>%
      group_by(DEPARTAMENTO) %>%
      summarize(geometry = st_union(geometry), .groups = "drop")

  # cambiar nombre de DEPARTAMENTO en peru_shp
  peru_shp <- peru_shp %>%
      rename(departamento_EESS = DEPARTAMENTO)

# minusculas y primera letra en mayusculas var departamento_EESS
    atenciones$departamento_EESS <- tolower(atenciones$departamento_EESS) %>%
      str_to_title()
    atendidos$departamento_EESS <- tolower(atendidos$departamento_EESS) %>%
        str_to_title()
    peru_shp$departamento_EESS <- tolower(peru_shp$departamento_EESS) %>%
        str_to_title()

    data$departamento_EESS <- tolower(data$departamento_EESS) %>%
    str_to_title()
    
  glimpse(data)
  glimpse(peru_shp)

  # Merge con shape
  atenciones_map <- left_join(peru_shp, atenciones, by = "departamento_EESS")
  atendidos_map <- left_join(peru_shp, atendidos, by = "departamento_EESS")

  indicador_map <- left_join(peru_shp, data, by = "departamento_EESS")
glimpse(indicador_map)

  #######################################INDICADORES 2021-23############################################
 glimpse(indicador_map)
 # Indicador 21
  map1 <- ggplot(data = indicador_map) +
      geom_sf(aes(geometry = geometry, fill = ind21), color = "gray", linewidth = 0.05) +
      geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
      scale_fill_gradient(
          low = "white", high = "red", na.value = "white",
          name = "Atenciones/atendidos", labels = comma,
          limits = c(0, 12)
      ) +
     labs(
         title = "2021"
         #fill = "ind21"
     ) +
      theme_minimal() +
      theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.title = element_text(size = 10, family = "", color = "black"),
          legend.text = element_text(size = 8, family = "", color = "black"),
          legend.key.size = unit(0.5, "cm"),
          legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
      ) 

  # Indicador 22
    map2 <- ggplot(data = indicador_map) +
        geom_sf(aes(geometry = geometry, fill = ind22), color = "gray", linewidth = 0.05) +
        geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
        scale_fill_gradient(
            low = "white", high = "red", na.value = "white",
            name = "Atenciones/atendidos", labels = comma,
            limits = c(0, 12)
        ) +
        labs(
           title = "2022"
           #fill = "ind22"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 10, family = "", color = "black"),
            legend.text = element_text(size = 8, family = "", color = "black"),
            legend.key.size = unit(0.5, "cm"),
            legend.position = "none",
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()
        )

# indicador 23
    map3 <- ggplot(data = indicador_map) +
        geom_sf(aes(geometry = geometry, fill = ind23), color = "gray", linewidth = 0.05) +
        geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
        scale_fill_gradient(
            low = "white", high = "red", na.value = "white",
            name = "Atenciones/atendidos", labels = comma,
            limits = c(0, 12)
        ) +
        labs(
            title = "2023"
            #fill = "ind23"
        ) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
            legend.title = element_text(size = 10, family = "", color = "black"),
            legend.text = element_text(size = 8, family = "", color = "black"),
            legend.key.size = unit(0.5, "cm"),
            legend.position = "none", #mantener leyenda
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank()
        )

 # extraer leyenda de un mapa
legend <- cowplot::get_legend(map3)

# Combine maps
combined_plot <- plot_grid(
    plot_grid(map1, map2, map3, ncol = 3),
    legend,
    rel_widths = c(3, 0.5)
)
print(combined_plot)
  # mejorar calidad tiff

  ggsave("figuras/indicador_2021-23.jpg", combined_plot, width = 13, height = 9, dpi = 300)


##################################  2021  ##################
    
  atendidos.map <- ggplot(data = atendidos_map) +
      geom_sf(aes(geometry = geometry, fill = atendidos2021), color = "gray", linewidth = 0.05) +
      geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
      scale_fill_gradient(
          low = "white", high = "blue", na.value = "white",
          name = "Atendidos", labels = comma
      ) +
      labs(
          title = "Atendidos en consulta externa, Perú, 2021 (n =  184,415)",
          fill = "atendidos2021"
      ) +
      theme_minimal() +
      theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.title = element_text(size = 10, family = "", color = "black"),
          legend.text = element_text(size = 8, family = "", color = "black"),
          legend.key.size = unit(0.5, "cm"),
          legend.position = "right",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()
      )

  # Combine maps
  combined_map <- grid.arrange(map1, map2, map3, ncol = 3)

  # Save
  # mejorar calidad tiff 
  
  ggsave("figuras/maps2021.tiff", combined_map, width = 13, height = 9, dpi = 300)


#################################2022############################################
# Atenciones map
atenciones.map2 <- ggplot(data = atenciones_map) +
    geom_sf(aes(geometry = geometry, fill = atenciones2022), color = "gray", linewidth = 0.05) +
    geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
    scale_fill_gradient(
        low = "white", high = "red", na.value = "white",
        name = "Atenciones", labels = comma
    ) +
    labs(
        title = "Atenciones en consulta externa, Perú, 2022 (n = 1,392,431)",
        fill = "atenciones_2022"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10, family = "", color = "black"),
        legend.text = element_text(size = 8, family = "", color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

# Atendidos map
atendidos.map2 <- ggplot(data = atendidos_map) +
    geom_sf(aes(geometry = geometry, fill = atendidos2022), color = "gray", linewidth = 0.05) +
    geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
    scale_fill_gradient(
        low = "white", high = "blue", na.value = "white",
        name = "Atendidos", labels = comma
    ) +
    labs(
        title = "Atendidos en consulta externa, Perú, 2022 (n =  223,792)",
        fill = "atendidos2022"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10, family = "", color = "black"),
        legend.text = element_text(size = 8, family = "", color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

# Combine maps
combined_map <- grid.arrange(atenciones.map2, atendidos.map2, ncol = 2)

# Save
# mejorar calidad tiff

ggsave("figuras/maps2022.tiff", combined_map, width = 13, height = 9, dpi = 300)


################################# 2023############################################
# Atenciones map
atenciones.map3 <- ggplot(data = atenciones_map) +
    geom_sf(aes(geometry = geometry, fill = atenciones2023), color = "gray", linewidth = 0.05) +
    geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
    scale_fill_gradient(
        low = "white", high = "red", na.value = "white",
        name = "Atenciones", labels = comma
    ) +
    labs(
        title = "Atenciones en consulta externa, Perú, 2023 (n = 1,571,093)",
        fill = "atenciones_2023"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10, family = "", color = "black"),
        legend.text = element_text(size = 8, family = "", color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

# Atendidos map
atendidos.map3 <- ggplot(data = atendidos_map) +
    geom_sf(aes(geometry = geometry, fill = atendidos2023), color = "gray", linewidth = 0.05) +
    geom_sf_text(aes(geometry = geometry, label = departamento_EESS), size = 2, color = "black") +
    scale_fill_gradient(
        low = "white", high = "blue", na.value = "white",
        name = "Atendidos", labels = comma
    ) +
    labs(
        title = "Atendidos en consulta externa, Perú, 2023 (n = 285,838)",
        fill = "atendidos2023"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.title = element_text(size = 10, family = "", color = "black"),
        legend.text = element_text(size = 8, family = "", color = "black"),
        legend.key.size = unit(0.5, "cm"),
        legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

# Combine maps
combined_map <- grid.arrange(atenciones.map3, atendidos.map3, ncol = 2)

# Save
ggsave("figuras/maps2023.jpg", combined_map, width = 13, height = 9, dpi = 300)

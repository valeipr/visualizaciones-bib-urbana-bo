# cargamos librerias
library(tidyverse)
library(openxlsx)
library(packcircles)
library(viridisLite)
library(viridis)
library(tidytext)

# cargamos datasets
publicaciones <- read_csv("data/publicaciones.csv")
autores <- read_csv("data/autores.csv")
editoriales <- read_csv("data/edits.csv")
temas <- read_csv("data/temas.csv")
disciplinas <- read_csv("data/disciplinas.csv")

# 1. prop tipo_doc
publicaciones %>%
  count(tipo_doc) %>%
  arrange(desc(n)) %>%
  mutate(prop_tipodoc = (prop.table(n) * 100)) %>%
  mutate(prop_tipodoc = round(prop_tipodoc, 2)) %>%
  ggplot(aes(x = fct_reorder(tipo_doc, prop_tipodoc), y = prop_tipodoc)) +
  geom_col(alpha = 0.75, fill = "#6BAA75") +
  coord_flip()+
  geom_label(aes(label = paste0(round(prop_tipodoc,0), "%")), size = 4) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggsave("figs/1-tipodoc.png")

#2. histograma de productividad
publicaciones %>%
  mutate("time" = 
           case_when(año < 1980 ~ "1970-1980",
                     año >= 1980 & año < 1990 ~ "1980-1990",
                     año >= 1990 & año < 2000 ~ "1990-2000",
                     año >= 2000 & año < 2010 ~ "2000-2010",
                     año >= 2010 & año ~ "2010-actualidad",)) %>%
  count(time) %>%
  ggplot(aes(x = time, y = n)) +
  geom_col(alpha = 0.75, fill = "#AE8E1C") +
  geom_label(aes(label = n)) +
  labs(title = "", x = NULL, y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10))

ggsave("figs/2-incremento.png")

#3. productividad de los autores
autores_productivos <- autores %>%
  pivot_longer(autor_1:autor_13, 
               names_to = "n_autor",
               values_to = "autor") %>%
  drop_na(autor) %>%
  count(autor) %>%
  rename(nombre = autor) %>%
  arrange(desc(n), nombre) %>%
  slice_head(n = 25)  # ojo: aqui salen 512, no 504. Es importante que tengamos cuidado con eso para el trabajo de antropologia!

write_csv(autores_productivos, "output/04_autores_productivos.csv")

# 4. Editoriales
editoriales_produccion <- editoriales %>%
  pivot_longer(edit_1:edit_7,
               names_to = "n_edit",
               values_to = "editoriales")%>%
  drop_na(editoriales) %>%
  count(editoriales) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  mutate(prop_produccion = (prop.table(n) * 100)) %>%
  mutate(prop_produccion = round(prop_produccion, 2))

# calculamos el area y el centro del circulo, luego unimos 
packing <- circleProgressiveLayout(editoriales_produccion$n, sizetype = 'area')
edits_productivo <- cbind(editoriales_produccion, packing)

burbujas_edit <- circleLayoutVertices(packing, npoints = 50)

ggplot() + 
  geom_polygon(data = burbujas_edit, 
               aes(x, y, group = id, fill = id), 
               colour = "black", 
               alpha = 0.6) +
  scale_fill_distiller(palette = "BuPu") +
  geom_text(data = edits_productivo, 
            aes(x, y, label = editoriales),
            size = 4.5) + # x y y son las coordenadas donde estara la etiqueta.
  scale_size_continuous(range = c(1,4)) + #preguntar a Ale que hace esto
  theme_void()+
  theme(legend.position="none") +
  coord_equal() +
  labs(title = "")

ggsave("figs/3-instituciones.png")

# pareto
edits_pareto <- editoriales %>%
  pivot_longer(edit_1:edit_7,
               names_to = "n_edit",
               values_to = "editoriales")%>%
  drop_na(editoriales) %>%
  count(editoriales) %>%
  arrange(desc(n)) %>%
  mutate(prop_produccion = round((prop.table(n) * 100), 2))

# 20% que produce el 80%
mas_productivos <- edits_pareto %>%
  slice_head(n=18)
d <- sum(mas_productivos$prop_produccion)
# 80% que produce el 20%
menos_productivos <- edits_pareto %>%
  slice_tail(n=74)

f <- sum(menos_productivos$prop_produccion)

# unimos las proporciones en un vector chikito
publicaciones_acumuladas <- c(d,f)
instituciones <- c("20% con mas publicaciones", "80% con menos publicaciones")
tabla_acumulacion <- data.frame(instituciones,publicaciones_acumuladas)

write_csv(tabla_acumulacion, "output/tabla_pareto.csv")

# 4. temas
temas %>%
  pivot_longer(e_1:e_3, names_to = "n_etiq", values_to = "temas") %>%
  drop_na(temas) %>%
  count(temas) %>%
  arrange(desc(n)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(temas, n), y = n)) +
  geom_col(alpha = 0.7, fill = "#7552A3") +
  coord_flip()+
  geom_label(aes(label = n), size = 3) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggsave("figs/4-15temas.png")

# 5. ciudades mas investigadas
temas %>%
  count(Ciudad) %>%
  arrange(desc(n)) %>%
  mutate(prop_ciudad = (prop.table(n) * 100)) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = fct_reorder(Ciudad, prop_ciudad), y = prop_ciudad)) +
  geom_col(alpha = 0.7, fill = "#2160D4") +
  coord_flip()+
  geom_label(aes(label = paste0(round(prop_ciudad,0), "%"))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

ggsave("figs/5-ciudades.png")

# 6. facetado por ciudad y tema
temas %>%
  pivot_longer(e_1:e_3, names_to = "n_etiq", values_to = "temas") %>%
  drop_na(temas) %>%
  select(Ciudad, temas) %>%
  group_by(Ciudad) %>%
  count(temas, sort = TRUE) %>%
  arrange(Ciudad) %>%
  filter(Ciudad %in% c("Cochabamba", "La Paz", "Santa Cruz", "El Alto")) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder_within(temas, n, Ciudad),y = n, fill = Ciudad), alpha = 0.6) +
  geom_col(show.legend = F, alpha = 0.8) +
  facet_wrap(~Ciudad, scales = "free_y", ncol = 2) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = NULL) +
  scale_fill_manual(values = c("#6DBEC5", "#F4D35E", "#D16666", "#A9BA3B")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_minimal()

ggsave("figs/6-temasciudades.png")

#datos disciplinas

disciplinas %>%
  count(rama) %>%
  arrange(desc(n)) %>%
  head(n = 6) %>%
  ggplot(aes(x = fct_reorder(rama, n), y = n, fill = rama)) +
  geom_col(show.legend = FALSE, alpha = 0.7) +
  scale_fill_manual(values = c("#A9BA3B", "#C3BABA", "#C3BABA", "#C3BABA", "#C3BABA", "#C3BABA")) +
  geom_label(aes(label = n), show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y= NULL) +
  theme(axis.text.y = element_text(size = 10))

ggsave("figs/7-antropologia.png")

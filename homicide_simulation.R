library(tidyverse)
library(magrittr)
library(zoo)
library(scales)
library(lubridate)
#install.packages("gganimate")
library(gganimate)
#install.packages("rust")
#install.packages("sf")
library(sf)
#install.packages("gifski")
#install.packages("transformr")

estatal <- read_csv("Estatal Victimas - agosto 2019.csv")

meses <- c(
    "Enero",
    "Febrero",
    "Marzo",
    "Abril",
    "Mayo",
    "Junio",
    "Julio",
    "Agosto",
    "Septiembre",
    "Octubre",
    "Noviembre",
    "Diciembre"
)



estatal %>%
    filter(`Tipo de delito` %in% c("Homicidio", "Feminicidio")) %>%
    filter(`Subtipo de delito` != "Homicidio culposo") %>%
    select(Año, Clave_Ent, Entidad, meses) %>%
    pivot_longer(meses, names_to = "Mes", values_to = "Homicidios") %>%
    group_by(Año, Clave_Ent, Entidad, Mes) %>%
    summarise(Homicidios = sum(Homicidios)) %>%
    mutate(Mes = factor(Mes, levels = meses)) %>%
    left_join(tibble(Mes = meses, mes_id = 1:12)) %>%
    arrange(Año, mes_id) -> homicidios_feminicidios_estatal_mensual

# join with poblacion

load("~/R/homicideconcentration/basepryentMX.rdata")

names(basepryentMX)[2] <- "año"

basepryentMX <- as_tibble(basepryentMX)

## Chose only the pob form one year and keep it steady just for illustrative purposes

basepryentMX %>%
    filter(ent != "Nacional") %>%
    filter(año == 2018) %>%
    group_by(año, ent, id_ent) %>%
    summarise(pob_anual = sum(pob)) %>%
    ungroup %>%
    select(Clave_Ent = id_ent, pob_anual)  -> pobs_2018

homicidios_feminicidios_estatal_mensual %>% 
    mutate(mes_id_text = ifelse(mes_id  > 9, as.character(mes_id), paste0("0", mes_id)),
        ym = as.Date(paste(Año, mes_id_text, "01", sep = "-"))) %>% 
    select(-mes_id_text) %>% 
    drop_na() %>%
    filter(ym > as.Date("2018-07-01") & ym < as.Date("2019-10-01")) %>%
    left_join(pobs_2018) %>%
    mutate(ldays = days_in_month(ym),
           tasa_homs_pob = Homicidios/pob_anual,
           tasa_pob_dia = tasa_homs_pob/ldays,
           tasa_dia = Homicidios/ldays) -> homs_estatal_ago2018_ago2019

homs_estatal_ago2018_ago2019 %>%
    ggplot(aes(ym, tasa_pob_dia, colour = Entidad)) +
    geom_point() + 
    geom_line() + 
    facet_wrap(~Entidad) +
    scale_y_sqrt() + 
    theme(legend.position = "none") 
    
daily_df <- data_frame(ident = rep(1:32, each = 396), 
                       day = rep(as.Date(as.Date("2018-08-01"):as.Date("2019-08-31")), 32))


homs_estatal_ago2018_ago2019 %>%
    ungroup %>%
    mutate(ymd = ym,
           ym = as.yearmon(ymd)) %>%
    select(Clave_Ent, Entidad, tasa_homs_pob, tasa_pob_dia, tasa_dia, ym) %>%
    right_join(transmute(daily_df, ym = as.yearmon(day),
                         Clave_Ent = ident,
                         day =  day)) -> tasas_diarias

## simulate
set.seed(42)
B = 10000
n <- nrow(tasas_diarias)

replicate(B,
    rpois(n, tasas_diarias$tasa_dia)) -> homs_simul

simulcols <- paste0("X", 1:10000)

tasas_diarias %>%
    select(Clave_Ent, Entidad, tasa_dia, day) %>%
    bind_cols(data.frame(homs_simul)) %>%
    pivot_longer(simulcols, names_to = "replicate") -> homicidios_simulados

homicidios_simulados %>%
    group_by(Entidad, day) %>%
    sample_n(1) -> homicidios_simulados_1

(homicidios_simulados_1 %>%
    ggplot(aes(day, value, colour = Entidad)) +
    #geom_point(alpha = 0.2) +
    geom_line(alpha = 0.5) +
    geom_smooth(se = FALSE) +
    #geom_line(aes(y = tasa_dia), size = 1) +
    facet_wrap(~Entidad) +
    scale_y_sqrt() +
    theme(legend.position = "none") -> homs_sim_plot)


# homs_sim_plot + 
#     transition_reveal(along = day)

### create map

mex <- st_read(dsn = "Capas_estados/Estados.shp")

# simplify mex map

st_simplify(mex, dTolerance = 5000) -> mex_simple

ggplot(mex_simple) + geom_sf() 

fac_to_num <- function(x) {
    as.numeric(as.character(x))
}


mex_simple %>%
    mutate(Clave_Ent = fac_to_num(CVE_ENT)) -> mex_simple_cve

(mex_simple_cve %>%
right_join(homicidios_simulados_1) -> homicidios_simulados_map)


homicidios_simulados_map %>%
    group_by(CVE_ENT) %>%
    summarise(homicidios = sum(value)) %>%
    ggplot() +
    geom_sf(aes(fill = homicidios)) -> map1

homicidios_simulados_map %>%
    mutate(disc_value = fct_rev(
                            fct_lump(
                                factor(value), 5, 
                                other_level = "5+"))) -> homs_sim_map_disc

fill_scale <- rev(RColorBrewer::brewer.pal(6, "Reds"))

homs_sim_map_disc %>%
    filter(day == "2018-08-01") %>%
    ggplot() +
    geom_sf(aes(fill = value), colour = "white") +
    scale_fill_gradient("Simulated \nhomicides",
                        low = "#ffe5e5", 
                        high = "#cc0000",
                        trans = "sqrt",
                        breaks = c(0, 1, 5, 10))

homs_sim_map_disc %>%
    ggplot() +
    geom_sf(aes(fill = value), colour = "white") +
    scale_fill_gradient("Simulated \nhomicides",
                        low = "#ffe5e5", 
                        high = "#cc0000",
                        trans = "sqrt",
                        breaks = c(0, 1, 5, 10)) -> daily_hom_map


daily_hom_map + 
    transition_time(day) +
    exit_fade() -> daily_hom_map_anim

anim_save(filename = "homicides_animation.gif",
          animation = animate(daily_hom_map_anim,
                              nframes = 396,
                              fps = 3),
          width = 800,
          height = 600)




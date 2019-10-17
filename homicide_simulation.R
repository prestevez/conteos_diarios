library(tidyverse)
library(magrittr)
library(zoo)
library(scales)
library(lubridate)

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
    left_join(data_frame(Mes = meses, mes_id = 1:12)) %>%
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
    
daily_df <- data_frame(ident = rep(1:32, each = 396), day = rep(as.Date(as.Date("2018-08-01"):as.Date("2019-08-31")), 32))


homs_estatal_ago2018_ago2019 %>%
    ungroup %>%
    mutate(ymd = ym,
           ym = as.yearmon(ymd)) %>%
    select(Clave_Ent, Entidad, tasa_homs_pob, tasa_pob_dia, tasa_dia, ym) %>%
    right_join(transmute(daily_df, ym = as.yearmon(day),
                         Clave_Ent = ident,
                         day =  day)) -> tasas_diarias

## simulate

B = 10000
n <- nrow(tasas_diarias)

replicate(B,
    rpois(n, tasas_diarias$tasa_dia)) -> homs_simul

simulcols <- paste0("X", 1:10000)

tasas_diarias %>%
    select(Entidad, tasa_dia, day) %>%
    bind_cols(data.frame(homs_simul)) %>%
    pivot_longer(simulcols, names_to = "replicate") -> homicidios_simulados

homicidios_simulados %>%
    ggplot(aes(day, value, colour = Entidad)) + 
    geom_point(alpha = 0.1) +
    geom_smooth(se = FALSE) +
    geom_line(aes(y = tasa_dia), linetype = 2) +
    facet_wrap(~Entidad) +
    #scale_y_sqrt() + 
    theme(legend.position = "none") -> homs_sim_plot

ggsave(homs_sim_plot, filename = "homicide_simulation.png", width = 11, height = 8)

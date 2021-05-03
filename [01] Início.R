rm(list = ls())

# INÍCIO
#==============================================================================
# Pacotes -----------------------------------------------------------------

library(countrycode)
library(seasonal)
library(readxl)
library(parsedate)
library(tidyverse)
library(mFilter)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(modelr)
library(tsibble)
library(tsibbledata)
library(feasts)
library(fabletools)
library(fable)
library(forecast)
library(sidrar)
library(xts)


# detach("package:vars", unload = TRUE)
# detach("package:MASS", unload = TRUE)

# Cores RGB ---------------------------------------------------------------

pal_v  <- paletavermelha <- c("#660000", "#990000", "#CC0000", "#FF0000", "#FF3333", "#FF6666", "#FF9999", "#FFCCCC")
pal_a  <- paletaazul     <- c("#003366", "#004C99", "#0066CC", "#0080FF", "#3399FF", "#66B2FF", "#99CCFF", "#CCE5FF")
pal_vd <- paletaverde    <- c("#336600", "#4C9900", "#66CC00", "#80FF00", "#99FF33", "#82FF66", "#CCFF99", "#E5FFCC")
pal_c  <- paletacinza    <- c("#000000", "#202020", "#404040", "#606060", "#808080", "#A0A0A0", "#C0C0C0", "#E0E0E0")
fundo  <- theme(plot.background  = element_rect(fill = "#f4f5f6", color = NA), 
                panel.background = element_rect(fill = "#f4f5f6", color = NA))


# Temas GGPLOT -------------------------------------------------------------------

temasggplot <- function(fonte         = "Arial", 
                        cor           = paletacinza[1],
                        titlesize     = 11, 
                        subtitlesize  = 10, 
                        captionsize   = 8,
                        axistextsize  = 9, 
                        axistitlesize = 9) {
  
  g1 <<- theme(panel.grid.major     = element_blank(), 
               panel.grid.minor     = element_blank(), 
               panel.background     = element_rect(fill = "#FFFFFF", color = cor, size = 0.5), 
               plot.title           = element_text(family = fonte, color = cor, size = titlesize, face = "bold", margin = margin(t = 5, b = 2, unit = "pt")),
               plot.subtitle        = element_text(family = fonte, color = cor, size = subtitlesize),
               plot.caption         = element_text(family = fonte, color = cor, size = captionsize, margin = margin(b = 5, unit = "pt")),
               plot.margin          = unit(c(0,12,0,0), unit = "pt"),
               axis.line.x.bottom   = element_blank(), 
               axis.line.x.top      = element_blank(), 
               axis.line.y.left     = element_blank(),
               axis.line.y.right    = element_blank(), 
               axis.text.y          = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(l = 5, unit = "pt")),
               axis.text.x          = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(b = 5, unit = "pt")),
               axis.title.y         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.9, margin = margin(l = 3, unit = "pt")),
               axis.title.x         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.1, margin = margin(b = 3, unit = "pt")),
               legend.background    = element_blank(), 
               legend.key           = element_rect(fill = "#FFFFFF", size= 0.5), 
               legend.position      = c(0,1), 
               legend.justification = c(0,1)); g_1 <<- g1
               
               g2 <<- g1 + theme(panel.background     = element_blank(), 
                                 axis.line.y.left     = element_line(color = cor, size = 0.5), 
                                 axis.line.x.bottom   = element_line(color = cor, size = 0.5)); g_2 <<- g2
               
               g3 <<- g2 + theme(panel.grid.major.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2), 
                                 panel.grid.major.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2)); g_4 <<- g3
               
}; temasggplot()

leg01 <- theme(legend.justification = c(0,1), legend.position = c(0,1))
leg00 <- theme(legend.justification = c(0,0), legend.position = c(0,0))
leg11 <- theme(legend.justification = c(1,1), legend.position = c(1,1))
leg10 <- theme(legend.justification = c(1,0), legend.position = c(1,0))


g_3 <- theme_bw() + theme(panel.border      = element_rect(size = 1.3, color = "#000000"),
                          panel.grid.major  = element_blank(), 
                          panel.grid.minor  = element_blank(), 
                          plot.title        = element_text(size = 14, hjust = 0, face = "bold"),
                          plot.subtitle     = element_text(size = 11, hjust = 0),
                          panel.grid        = element_line(size = 1, color = "#000000"),
                          axis.ticks        = element_line(colour = "black"), 
                          axis.line         = element_line(color = "#000000"),
                          axis.title.y      = element_text(colour = "black", size = 10, hjust = 0.5, face = "bold"), 
                          axis.title.x      = element_blank(), 
                          axis.text.y       = element_text(size = 10, colour = "black", face = "bold"), 
                          axis.text.x       = element_text(size = 10, colour = "black", face = "bold", angle = 0, vjust = 0.5, hjust = 0.5),
                          legend.title      = element_text(size = 13, colour = "black", face = "bold"),
                          legend.background = element_blank(),
                          legend.key        = element_rect(colour = "black"),
                          legend.text       = element_text(size = 11, face = "bold"), 
                          plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g4 <- g_4 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                panel.grid.minor  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_text(colour = "#000000", size = 9, hjust = 0.1, face = "bold"), 
                                axis.title.x      = element_text(colour = "#808080", size = 9, hjust = 0.1, face = "bold"), 
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g5 <- g_5 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_blank(),
                                panel.grid.minor  = element_blank(),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_blank(),
                                axis.title.x      = element_blank(),
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 



# geom_label_repel


# Regiões, Estados -----------------------------------------------------------------

Estados <- 
  tibble(UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", 
                "MG", "ES", "RJ", "SP", 
                "PR", "SC", "RS", 
                "MS", "MT", "GO", "DF"), 
         Nome = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", 
                  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", 
                  "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
                  "Paraná", "Santa Catarina", "Rio Grande do Sul", 
                  "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"), 
         CodUF = c(c(11:17), 
                   c(21:29),
                   c(31:33, 35), 
                   c(41:43), 
                   c(50:53))) %>%
  mutate(Reg = fct_collapse(UF, 
                            Norte    = c("RO", "AC", "AM", "RR", "PA", "AP", "TO"), 
                            Nordeste = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
                            Sudeste  = c("MG", "ES", "RJ", "SP"),
                            Sul      = c("PR", "SC", "RS"), 
                            Centro   = c("MS", "MT", "GO", "DF"))) %>%
  select(Reg,UF, CodUF, Nome)


Norte    <- Estados %>% filter(Reg == "Norte")
Nordeste <- Estados %>% filter(Reg == "Nordeste")
Sudeste  <- Estados %>% filter(Reg == "Sudeste")
Sul      <- Estados %>% filter(Reg == "Sul")
Centro   <- Estados %>% filter(Reg == "Centro")
Regioes  <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro")


# Países ------------------------------------------------------------------

paises <- 
  codelist %>% 
  select(un.region.name, un.regionsub.name, region23, iso3c, cldr.name.pt, iso.name.en) %>%
  rename(cont    = un.region.name, 
         subcont = un.regionsub.name, 
         reg     = region23,
         id      = iso3c, 
         nome    = cldr.name.pt, 
         nome2   = iso.name.en) %>%
  select(cont,subcont,everything()) %>%
  select(-nome2) %>%
  mutate(cont    = if_else(nome    == "Kosovo", "Europe", cont),
         cont    = if_else(nome    == "Taiwan", "Asia", cont),
         reg     = if_else(subcont == "Australia and New Zealand", "Australia and New Zealand", reg), 
         reg     = if_else(nome    == "Taiwan", "Eastern Asia",    reg), 
         reg     = if_else(nome    == "Kosovo", "Southern Europe", reg), 
         subcont = if_else(nome    == "Taiwan", "Eastern Asia",    subcont), 
         subcont = if_else(nome    == "Kosovo", "Southern Europe", subcont), 
         id      = if_else(nome    == "Kosovo", "RKS", id)) %>%
  filter(!is.na(nome), !is.na(reg)) %>%
  mutate(gr = "g0") %>%
  mutate(gr = if_else(id %in% c("ARG", "BOL", "BRA", "CHL", "COL", "ECU", "PRY", "PER", "URY", "VEN"), "g1", gr), 
         gr = if_else(id %in% c("CAN", "MEX", "USA"), "g2", gr), 
         gr = if_else(id %in% c("DNK", "FIN", "ISL", "NOR", "SWE"), "g3", gr),
         gr = if_else(id %in% c("DEU", "AUT", "BEL", "FRA", "ESP", "ITA", "LIE", "LUX", "MCO", "PRT", "NLD", "CHE"), "g4", gr),
         gr = if_else(id %in% c("CHN", "KOR", "JPN", "TWN"), "g5", gr)) %>%
  arrange(cont,subcont,reg,nome) %>%
  select(cont, subcont,reg, id, gr, nome) %>%
  rename(Cont = cont, Subcont = subcont, Reg = reg, Nome = nome)

gr0 <- paises %>% filter(gr == "g0") %>% pull(id)
gr1 <- paises %>% filter(gr == "g1") %>% pull(id)
gr2 <- paises %>% filter(gr == "g2") %>% pull(id)
gr3 <- paises %>% filter(gr == "g3") %>% pull(id)
gr4 <- paises %>% filter(gr == "g4") %>% pull(id)
gr5 <- paises %>% filter(gr == "g5") %>% pull(id)

Europa   <- paises %>% filter(Cont == "Europe")   %>% pull(id)
Americas <- paises %>% filter(Cont == "Americas") %>% pull(id)
Africa   <- paises %>% filter(Cont == "Africa")   %>% pull(id)
Asia     <- paises %>% filter(Cont == "Asia")     %>% pull(id)


#==============================================================================


# FUNÇÕES
#==============================================================================
# Número-Índice -----------------------------------------------------------

Ind <- function(x, nivel, final = FALSE) {
  # x     >> taxa de variação da série 
  # nível >> Nivelamento: 1 ou 100?
  # final >> Nivalmento n início ou final da série?
  if (!is.vector(x) || length(nivel) > 1 || !is.numeric(nivel)  || !is.logical(final) || length(final) >1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- nivel;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)*(1+x[i]))
      }
    } else {
      indice <- nivel; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)/(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice
  }
}


# Multiplicador de preços ----------------------------------------------------

Mp <- function(x, final = FALSE) {
  # x     >> taxa de variação do índice de preços 
  # final >> Normalização no iníco ou no final?
  if (!is.vector(x) || !is.logical(final) || length(final) >1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- 1;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)/(1+x[i]))
      }
    } else {
      indice <- 1; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)*(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice
  }
}


# Produzir séries reais ---------------------------------------------------

Real <- function(x, y, final = FALSE) {
  # x     >> taxa de variação dos preços
  # y     >> série nominal
  # final >> trazer para preços do início ou final do período (padrão: início)
  
  if (!is.vector(x) || !is.vector(y) || !(near(length(x),length(y))) || !is.logical(final) || length(final) > 1) {
    stop("checar entradas!")
  } else {
    if (final == FALSE) {
      indice    <- 1;
      for (i in 2:length(x)) {
        indice <- c(indice, last(indice)/(1+x[i]))
      }
    } else {
      indice <- 1; 
      for (i in (length(x)):2) {
        indice <- c(indice, (last(indice)*(1+x[i])))
      }
      indice <- rev(indice)  #inverter a ordem do vetor
    }  
    indice*y
  }
}


#==============================================================================





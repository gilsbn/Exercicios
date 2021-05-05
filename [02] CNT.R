

# INPUT
#==========================================================================================================================
# CNT: Tabela de volumes (API)  -------------------------------------------------

sidra_1620 <- get_sidra(api = "/t/1620/n1/all/v/all/p/all/c11255/all/d/v583%202")
x <- 
  sidra_1620 %>%
  as_tibble() %>%
  select(Trimestre, `Setores e subsetores`, Valor) %>%
  rename(t = Trimestre, `Nome` = `Setores e subsetores`, y = Valor) %>%
  mutate(tt = str_extract(t, "^\\d"), 
         aa = trimws(str_extract(t, "\\s\\d+$")),
         t  = as.yearqtr(paste(aa,tt, sep = ":"), format = "%Y:%q") %>% yearquarter()) %>%
  select(t, `Nome`, y)

descr <- x %>% pull(`Nome`) %>% unique()

volume <- 
  x %>% 
  mutate(`Acrônimo` = factor(`Nome`, levels = descr) %>% fct_recode("Agro"     = "Agropecuária - total",
                                                                    "Ind"      = "Indústria - total",
                                                                    "Extr"     = "Indústrias extrativas",
                                                                    "Transf"   = "Indústrias de transformação", 
                                                                    "SIUP"     = "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos",
                                                                    "Serviços" = "Serviços - total", 
                                                                    "Transp"   = "Transporte, armazenagem e correio",  
                                                                    "Info"     = "Informação e comunicação",  
                                                                    "Fin"      = "Atividades financeiras, de seguros e serviços relacionados",
                                                                    "Imob"     = "Atividades imobiliárias",
                                                                    "ServOtr"  = "Outras atividades de serviços",
                                                                    "Adm"      = "Administração, saúde e educação públicas e seguridade social",
                                                                    "VA"       = "Valor adicionado a preços básicos", 
                                                                    "Imp"      = "Impostos líquidos sobre produtos",
                                                                    "PIB"      = "PIB a preços de mercado",     
                                                                    "Consumo"  = "Despesa de consumo das famílias",
                                                                    "Governo"  = "Despesa de consumo da administração pública",
                                                                    "FBCF"     = "Formação bruta de capital fixo",    
                                                                    "Export"   = "Exportação de bens e serviços",       
                                                                    "Import"   = "Importação de bens e serviços (-)"), 
         Grupo = `Acrônimo` %>% fct_recode("Agropecuária"         = "Agro", 
                                           "Indústria"            = "Ind", 
                                           "Ind. Extrativas"      = "Extr", 
                                           "Ind. Transformação"   = "Transf",
                                           "Transporte e outros"  = "Transp", 
                                           "Informação e outros"  = "Info", 
                                           "Financeiro e outros"  = "Fin",
                                           "Imobiliário e outros" = "Imob",
                                           "Outros serviços"      = "ServOtr", 
                                           "Adm pública e outros" = "Adm",
                                           "Valor adicionado"     = "VA",
                                           "Impostos líquidos"    = "Imp",
                                           "Consumo das famílias" = "Consumo", 
                                           "Consumo do governo"   = "Governo", 
                                           "Exportação"           = "Export", 
                                           "Importação"           = "Import")) %>%
  group_by(t) %>%
  mutate(k = 1:n()) %>%
  relocate(t,k, Grupo, y, `Acrônimo`,Nome) %>%
  ungroup()

grupos <- volume %>% select(k, Grupo, y, `Acrônimo`, Nome) %>% unique() 


# CNT: Tabela de volumes (Excel) ------------------------------------------

# cnt_vol <- read_excel("[D] CNT/tabela1620.xlsx", range = "A5:W105", col_names = TRUE)
# cnt_vol <- 
#   cnt_vol %>%
#   rename(t = `...1`, 
#          `Importação de bens e serviços` = `Importação de bens e serviços (-)`) %>% 
#   mutate(tt = str_extract(t, "^\\d"), 
#          aa = trimws(str_extract(t, "\\s\\d+$")), 
#          t  = as.yearqtr(paste(aa, tt, sep = ":"), format = "%Y:%q") %>% yearquarter()) %>% 
#   select(-tt, -aa) %>%
#   pivot_longer(c(`Agropecuária - total`:`Importação de bens e serviços`), names_to = "Descrição", values_to = "y")
# 
# cnt_descr <- cnt_vol %>% pull(`Descrição`) %>% unique
# cnt_vol   <- 
#   cnt_vol %>%
#   mutate(Gp1 = factor(`Descrição`, levels = cnt_descr), 
#          Gp1 = fct_recode(Gp1, 
#                           "Agro"     = "Agropecuária - total",
#                           "Ind"      = "Indústria - total",
#                           "Extr"     = "Indústrias extrativas",
#                           "Transf"   = "Indústrias de transformação", 
#                           "SIUP"     = "Eletricidade e gás, água, esgoto, atividades de gestão de resíduos",
#                           "Serviços" = "Serviços - total", 
#                           "Transp"   = "Transporte, armazenagem e correio",  
#                           "Info"     = "Informação e comunicação",  
#                           "Fin"      = "Atividades financeiras, de seguros e serviços relacionados",
#                           "Imob"     = "Atividades imobiliárias",
#                           "ServOtr"  = "Outras atividades de serviços",
#                           "Adm"      = "Administração, saúde e educação públicas e seguridade social",
#                           "VA"       = "Valor adicionado a preços básicos", 
#                           "Imp"      = "Impostos líquidos sobre produtos",
#                           "PIB"      = "PIB a preços de mercado",     
#                           "Consumo"  = "Despesa de consumo das famílias",
#                           "Governo"  = "Despesa de consumo da administração pública",
#                           "FBCF"     = "Formação bruta de capital fixo",    
#                           "Export"   = "Exportação de bens e serviços",       
#                           "Import"   = "Importação de bens e serviços"), 
#          Gp2 = Gp1, 
#          Gp2 = fct_recode(Gp2, 
#                           "Agropecuária"         = "Agro", 
#                           "Indústria"            = "Ind", 
#                           "Ind. Extrativas"      = "Extr", 
#                           "Ind. Transformação"   = "Transf",
#                           "Transporte e outros"  = "Transp", 
#                           "Informação e outros"  = "Info", 
#                           "Financeiro e outros"  = "Fin",
#                           "Imobiliário e outros" = "Imob",
#                           "Outros serviços"      = "ServOtr", 
#                           "Adm pública e outros" = "Adm",
#                           "Valor adicionado"     = "VA",
#                           "Impostos líquidos"    = "Imp",
#                           "Consumo das famílias" = "Consumo", 
#                           "Consumo do governo"   = "Governo", 
#                           "Exportação"           = "Export", 
#                           "Importação"           = "Import")) %>%
#   group_by(t) %>%
#   mutate(k = seq(1:n())) %>%
#   relocate(t,k,Gp1,Gp2,y) %>%
#   ungroup()
# 
# cnt_gp <- cnt_vol %>% ungroup() %>% select(k, Gp1, Gp2, `Descrição`) %>% unique(); rm(cnt_descr)


#==========================================================================================================================


# GRÁFICOS BÁSICOS
#==========================================================================================================================
# Decomposição do PIB: escolha do período-base ----------------------------

graf <- function(ANO = 2020, filtro = "Demanda", CTG = c(1,2,7,8,17), legenda = leg00, lab = c(1,0,0)) {
  
  if (!is.null(filtro)) {
    
    if (filtro == "demanda") {
      ctg    <- pull(volume, `Acrônimo`)[c(17,18,19,20,21,22)]
      Titulo <- "DECOMPOSIÇÃO DO PIB: ÓTICA DA DEMANDA"
      
    } else if (filtro == "setores") {
      ctg    <- pull(volume, `Acrônimo`)[c(17,1,2,7,8)]
      Titulo <- "DECOMPOSIÇÃO DO PIB: GRANDES SETORES"
      
    } else {
      stop("Escolha setores válidos")
    }
    
  } else {
    ctg    <- pull(volume, `Acrônimo`)[CTG]
    Titulo <- "DECOMPOSIÇÃO DO PIB (SELEÇÃO DE SETORES)"
  }
  
  x <<- x1 <-
    volume %>%
    as_tsibble(index = t, key = c(`Acrônimo`, Grupo)) %>%
    filter(`Acrônimo` %in% ctg) %>%
    model(X13 = X_13ARIMA_SEATS(y ~ seats())) %>%
    components() %>%
    as_tsibble() %>%
    transmute(t, `Acrônimo`, Grupo, y = season_adjust) %>%
    group_by(`Acrônimo`) %>%
    mutate(y = ((y/lag(y))-1)) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y,100))
  
  x1 <- x1 %>% filter(`Acrônimo` == "PIB")
  x2 <- x1 %>% filter(row_number() == (length(t) %/% lab[1]))
  x  <- x  %>% filter(`Acrônimo` != "PIB")
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(c(x$y, x1$y), na.rm = TRUE)
  ymax <- max(c(x$y, x1$y), na.rm = TRUE)
  
  g <<- 
    x %>%
    ggplot(aes(t,y, color = Grupo)) + 
    geom_line(x1,  mapping = aes(t,y), color = pal_c[1]) +
    geom_point(x1, mapping = aes(t,y), color = pal_c[1]) +
    geom_label_repel(x2, mapping = aes(t, y, label = Grupo), 
                     size     = 3,
                     fontface = "bold", 
                     nudge_x  = lab[2], 
                     nudge_y  = lab[3], 
                     color    = pal_c[1],
                     segment.color = pal_c[1],
                     segment.size  = 0.25) + 
    geom_line() + 
    geom_point() + 
    scale_x_yearquarter(NULL, date_labels = "%Y:%q", expand = c(0,0)) + 
    scale_y_continuous(NULL) + 
    scale_color_hue(NULL) + 
    labs(title    = Titulo, 
         subtitle = paste0("Número-índice: ", format(first(unique(x$t)), "%Y:%q"), " = 100"), 
         caption  = "Fonte: IBGE") + 
    g1 + legenda
  
  g
  
}; 
graf(ANO = 2019, filtro = "demanda", CTG = NULL,  legenda = leg01, lab = c(1,-70,+8))
graf(ANO = 2019, filtro = "setores", CTG = NULL,  legenda = leg00, lab = c(1,-70,-8))
graf(ANO = 2019, filtro = NULL, CTG = c(2:5,17),  legenda = leg00, lab = c(1,-70,-8))
graf(ANO = 2019, filtro = NULL, CTG = c(9:14,17), legenda = leg00, lab = c(1,-70,-4))

#==========================================================================================================================


# MODELOS ARIMA
#==========================================================================================================================
# Modelos simples de previsão: antes da pandemia --------------------------

decomp <- 
  volume %>% 
  as_tsibble(index = t, key = c(Grupo, `Acrônimo`)) %>%
  filter(Grupo == "PIB") %>%
  model(X13 = X_13ARIMA_SEATS(y ~ seats())) %>%
  components() %>%
  as_tibble() %>%
  ungroup() 

completa <- decomp   %>% transmute(y = season_adjust) %>% ts(start = c(1996,1), frequency = 4)
amostra  <- completa %>% window(end = c(2019,4))
training <- completa %>% window(end = c(2017,4))
testsamp <- completa %>% window(start = c(2018,1), end = c(2019,4))

modelos <- function(MOD = 0) {
  
  if (MOD == 0) {
    mod  <<- auto.arima(log(training), stepwise = FALSE)
    fct  <<- forecast(mod,  h = 8)
    tipo <<- "auto.arima()"} else {}
  
  if (MOD == 1) {
    mod  <<- Arima(log(training), order = c(1,1,0), include.constant = TRUE)
    fct  <<- forecast(mod, h = 8)
    tipo <<- "ARIMA(1,1,0)"} else {}
  
  if (MOD == 2) {
    mod <<- Arima(log(training), order = c(2,1,0), include.constant = TRUE)
    fct <<- forecast(mod, h = 8)
    tipo <<- "ARIMA(2,1,0)"} else {}
  
  if (MOD == 3) {
    mod  <<- Arima(log(training), order = c(1,1,1), include.constant = TRUE)
    fct  <<- forecast(mod, h = 8)
    tipo <<- "ARIMA(1,1,1)"} else {}
  
  if (MOD == 4) {
    mod  <<- Arima(log(training), order = c(2,1,1), include.constant = TRUE)
    fct  <<- forecast(mod, h = 8)
    tipo <<- "ARIMA(2,1,1)"} else {}
  
  if (MOD == 5) {
    mod  <<- Arima(log(training), order = c(2,1,2), include.constant = TRUE)
    fct  <<- forecast(mod, h = 8)
    tipo <<- "ARIMA(2,1,2)"} else {}
  
  fct.mean <<- exp(fct[["mean"]])
  fct.ic80 <<- exp(cbind(l = fct[["lower"]][,"80%"], u = fct[["upper"]][,"80%"]))
  fct.ic95 <<- exp(cbind(l = fct[["lower"]][,"95%"], u = fct[["upper"]][,"95%"]))
  
  mod %>% summary()
  
  x <- 
    cbind(pib = amostra, fct = fct.mean) %>%
    as_tsibble() %>%
    rename(t = index, k = key, y = value) %>%
    mutate(k = if_else(k == "fct.mean", "fct", k), 
           k = factor(k, levels = c("pib", "fct")))
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  
  g <<- 
    x %>%
    ggplot() + 
    geom_line(aes(t, y, color = k))  + 
    geom_ribbon(data    = fct.ic95 %>% as_tsibble(pivot_longer = FALSE) %>% rename(t = index), 
                mapping = aes(t, ymin = l, ymax = u, fill = "95pp"), size = 0.1, alpha = 0.3) + 
    geom_ribbon(data    = fct.ic80 %>% as_tsibble(pivot_longer = FALSE) %>% rename(t = index), 
                mapping = aes(t, ymin = l, ymax = u, fill = "80pp"), size = 0.1, alpha = 0.3) + 
    scale_x_yearquarter(NULL, breaks = seq(xmin, xmax, length.out = 10), expand = c(0,0), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL, expand = c(0,0)) + 
    scale_color_manual(NULL, values = c(pal_c[1], pal_v[3]), labels = c("PIB", "PIB: previsão (fora da amostra)")) + 
    scale_fill_manual(NULL, values = c("80pp" = pal_a[3], "95pp" = pal_a[4]),  labels = c("80pp" = "80%", "95pp" = "95%")) + 
    labs(title    = paste0("PREVISÃO DO PIB 2017-2019: ", tipo), 
         subtitle = "Índice de variação do volume com ajuste sazonal (média 1995 = 100)", 
         caption  = "Fonte: Estimativas do autor; dados do IBGE") + 
    g1 + 
    theme(plot.margin = margin(r = 16, unit = "pt"))
  
  g
  
}; 
modelos(MOD = 0)
modelos(MOD = 1)
modelos(MOD = 2)
modelos(MOD = 3)
modelos(MOD = 4)
modelos(MOD = 5)


# Modelos simples de previsão: pandemia -----------------------------------

decomp <- 
  volume %>% 
  as_tsibble(index = t, key = c(`Acrônimo`, Grupo)) %>%
  filter(Grupo == "PIB") %>%
  model(X13 = X_13ARIMA_SEATS(y ~ seats())) %>%
  components() %>%
  as_tibble() %>%
  ungroup() 

completa <- decomp %>% transmute(y = season_adjust) %>% ts(start = c(1996,1), frequency = 4)
amostra  <- completa %>% window(end = c(2019,4))
training <- completa %>% window(end = c(2017,4))
testsamp <- completa %>% window(start = c(2018,1), end = c(2019,4))

modelos <- function(MOD = 0) {
  
  if (MOD == 0) {
    mod  <<- auto.arima(log(training), stepwise = FALSE)
    fct  <<- forecast(mod,  h = 12)
    tipo <<- "auto.arima()"} else {}
  
  if (MOD == 1) {
    mod  <<- Arima(log(training), order = c(1,1,0), include.constant = TRUE)
    fct  <<- forecast(mod, h = 12)
    tipo <<- "ARIMA(1,1,0)"} else {}
  
  if (MOD == 2) {
    mod <<- Arima(log(training), order = c(2,1,0), include.constant = TRUE)
    fct <<- forecast(mod, h = 12)
    tipo <<- "ARIMA(2,1,0)"} else {}
  
  if (MOD == 3) {
    mod  <<- Arima(log(training), order = c(1,1,1), include.constant = TRUE)
    fct  <<- forecast(mod, h = 12)
    tipo <<- "ARIMA(1,1,1)"} else {}
  
  if (MOD == 4) {
    mod  <<- Arima(log(training), order = c(2,1,1), include.constant = TRUE)
    fct  <<- forecast(mod, h = 12)
    tipo <<- "ARIMA(2,1,1)"} else {}
  
  if (MOD == 5) {
    mod  <<- Arima(log(training), order = c(2,1,2), include.constant = TRUE)
    fct  <<- forecast(mod, h = 12)
    tipo <<- "ARIMA(2,1,2)"} else {}
  
  fct.mean <<- exp(fct[["mean"]])
  fct.ic80 <<- exp(cbind(l = fct[["lower"]][,"80%"], u = fct[["upper"]][,"80%"]))
  fct.ic95 <<- exp(cbind(l = fct[["lower"]][,"95%"], u = fct[["upper"]][,"95%"]))
  
  mod %>% summary()
  
  x <- 
    cbind(pib = completa, fct = fct.mean) %>%
    as_tsibble() %>%
    rename(t = index, k = key, y = value) %>%
    mutate(k = if_else(k == "fct.mean", "fct", k), 
           k = factor(k, levels = c("pib", "fct")))
  
  x %>% pull(t)
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  # x0   <- x %>% filter(year(t) == 2017, quarter(t) == 4) %>% pull(t) %>% unique()
  
  g <<- 
    x %>%
    ggplot() + 
    geom_line(aes(t, y, color = k))  + 
    geom_vline(xintercept = ymd(20171201), linetype = 3, color = pal_c[3]) + 
    geom_ribbon(data    = fct.ic95 %>% as_tsibble(pivot_longer = FALSE) %>% rename(t = index), 
                mapping = aes(t, ymin = l, ymax = u, fill = "95pp"), size = 0.1, alpha = 0.2) + 
    geom_ribbon(data    = fct.ic80 %>% as_tsibble(pivot_longer = FALSE) %>% rename(t = index), 
                mapping = aes(t, ymin = l, ymax = u, fill = "80pp"), size = 0.1, alpha = 0.2) + 
    scale_x_yearquarter(NULL, breaks = seq(xmin, xmax, length.out = 10), expand = c(0,0), date_labels = "%Y:%q") + 
    scale_y_continuous(NULL, expand = c(0,0)) + 
    scale_color_manual(NULL, values = c(pal_c[1], pal_v[2]), labels = c("PIB", "PIB: previsão (fora da amostra)")) + 
    scale_fill_manual(NULL, values = c("80pp" = pal_a[4], "95pp" = pal_a[3]),  labels = c("80pp" = "80%", "95pp" = "95%")) + 
    labs(title    = paste0("PREVISÃO DO PIB 2017-2020: ", tipo), 
         subtitle = "Índice de variação do volume com ajuste sazonal (média 1995 = 100)", 
         caption  = "Fonte: Estimativas do autor; dados do IBGE") + 
    g1 + 
    theme(plot.margin = margin(r = 16, unit = "pt"))
  
  g
  
}; 
modelos(MOD = 0)
modelos(MOD = 1)
modelos(MOD = 2)
modelos(MOD = 3)
modelos(MOD = 4)
modelos(MOD = 5)


# Modelos simples de previsão: utilizando fable -------------------------------

decomp <- 
  volume %>% 
  as_tsibble(index = t, key = c(Grupo, `Acrônimo`)) %>%
  filter(Grupo == "PIB") %>%
  model(X13 = X_13ARIMA_SEATS(y ~ seats())) %>%
  components() %>%
  as_tsibble() %>%
  ungroup()

completa <- 
  decomp %>%
  transmute(t, 
            y = season_adjust, 
            d1 = if_else(year(t) == 2020 & quarter(t) == 1, 1, 0), 
            d2 = if_else(year(t) == 2020 & quarter(t) == 2, 1, 0))

training <- decomp %>% filter(year(t) <= 2019)

modelos <- 
  training %>% 
  transmute(t, y = season_adjust) %>%
  mutate(d1 = if_else(year(t) == 2020 & quarter(t) == 1, 1, 0), 
         d2 = if_else(year(t) == 2020 & quarter(t) == 2, 1, 0)) %>%
  model(`ARIMA(1,1,0)` = ARIMA(log(y) ~ 1 + pdq(1,1,0) + PDQ(0,0,0)), 
        `ARIMA(2,1,0)` = ARIMA(log(y) ~ 1 + pdq(2,1,0) + PDQ(0,0,0)), 
        `ARIMA(0,1,1)` = ARIMA(log(y) ~ 1 + pdq(0,1,1) + PDQ(0,0,0)), 
        `ARIMA(0,1,2)` = ARIMA(log(y) ~ 1 + pdq(0,1,2) + PDQ(0,0,0)), 
        `ARIMA(1,1,1)` = ARIMA(log(y) ~ 1 + pdq(1,1,1) + PDQ(0,0,0)), 
        `ARIMA(2,1,1)` = ARIMA(log(y) ~ 1 + pdq(2,1,1) + PDQ(0,0,0)), 
        `ARIMA(2,1,2)` = ARIMA(log(y) ~ 1 + pdq(2,1,2) + PDQ(0,0,0)))

modelos %>% glance()

modelos_fct <- modelos %>% forecast(h = 8)

levels <- modelos_fct %>% tibble() %>% pull(.model) %>% unique()
xmin   <- min(c(completa$t, unique(pull(as_tibble(modelos_fct),t))))
xmax   <- max(c(completa$t, unique(pull(as_tibble(modelos_fct),t))))
ymin   <- min(c(completa$y, unique(pull(as_tibble(modelos_fct),.mean))))
ymax   <- max(c(completa$y, unique(pull(as_tibble(modelos_fct),.mean))))

completa %>%
  autoplot(y, size = 0.7) + 
  autolayer(modelos_fct %>% mutate(.model = factor(.model, levels = levels)), level = seq(30,90,10), alpha = 0.15) + 
  geom_vline(xintercept = as_date("2019-12-31"), linetype = 3) +
  scale_x_yearquarter(NULL, breaks = seq(xmin, xmax, length.out = 10), expand = c(0,0), date_labels = "%Y:%q") + 
  scale_y_continuous(NULL) + 
  labs(title    = "MODELOS ARIMA: PREVISÃO DO PIB PARA 2019-2020", 
       subtitle = "Índice de volume com ajuste sazonal (média 1995 = 100)", 
       caption  = "Fonte: Estimativas do autor, dados do IBGE") + 
  guides(fill   = guide_legend(title = "Modelos"), 
         color  = guide_legend(title = "Modelos", override.aes = list(size = 1.2)), 
         level  = guide_legend(title = "Intervalos", override.aes = list(fill = pal_c[1:7]))) +
  g1 + leg01 + 
  theme(plot.margin = margin(r = 16, unit = "pt"))
  coord_cartesian(xlim = c(yearquarter(as.yearqtr(ymd(20171231))), xmax)) + 

  
  
  guides(color = guide_legend(override.aes = list(size = 1),
                              title = list("Modelos")), 
         fill  = guide_legend(title = list("Modelos", "IC"), nrow = 2)) + 

# ?aes    
#     
# ?guide_legend  
# yearquarter(as.yearqtr(ymd(20181231)))
# 
# guides(color = guide_legend(override.aes = list(size = 1), 
#                             title = list("Modelos"), alpha = 1), 
#        fill  = guide_legend(title = list("Modelos"))) + 


#==========================================================================================================================




#
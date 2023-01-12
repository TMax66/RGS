#mappa home----
com <- readRDS(here("Bergamo", "data", "processed", "ITA_adm3.sf.rds"))
prov <- readRDS(here("Bergamo", "data", "processed", "ITA_adm2.sf.rds"))
reg <- readRDS(here("Bergamo", "data", "processed", "ITA_adm1.sf.rds"))
sf::st_crs(com) = 4326
sf::st_crs(prov) = 4326
sf::st_crs(reg) = 4326

BG <- prov[prov$NAME_2 == "Bergamo", ]
combg <- com[com$NAME_2 == "Bergamo",]
LOM <- reg[reg$NAME_1 == "Lombardia", ]
PRlom <- prov[prov$NAME_1 == "Lombardia", ]

# distrettiATS <- read_excel("Bergamo/data/distrettibg.xlsx")




x <- conf %>% filter(str_detect(ASL, "A.T.S. BERGAMO")) %>% 
  select(ASL, comune) %>% 
  unique() 


A <- x %>% filter(ASL == "A.T.S. BERGAMO - DISTR. A") %>% 
select(comune)  


B <- x %>% filter(ASL == "A.T.S. BERGAMO - DISTR. B") %>% 
  select(comune)  


z <- as.vector(A$comune)
zz <- as.vector(B$comune)








distretti <- combg %>% 
  st_as_sf() %>% 
  mutate(DISTRETTO = case_when(
    NAME_3 %in% z ~ paste0("A.T.S. BERGAMO - DISTR. A"), 
    NAME_3 %in% zz ~ paste0("A.T.S. BERGAMO - DISTR. B")
    )) %>% 
    group_by(DISTRETTO) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>%
  sf::st_transform('+proj=longlat +datum=WGS84') %>% 
  mutate(lng = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 




combg %>% 
  st_as_sf() %>% 
  filter(!NAME_3 %in% z ) %>%  View()
  
  mutate(DISTRETTO = case_when(
    NAME_3 %in% z ~ paste0("A.T.S. BERGAMO - DISTR. A"), 
    NAME_3 %in% zz ~ paste0("A.T.S. BERGAMO - DISTR. B")
  )) %>% 















pal <-
   colorFactor(palette = c( "#9999cd",  "#0284d8"),
               levels = c("A.T.S. BERGAMO - DISTR. A","A.T.S. BERGAMO - DISTR. B"))

labels <- paste("<strong>",
                distretti$DISTRETTO,
                "</strong><br>",
                "n° di allevamenti:",
                distretti$allev,
                "<br>",
                "n° esami eseguiti",
                distretti$esam) %>%
  lapply(htmltools::HTML)

labels2 <- paste("distretto di",
                 "<br>",
                 "<strong>",
                 distretti$DISTRETTO2,
                 "</strong>") %>%
  lapply(htmltools::HTML)

factpal <- colorFactor(c( "#9999cd",  "#0284d8"), 
                       c("A.T.S. BERGAMO - DISTR. A","A.T.S. BERGAMO - DISTR. B"))

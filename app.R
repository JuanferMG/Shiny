library(sf)
library(shiny)
library(leaflet)
library(viridis)
library(stringr)
library(RColorBrewer)
library(sqldf)
library(dplyr)

options(scipen=999)

# load("data/indicadores.rda")
# load("data/secciones2021.rda")
load("data/municipios.rda")
load("data/datos_asociaciones.rda")
load("data/datos_turistas.rda")
load("data/datos_paro.rda")

# municipios <- municipios[municipios$CPRO==46,]
# municipios$CMUN <- as.numeric(substr(municipios$NATCODE,7,12))
asoc_agg <- sqldf::sqldf("SELECT CMUN, COUNT(*) AS ASOCIACIONES FROM datos_asociaciones GROUP BY CMUN")
turismo_agg <- sqldf::sqldf("SELECT CMUN, SUM(turistas)/COUNT(*) TURISMO_MEDIO FROM datos_turistas WHERE pais_orig_cod = '000' GROUP BY CMUN")

municipios <- dplyr::left_join(municipios,asoc_agg, by = "CMUN")
municipios$ASOCIACIONES[is.na(municipios$ASOCIACIONES)] <- 0

municipios <- dplyr::left_join(municipios,turismo_agg, by = "CMUN")
municipios$TURISMO_MEDIO[is.na(municipios$TURISMO_MEDIO)] <- 0

municipios <- dplyr::left_join(municipios,datos_paro, by = "CMUN")
municipios$PCT_PARO[is.na(municipios$PCT_PARO)] <- 0


# n <- nrow(municipios)
# colores <- viridis_pal(option = "D")(n)
# municipios$COLOR <- colores

n <- nrow(municipios)
gama<-RColorBrewer::brewer.pal(n = 9, name = "Spectral")
col_p <- colorRampPalette(gama)
paleta<-col_p(n)
paleta_leyenda <- col_p(11)

# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# col_vector = col_vector[!col_vector%in%c("#CCCCCC","#B3B3B3","#666666","#999999")]
# set.seed(10)
# municipios$COLOR <-sample(col_vector, n,replace = T)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 22px;
  }
"))


title <- tags$div(
  tag.map.title, HTML('<strong> Provincia de Valencia "Capital de la Fraternidad de la Uni\u00F3n Europea"</strong>')
) 

rr <- tags$div(
  HTML('<strong> Provincia de Valencia "Capital de la Fraternidad de la Uni\u00F3n Europea"</strong>')
) 


ui <- bootstrapPage(
  tags$style("html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput("code", "Indicador",choices = c("N\u00BA Asociaciones","Turismo extranjero medio mensual","Paro registrado")
                ),
                actionButton("center", "Centrar Mapa")
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet(municipios,options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) 
  })
  
  observeEvent(input$code,{
    
    if(input$code=="N\u00BA Asociaciones"){
      
      municipios=municipios[order(municipios$ASOCIACIONES,decreasing = T),]
      municipios$COLOR <- rev(paleta)
      
      
      state_popup <- paste0("<strong>  Municipio: ",municipios$NAMEUNIT,"<br>",'</strong>', "N\u00BA Asociaciones: ",formatC(x = municipios$ASOCIACIONES,decimal.mark = ",", big.mark = ".",format = "fg"))
      state_popup2 <- paste0("<strong>",municipios$NAMEUNIT,'</strong>')
      labs <- as.list(state_popup) 
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # addControl(title, className="map-title", position = "topleft")%>%
        addControl("\u00A9 2022: Plataforma por Valencia Capital Europea de la Fraternidad", position = "bottomleft") %>% 
        addControl("Fuente: Generalitat Valenciana (GVA).", position = "bottomleft") %>% 
        # addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),popup = state_popup,popupOptions = popupOptions(closeOnClick = TRUE),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addLegend("bottomleft", colors=paleta_leyenda, 
                  labels = c(format(x = round(quantile(x = municipios$ASOCIACIONES, prob=seq(0,1,0.1),na.rm=T),0),big.mark = ".", decimal.mark = ",")),
                  title = "Asociaciones",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 0.75
        )
    }
    
    if(input$code=="Turismo extranjero medio mensual"){
      
      municipios=municipios[order(municipios$TURISMO_MEDIO,decreasing = T),]
      municipios$COLOR <- rev(paleta)
      
    state_popup <- paste0("<strong>  Municipio: ",municipios$NAMEUNIT,"<br>",'</strong>', "N\u00BA Turistas: ",formatC(x = round(municipios$TURISMO_MEDIO),decimal.mark = ",", big.mark = ".",format = "fg"))
      state_popup2 <- paste0("<strong>",municipios$NAMEUNIT,'</strong>')
      labs <- as.list(state_popup) 
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # addControl(title, className="map-title", position = "topleft")%>%
        addControl("\u00A9 2022: Plataforma por Valencia Capital Europea de la Fraternidad", position = "bottomleft") %>% 
        addControl("Fuente: Instituto Nacional de Estad\u00EDstica (INE).", position = "bottomleft") %>% 
        # addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),popup = state_popup,popupOptions = popupOptions(closeOnClick = TRUE),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addLegend("bottomleft", colors=paleta_leyenda, 
                  labels = c(format(x = round(quantile(x = municipios$TURISMO_MEDIO, prob=seq(0,1,0.1),na.rm=T),0),big.mark = ".", decimal.mark = ",")),
                  title = "Turistas",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 0.75
        )
    }
    
    if(input$code=="Paro registrado"){
      
      municipios=municipios[order(municipios$PCT_PARO,decreasing = T),]
      municipios$COLOR <- paleta
      
      state_popup <- paste0("<strong>  Municipio: ",municipios$NAMEUNIT,"<br>",'</strong>', "Poblaci\u00F3n 16-64 a\u00F1os en paro: ",paste0(formatC(x = round(municipios$PCT_PARO*100,2),decimal.mark = ",", big.mark = ".",format = "fg")," %"))
      state_popup2 <- paste0("<strong>",municipios$NAMEUNIT,'</strong>')
      labs <- as.list(state_popup) 
      leafletProxy("map") %>%
        clearShapes() %>%
        clearControls() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        # addControl(title, className="map-title", position = "topleft")%>%
        addControl("\u00A9 2022: Plataforma por Valencia Capital Europea de la Fraternidad", position = "bottomleft") %>% 
        addControl("Fuente: Servicio P\u00FAblico de Empleo Estatal (SEPE).", position = "bottomleft") %>% 
        # addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),popup = state_popup,popupOptions = popupOptions(closeOnClick = TRUE),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addPolygons(data = municipios,label = lapply(labs, HTML),labelOptions =  labelOptions(style=list('font-size' = '14px', 'border-color' = 'rgba(0,0,0,0.5)')),stroke=T,color = "black", fillColor = ~COLOR, weight = 1, opacity=1, fillOpacity = 0.8)%>%
        addLegend("bottomleft", colors=rev(paleta_leyenda), 
                  labels = c(format(x = round(quantile(x = municipios$PCT_PARO*100, prob=seq(0,1,0.1),na.rm=T),2),big.mark = ".", decimal.mark = ",")),
                  title = "% Paro",
                  labFormat = labelFormat(prefix = "$"),
                  opacity = 0.75
        )
    }
    
    
  })
  
  observeEvent(input$center, {
    leafletProxy("map")  %>%
      fitBounds(min(municipios$long), min(municipios$lat), max(municipios$long), max(municipios$lat))
  })
  
}

shinyApp(ui, server)
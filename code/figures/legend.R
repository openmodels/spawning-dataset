setwd("~/Dropbox/Spawning ProCreator")

library(PBSmapping)
library(ggplot2)

shp = importShapefile("dataset/GO-FISH-hs.shp")
polydata <- attr(shp, 'PolyData')

shp.coast <- importShapefile("~/data/shapefiles/ne_10m_coastline/ne_10m_coastline.shp")
shp.adm0 <- importShapefile("~/data/shapefiles/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")

## plot(0, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab='', xlim=c(0, 1), ylim=c(0, 1))
## for (ii in 1:100) {
##     y0 = log10(ii) / 2
##     rect(.25, .25 + y0 / 2, .5, .75, col="#0000001a", border=NA)
##     lines(c(.25, .25), c(.25 + y0 / 2, .75), col="#00000033")
##     lines(c(.5, .5), c(.25 + y0 / 2, .75), col="#00000033")
## }

for (month in NA) { ##c('djf', 'mam', 'jja', 'son')) {
    grid <- expand.grid(X=seq(-180 + .05, 180 - .05, by=.1),
                    Y=seq(-78 + .05, 81 - .05, by=.1))
    grid$count <- 0
    grid$EID <- 1:nrow(grid)
    grid <- as.EventData(grid, projection=1)

    for (pid in unique(shp$PID)) {
        if (!is.na(month)) {
            if (month %in% c('djf', 'mam', 'jja', 'son')) {
                months <- list('djf'=c(12, 1, 2), 'mam'=3:5, 'jja'=6:8, 'son'=9:11)[[month]]
                if (all(is.na(polydata[pid, months + 1]) | polydata[pid, months + 1] == 0))
                    next
            } else if (is.na(polydata[pid, month + 1]) || polydata[pid, month + 1] == 0)
                next
        }
        print(c(month, pid))
        found <- findPolys(grid, subset(shp, PID == pid), maxRows=1.1 * nrow(grid))
        grid$count[found$EID] <- grid$count[found$EID] + 1
    }
    if (!is.na(month))
        grid$count <- pmin(grid$count, 100)

    ggplot(subset(grid, count > 0), aes(X, Y)) +
        coord_fixed(ylim=c(-78, 81)) +
        geom_raster(aes(fill=count)) +
        geom_polygon(data=shp.adm0, aes(group=paste(PID, SID)), colour='#c3b840', fill='#936d24', size=.1) +
        geom_path(data=shp.coast, aes(group=paste(PID, SID)), colour='#c3b840', size=.1) +
        scale_fill_gradient("Spawning regions:", low='#9ecae1', high='#08306b', trans='log10', breaks=c(1, 3, 10, 30, 100), limits=c(1, max(100, max(grid$count)))) + scale_x_continuous(NULL, expand=c(0, 0)) +
        scale_y_continuous(NULL, expand=c(0, 0)) + theme_bw() +
        theme(legend.position="bottom", legend.key.width=unit(1, 'cm'))
    if (is.na(month))
        ggsave("figures/global.png", width=13, height=6.5)
    else
        ggsave(paste0("figures/month", month, ".png"), width=13, height=6.5)
}

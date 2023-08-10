taxonomy <- read.csv("inputs/taxonomy.csv")

lowest.hierarchy <- function(scientific, callback) {
    if (length(grep(" ", scientific)) == 0)
        scientific <- paste(scientific, "spp.")

    taxrow <- taxonomy[taxonomy$scientific == scientific,]
    if (nrow(taxrow) >= 1)
        taxrow <- taxrow[1, -c(1:2)] # Drop title
    else {
        genus <- strsplit(scientific, " ")[[1]][1]
        taxrow <- taxonomy[!is.na(taxonomy$genus) & taxonomy$genus == genus,]
        if (nrow(taxrow) >= 1)
            taxrow <- taxrow[1, -c(1:2)] # Drop title
        else
            taxrow <- data.frame(kingdom="Animalia", genus)
    }
    columns <- c('scientific', rev(names(taxrow)))
    entries <- c(scientific, rev(as.character(t(taxrow))))

    for (cc in 1:length(columns)) {
        if (columns[cc] == 'scientific')
            specwithin <- scientific
        else
            specwithin <- taxonomy$scientific[taxonomy[, columns[cc]] == entries[cc]]

        values <- callback(specwithin)
        if (is.data.frame(values)) {
            if (nrow(values) > 0) {
                values$level <- columns[cc]
                return(values)
            }
        } else if (length(values) > 0)
            return(list(values=values, level=columns[cc]))
    }

    stopifnot(F) # Should never get here
}

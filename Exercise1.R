library(animint2)
data(WorldBank)
WorldBank$Region <- sub(" (all income levels)", "", WorldBank$region, fixed=TRUE)

library(data.table)
not.na <- data.table(
  WorldBank)[!(is.na(life.expectancy) | is.na(fertility.rate))]
not.na[is.na(not.na$population)]
FACETS <- function(df, top, side){
  data.frame(df,
             top=factor(top, c("Fertility rate", "Years")),
             side=factor(side, c("Years", "Life expectancy")))
}

TS.RIGHT <- function(df)FACETS(df, "Years", "Life expectancy")
SCATTER <- function(df)FACETS(df, "Fertility rate", "Life expectancy")
TS.ABOVE <- function(df)FACETS(df, "Fertility rate", "Years")

years <- unique(not.na[, .(year)])

#Creating a scatter plot Life expectancy vs Fertility rate
ts.scatter <- ggplot()+
  theme_animint(width=600)+
  geom_point(aes(
    fertility.rate, life.expectancy,
    colour=Region, size=population,
    key=country), 
    clickSelects="country",
    showSelected="year",
    data=SCATTER(not.na))+
  scale_size_animint(pixel.range=c(2, 20), breaks=10^(9:5))+geom_text(aes(
    x=fertility.rate, y=life.expectancy, label=country,
    key=country),
    showSelected="country",
    data=not.na) #Using geom text with the value of label as show selected
animint(ts.scatter)







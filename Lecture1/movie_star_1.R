
#### Read into the R environment
movie_star = read.table("movie_star.txt", col.names = c("movie", "star"))
head(movie_star)


#### Barplot for number of movies of each star
barplot(table(movie_star$star), ylab = "Number of Movies", cex.lab = 1.3)
barplot(sort(table(movie_star$star), decreasing = T), col = heat.colors(15), ylab = "Number of Movies", cex.lab = 1.3)

require(showtext)  ### change the fonts
showtext.auto(enable = TRUE)
X11()
barplot(sort(table(movie_star$star), decreasing = T), 
        col = heat.colors(15), family = "wqy-microhei", cex.names = 1.4, ylab = "Number of Movies", cex.lab = 1.3)

#### Only show the top 3 and others
mm = sort(table(movie_star$star), decreasing = T)
mm[4] = sum(mm[-(1:3)])
names(mm)[4] = "ÆäËû"
barplot(mm[1:4], col = c("#F15854",  "#5DA5DA", "#FAA43A", "#60BD68"), 
        family = "wqy-microhei", cex.names = 1.5, ylab = "Number of Movies", cex.lab = 1.3)

### Pie plot
pie(sort(table(movie_star$star), decreasing = T))
pie(mm[1:4])
pie(mm[1:4], col = c("#F15854",  "#5DA5DA", "#FAA43A", "#60BD68"), 
    border = NA, family = "wqy-microhei", cex = 1.5)


#### download cran logs

library(devtools)
install_github("metacran/cranlogs")
library(cranlogs)
cran_top_downloads(when = "last-week")

cran_ggplot2 = cran_downloads(package = "ggplot2", from = "2012-10-01", to = "2016-05-31")
head(cran_ggplot2)
sum(cran_ggplot2$count)

#### ggplot2 histograms

library(ggplot2)

ggplot(cran_ggplot2, aes(count)) + 
  geom_histogram(aes(y=..density..), bins = 30, color = "white", fill = "grey")

library(showtext)
X11()
ggplot(cran_ggplot2, aes(count)) + 
  geom_histogram(aes(y=..density..), bins = 30, color = "white", fill = "grey") + 
  geom_density(aes(y = ..density..), color = 'red',fill='red', alpha=0.2) + 
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))

showtext.auto(enable = TRUE)

cran2 = cran_downloads(package = c("ggplot2", "plyr"), from = "2012-10-01", to = "2016-05-31")
cran2$package = factor(cran2$package)
ggplot(cran2, aes(count, fill = package, colour = package))  + 
  geom_density(aes(y = ..density..), alpha=0.2, size = 1) + 
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))

#### barplot of super stars

super_star = read.table("movie_star.txt", col.names = c("super", "star"))
head(super_star)
Num = sort(table(super_star$star), decreasing = T)
df = data.frame(star = names(Num), num_super = Num)
df$star = factor(df$star, levels = df$star)


showtext.auto(enable = TRUE)
X11()
ggplot(df, aes(x = star, y = num_super, fill = num_super)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  scale_fill_gradient(limits=c(0, 4), low="white",high="red")+
  xlab("Super Star") +
  ylab("Number of TV Series") +
  theme(axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_text(vjust=-2),
        legend.position="none")


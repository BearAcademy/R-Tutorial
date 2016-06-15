library(showtext)
library(ggplot2)


### read data
yitian = readLines("倚天屠龙记.Txt")
yitian[1:10]


#### cut paragraph
para_head = grep("\\s+", yitian)
cut_para1 = cbind(para_head[1:(length(para_head)-1)], para_head[-1]-1)
yitian_para = sapply(1:nrow(cut_para1), function(i) paste(yitian[cut_para1[i,1]:cut_para1[i,2]], collapse = ""))
yitian_para[1:4]


#### read in main roles
roles = readLines("主角名单.txt")
roles[1:5]

#### count roles
roles1 = paste0("(", gsub(" ", ")|(", roles), ")")
main_roles = c("殷离","周芷若","赵敏","小昭", "张无忌")
role_para = sapply(roles1[1:5], grepl, yitian_para)
colnames(role_para) = main_roles

role_count = data.frame(role = factor(colnames(role_para), 
                                      levels = c("张无忌", "赵敏","周芷若", "殷离","小昭")), 
                        count = colSums(role_para))

showtext.auto(enable = TRUE)
X11()
ggplot(role_count, aes(x = role, y = count, fill = role)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  xlab("Role") +
  ylab("Count") +
  theme(axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_text(vjust=-2),
        legend.position="none")

#### show-up density
role_para_df = data.frame(role = factor(rep(main_roles, colSums(role_para))), 
                          para = which(role_para, arr.ind = T)[,1])

role_para_df1 = role_para_df[is.element(role_para_df$role, c("赵敏","周芷若", "张无忌")),]

ggplot(role_para_df1, aes(para, fill = role, colour = role))  + 
  geom_density(aes(y = ..density..), alpha=0.2, size = 1) +
  xlab("出场顺序") +
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))

#### count common show-up
colSums(role_para[,5]*role_para[,1:4])
role_count1 = data.frame(role = factor(colnames(role_para)[1:4]), 
                count = colSums(role_para[,5]*role_para[,1:4]))

ggplot(role_count1, aes(x = role, y = count, fill = role)) + 
  geom_bar(stat = "identity", width = 0.75) + 
  xlab("妹子们") +
  ylab("亲密值") +
  theme(axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title=element_text(size=20,face="bold"),
        axis.title.x = element_text(vjust=-2),
        legend.position="none")


#### density of different names of one role 
rolesl = strsplit(roles, " ")

#### (1) 殷离
roles_para = sapply(rolesl[[1]], grepl, yitian_para)
colnames(roles_para) = rolesl[[1]]
roles_para_df = data.frame(role = factor(rep(colnames(roles_para), colSums(roles_para))), 
                           para = which(roles_para>0, arr.ind = T)[,1])


ggplot(roles_para_df, aes(para, fill = role, colour = role))  + 
  geom_density(aes(y = ..density..), alpha=0.2, size = 1) + 
  xlab("出现段落") + xlim(0, 4614)+ 
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))

#### (2) 周芷若
roles_para = role_para[,5]*sapply(rolesl[[2]], grepl, yitian_para)[,c(2,3,4,7)]
colnames(roles_para) = rolesl[[2]][c(2,3,4,7)]
roles_para_df = data.frame(role = factor(rep(colnames(roles_para), colSums(roles_para))), 
                           para = which(roles_para>0, arr.ind = T)[,1])


ggplot(roles_para_df, aes(para, fill = role, colour = role))  + 
  geom_density(aes(y = ..density..), alpha=0.2, size = 1) + 
  xlab("出现段落") + xlim(0, 4614)+ 
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))


#### (2) 赵敏
roles_para = role_para[,5]*sapply(rolesl[[3]], grepl, yitian_para)[,c(1,4,5,6)]
colnames(roles_para) = rolesl[[3]][c(1,4,5,6)]
roles_para_df = data.frame(role = factor(rep(colnames(roles_para), colSums(roles_para))), 
                           para = which(roles_para>0, arr.ind = T)[,1])


ggplot(roles_para_df, aes(para, fill = role, colour = role))  + 
  geom_density(aes(y = ..density..), alpha=0.2, size = 1)+ 
  xlab("出现段落") +  
  theme(plot.margin = unit(c(0,1,1,0), "cm"),
        axis.text=element_text(size=17, family = "wqy-microhei"),
        axis.title.x = element_text(vjust=-2),
        axis.title=element_text(size=20,face="bold"))

#### split words
library(jiebaR)
cutter = worker(bylines = TRUE, stop_word = "stop.txt")
new_user_word(cutter, unlist(rolesl), rep("n", length(unlist(rolesl))))  ### insert new words

yitian_words = cutter[yitian_para]
yitian_split = sapply(yitian_words, paste, collapse = " ")
writeLines(yitian_split, "yitian_split.txt")

#### train word2vec
library(wordVectors)
model = train_word2vec("yitian_split.txt", output="yitian_split.bin", 
                       threads = 3, vectors = 100, window=12, force = T)
vec = read.vectors("yitian_split.bin")

nearest_to(model,model[["张无忌"]])
nearest_to(model,model[["赵敏"]])
nearest_to(model,model[["周芷若"]])


rr = sapply(rolesl, function(x) x[1])
cos_dist = cosineDist(vec[rr],vec[rr])
hc = hclust(as.dist(cos_dist), method = "average")
plot(hc)




library(ggplot2)

simulated<-read.csv("fat_03_4_simulated.csv",header = TRUE)
simulated<-simulated[1:999,]

approximation<-read.csv("fat_03_4_approx.txt",header = FALSE)
approximation<-approximation[1:999,]

simvsapprox<-data.frame(x_cords=simulated$x_cords,simulated=simulated$allele_frequency,
                        approx=approximation$V2)

g1<-ggplot(simvsapprox, aes(x_cords)) +
  geom_line(aes(y = simulated, color = "modified Gamma(4,0.3) simulation"), linetype = "solid") +
  geom_line(aes(y = approx, color = "Model approximation"), linetype = "dashed")+
  ylim(0,1)+theme_bw()+ylab("Allele Frequency") + xlab("X cordinates")+
  scale_color_manual(name = "Key", values = c("modified Gamma(4,0.3) simulation" = "black", "Model approximation" = "blue"))+ 

  annotate("rect", xmin = -10, xmax = 10, ymin = 0, ymax = 1, fill = "red", alpha = 0.5) +
  geom_label(aes(x = 0, y = 0.1, label = "Negative selection"), fill = "red", alpha = 1)+ylim(-250,250)

ggsave(file = "cline.png",g1, dpi = 600, width = 8, height = 6, units = "in")

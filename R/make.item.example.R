
library(diagram)

names <- c('Sites','Phases','C14Samples','Graves','Individual.A','Individual.B','tooth:M1','tooth:M2','tooth:M2')
M <- matrix(nrow = 9, ncol = 9, byrow = TRUE, data = 0)
M[2,1] <- M[3,2] <- M[4,2] <- M[5,4] <- M[6,4] <- M[7,5] <- M[8,5] <- M[9,6]  <- ''
pos <- c(1,1,2,2,3)


png('../tools/plots/item.example5.png',width=700,height=700)
pp <- plotmat(M, pos=pos, name=names, lwd = 1, box.lwd = 2, cex.txt = 0.8, box.type = "square", box.prop = 0.4, txt.yadj=0)
text(pp$comp[5,1],pp$comp[5,2],'erewtwet',pos=1,cex=0.6)
dev.off()
# Theme to be used for posters in ggplot2
library(grid)
library(ggplot2)
poster.theme1<-theme(plot.title=element_text(face="bold", size=rel(3))) + 
theme(legend.text=element_text(size=rel(1.5)), 
      legend.title=element_text(size=rel(2)),
      legend.key=element_blank(),
      legend.key.size=unit(2, "lines")) + 
      # legend.key.width=unit(2, "lines")) + 
theme(axis.line=element_line(color="black", size=0.5), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.border=element_blank(), 
      panel.background=element_blank(), 
      axis.text.x=element_text(angle=0, color="black", size=rel(2.5)), 
      axis.text.y=element_text(color="black", size=rel(2.5)), 
      axis.title.x=element_text(face="bold", size=rel(2.25), vjust=-0.5),  
      axis.title.y=element_text(face="bold", size=rel(2.25), vjust=1))

poster.theme2 <- theme(axis.line=element_line(color="black"), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                    panel.background=element_blank(), axis.text.x=element_text(angle=0, color="black", size=21),
                    axis.text.y=element_text(angle=0, color="black", size=21), axis.title.x=element_text(face="bold", size=28),
                    axis.title.y=element_text(face="bold", size=28), strip.text=element_text(face="bold", size=rel(1.75)),
                    title=element_text(face="bold", size=30), legend.text=element_text(size=18))
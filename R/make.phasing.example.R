#-----------------------------------------------------------------------------------------
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
#-----------------------------------------------------------------------------------------
# Example 1
#-----------------------------------------------------------------------------------------
png('../tools/plots/phasing.example.1.plot.png',width=800,height=400)
par(mar=c(0,0,0,0))
plot(NULL, xlim=c(0,1),ylim=c(0,1),xlab='',ylab='',xaxt='n',yaxt='n',bty='n')

arrows(0,0,1,0, lwd=2, col='grey')
text(x=0.5, y=0.02, 'time', col='grey')

rect(0,0.6,0.3,0.9,border='steelblue',lwd=3)
text(x=0.15,y=0.75, 'Early Neolithic (EN)')
text(x=0.15,y=0.7, 'Faunal n=121',col='steelblue')

rect(0.3,0.6,0.7,0.9,border='steelblue',lwd=3)
text(x=0.5,y=0.75, 'Middle Neolithic (MN)')
text(x=0.5,y=0.7, 'Faunal n=2',col='steelblue')

rect(0.8,0.6,1,0.9,border='steelblue',lwd=3)
text(x=0.9,y=0.75, 'Early Bronze Age (EBA)')
text(x=0.9,y=0.7, 'Faunal n=79',col='steelblue')

rect(0.05,0.1,0.65,0.4,border='firebrick',lwd=3)
text(x=0.35,y=0.25, 'Undetermined Neolithic (UN)')
text(x=0.35,y=0.2, 'Botanical n=72',col='firebrick')
dev.off()

example.1 <- '
digraph cluster {
node[ shape = none, fontname = "Arial" ]
rankdir=LR;

phases[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td colspan="4"><b>Phases</b></td></tr>
<tr><td>PhaseID</td><td>Order</td><td>Period</td><td>etc...</td></tr>
<tr><td>A</td><td>1</td><td>UN</td><td></td></tr>
<tr><td>B</td><td>2</td><td>EBA</td><td></td></tr>
</table>>];
}'

export_svg(grViz(diagram=example.1)) %>% charToRaw %>% rsvg_png("../tools/plots/phasing.example.1.table.png")
#-----------------------------------------------------------------------------------------
# Example 2
#-----------------------------------------------------------------------------------------
png('../tools/plots/phasing.example.2.plot.png',width=800,height=400)
par(mar=c(0,0,0,0))
plot(NULL, xlim=c(0,1),ylim=c(0,1),xlab='',ylab='',xaxt='n',yaxt='n',bty='n')

arrows(0,0,1,0, lwd=2, col='grey')
text(x=0.5, y=0.02, 'time', col='grey')

rect(0,0.6,0.3,0.9,border='steelblue',lwd=3)
text(x=0.15,y=0.75, 'Early Neolithic (EN)')
text(x=0.15,y=0.7, 'Faunal n=121',col='steelblue')

rect(0.3,0.6,0.7,0.9,border='steelblue',lwd=3)
text(x=0.5,y=0.75, 'Middle Neolithic (MN)')
text(x=0.5,y=0.7, 'Faunal n=233',col='steelblue')

rect(0.8,0.6,1,0.9,border='steelblue',lwd=3)
text(x=0.9,y=0.75, 'Early Bronze Age (EBA)')
text(x=0.9,y=0.7, 'Faunal n=79',col='steelblue')

rect(0.05,0.1,0.65,0.4,border='firebrick',lwd=3)
text(x=0.35,y=0.25, 'Undetermined Neolithic (UN)')
text(x=0.35,y=0.2, 'Botanical n=72',col='firebrick')
dev.off()

example.2 <- '
digraph cluster {
node[ shape = none, fontname = "Arial" ]
rankdir=LR;

phases[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td colspan="4"><b>Phases</b></td></tr>
<tr><td>PhaseID</td><td>Order</td><td>Period</td><td>etc...</td></tr>
<tr><td>A</td><td></td><td>UN</td><td></td></tr>
<tr><td>B</td><td>1</td><td>EN</td><td></td></tr>
<tr><td>C</td><td>2</td><td>MN</td><td></td></tr>
<tr><td>D</td><td>3</td><td>EBA</td><td></td></tr>
</table>>];
}'

export_svg(grViz(diagram=example.2)) %>% charToRaw %>% rsvg_png("../tools/plots/phasing.example.2.table.png")
#-----------------------------------------------------------------------------------------

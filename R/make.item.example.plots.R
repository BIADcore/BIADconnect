#-----------------------------------------------------------------------------------------
# Graphviz
#-----------------------------------------------------------------------------------------
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
#-----------------------------------------------------------------------------------------
# example 1
#-----------------------------------------------------------------------------------------
# overview
#-----------------------------------------------------------------------------------------
example.1.overview <- '
digraph cluster {
node[ shape = none, fontname = "Arial" ]
rankdir=LR;

sites[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">Sites</font></td></tr>
</table>>];

phases[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">Phases</font></td></tr>
</table>>];

graves[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">Graves</font></td></tr>
</table>>];

c14samples[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">C14Samples</font></td></tr>
</table>>];

graveindividuals[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">GraveIndividuals</font></td></tr>
</table>>];

strontium[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td port="1"><font color="blue">Strontium</font></td></tr>
</table>>];

sites:1 -> phases:1 [dir="both"]
phases:1 -> graves:1 [dir="both"]
phases:1 -> c14samples:1 [dir="both"]
graves:1 -> graveindividuals:1 [dir="both"]
graveindividuals:1 -> strontium:1 [dir="both"]
}'
#-----------------------------------------------------------------------------------------
# items
#-----------------------------------------------------------------------------------------
example.1.items <- '
digraph cluster {
node[ shape = none, fontname = "Arial" ]
rankdir=LR;

strontium[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td colspan="5"><font color="blue">Strontium</font></td></tr>
<tr><td>StrontiumID</td><td>IndividualID</td><td>ItemID</td><td>Element</td><td>87Sr/86Sr</td></tr>
<tr><td port="1">sr01</td><td>ind01</td><td>it01</td><td>upper right M1</td><td>0.70789</td></tr>
</table>>];

c14samples[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td colspan="4"><font color="blue">C14Samples</font></td></tr>
<tr><td>C14ID</td><td>ItemID</td><td>LabID</td><td>Element</td></tr>
<tr><td port="1">c01</td><td>it01</td><td>OxA-1234</td><td>upper right M1</td></tr>
<tr><td port="2">c02</td><td>it02</td><td>OxA-5678</td><td>lower left M2</td></tr>
 </table>>];

graveindividuals[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td colspan="3"><font color="blue">GraveIndividuals</font></td></tr>
<tr><td>IndividualID</td><td>GraveID</td><td>ItemID</td></tr>
<tr><td port="1">ind01</td><td>grave01</td><td>it02</td></tr>
</table>>];

items[ label=<
<table border="0" cellborder="1" cellspacing="0" cellpadding="4">
<tr><td><font color="blue">Items</font></td></tr>
<tr><td>ItemID</td></tr>
<tr><td port="1">it01</td></tr>
<tr><td port="2">it02</td></tr>
</table>>];

items:1 -> c14samples:1 [dir="both"]
items:1 -> strontium:1 [dir="both"]
items:2 -> c14samples:2 [dir="both"]
items:2 -> graveindividuals:1 [dir="both"]

}'
#-----------------------------------------------------------------------------------------
export_svg(grViz(diagram=example.1.overview)) %>% charToRaw %>% rsvg_png("../tools/plots/items.example.1.overview.png")
export_svg(grViz(diagram=example.1.items)) %>% charToRaw %>% rsvg_png("../tools/plots/items.example.1.items.png")
#-----------------------------------------------------------------------------------------
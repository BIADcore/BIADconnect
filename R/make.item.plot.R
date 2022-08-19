#------------------------------------------------------------------
# R script for relationships used in Items table
#------------------------------------------------------------------
library(DiagrammeR)
library(rsvg)
library(DiagrammeRsvg)
#------------------------------------------------------------------
# Example 1 
# A single human individual with 87Sr/86Sr, δ13C and δ15N data from the upper right M1 tooth
#------------------------------------------------------------------
item.plot <- grViz("
digraph {	
graph [layout = dot,rankdir = LR]
{
node [shape = oval, style = filled, fillcolor = skyblue, height = 1, width = 3, fontsize = 15, fontname = Helvetica] 
Sites Phases Graves GraveIndividuals Strontium C14Samples
}

{
Sites -> Phases [dir=none, penwidth=3, label='SiteID']
Phases -> Graves [dir=none, penwidth=3, label='PhaseID']
Graves -> GraveIndividuals [dir=none, penwidth=3, label='GraveID']
GraveIndividuals -> Strontium [dir=none, penwidth=3, label='IndividualID']
Phases -> C14Samples [dir=none, penwidth=3, label='PhaseID']
}
}",height=200)

item.plot


export_svg(item.plot) %>% charToRaw %>% rsvg_png("../tools/plots/database.items.example1.png")
#------------------------------------------------------------------
# Example 2
# A single human individual with 87Sr/86Sr data from the lower left M1 and a radiocarbon date from the right femur
#------------------------------------------------------------------
item.plot <- DiagrammeR::grViz("
digraph {
 node [shape = record];
{
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 2,
  fontsize = 15]
  Items Strontium C14Samples GraveIndividuals
  }
{
node [shape = box
  style = filled
  fillcolor = white
  fixedsize = true,
  width = 2,
  fontsize = 15]
  ItemID
  }
{
GraveIndividuals -> ItemID [dir = both]
Items -> ItemID
ItemID -> Strontium
ItemID -> C14Samples
}
subgraph cluster {
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 1.2,
  fontsize = 10]
  DataTable
  node [shape = box
  fillcolor = white,
  fixedsize = true,
  width = 1.2
  fontsize = 10]
  DataID}
}
")
item.plot
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example2.png")

#------------------------------------------------------------------
# Example 3 - two individuals from a single grave, each with one radiocarbon date
#------------------------------------------------------------------
item.plot <- DiagrammeR::grViz("
digraph circo{
 node [shape = record];
{
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 2,
  fontsize = 15]
  Graves GraveIndividuals Items C14Samples
  }
{
Graves -> GraveIndividuals
GraveIndividuals -> ItemID [dir = both]
Items -> ItemID
ItemID -> C14Samples
}
subgraph cluster {
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 1.2,
  fontsize = 10]
  DataTable
  node [shape = box
  fillcolor = white,
  fixedsize = true,
  width = 1.2
  fontsize = 10]
  DataID}
}
")
item.plot
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example3.png")

#------------------------------------------------------------------
# Example 4 - two individuals and a canine from a single grave, first individual with
# 3 strontium sampled teeth, 1 tooth used for aDNA, 1 femur used
# for radiocarbon data, second individual's tooth samled for carbon and nitrogen,
# canine sampled for faunal data
#------------------------------------------------------------------
item.plot <- DiagrammeR::grViz("
digraph {
 node [shape = record];
{
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 2,
  fontsize = 15]
  Graves GraveIndividuals Items C14Samples FaunalSpecies FaunalBiometrics Strontium HumanIsotopes MaterialCulture
  }
{
node [shape = circle
  style = filled,
  style = dotted,
  fillcolor = orange,
  fixedsize = true,
  width = 2,
  fontsize = 15]
  aDNA
  }  
{
node [shape = box
  style = filled
  fillcolor = white
  fixedsize = true,
  width = 2,
  fontsize = 15]
  ItemID1 ItemID2
}
subgraph {
{rank = same Graves FaunalSpecies FaunalBiometrics HumanIsotopes MaterialCulture C14Samples Strontium aDNA}
Graves -> GraveIndividuals
ItemID1 -> aDNA [dir = both]
ItemID1 -> GraveIndividuals [dir = both]
ItemID1 -> C14Samples [dir = both]
ItemID1 -> Strontium [dir = both]
ItemID2 -> MaterialCulture [dir = both]
ItemID2 -> FaunalBiometrics [dir = both]
ItemID2 -> FaunalSpecies [dir = both]
ItemID2 -> HumanIsotopes [dir = both]
ItemID2 -> Graves [dir=both]
}
subgraph {
{rank = same ItemID1 ItemID2}
Items -> ItemID1
Items -> ItemID2
}
subgraph cluster {
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 1.2,
  fontsize = 10]
  DataTable
  node [shape = box
  fillcolor = white,
  fixedsize = true,
  width = 1.2
  fontsize = 10]
  DataID
  node [shape = circle
  style = dotted,
  fillcolor = white,
  fixedsize = true,
  width = 1.2
  fontsize = 10]
  'Under construction'}
}
")
item.plot
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example4.png")

#------------------------------------------------------------------
# Example 5 - two individuals from a single grave with one radiocarbon
# date each
#------------------------------------------------------------------
item.plot <- DiagrammeR::grViz("
digraph circo{
 node [shape = record];
{
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 2,
  fontsize = 15]
  GraveIndividuals Items Strontium HumanIsotopes
  }
  {
  node [shape = box
  style = filled
  fillcolor = white
  fixedsize = true,
  width = 2,
  fontsize = 15]
  ItemID
  }
subgraph{
{rank = same GraveIndividuals}
Items -> ItemID
}
subgraph{
GraveIndividuals -> ItemID [dir= both]
}
subgraph{
{rank = same Strontium HumanIsotopes Items}
ItemID -> Strontium [dir = both]
ItemID -> HumanIsotopes [dir = both]
}
subgraph cluster {
node [shape = circle
  style = filled,
  fillcolor = orange,
  fixedsize = true,
  width = 1.2,
  fontsize = 10]
  DataTable
  node [shape = box
  fillcolor = white,
  fixedsize = true,
  width = 1.2
  fontsize = 10]
  DataID}
}
")
item.plot
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example5.png")

#------------------------------------------------------------------------------------



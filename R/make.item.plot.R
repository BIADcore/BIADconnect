#------------------------------------------------------------------
# Example R script for relationships used in Items table
#------------------------------------------------------------------
# Created: 03.08.2022
# Last modified: 03.08.2022

#------------------------------------------------------------------
# Example 1 - single buried individual with 87Sr/86Sr δ13C and δ15N
# data from the upper right M1 tooth
#------------------------------------------------------------------

library(DiagrammeR)
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
  Items Strontium HumanIsotopes GraveIndividuals
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
library(rsvg)
library(DiagrammeRsvg)
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example1.png")

#------------------------------------------------------------------
# Example 2 - single buried individual with 87Sr/86Sr data from the
# lower left M1 and a radiocarbon date from the right femur
#------------------------------------------------------------------

library(DiagrammeR)
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
library(rsvg)
library(DiagrammeRsvg)
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example2.png")

#------------------------------------------------------------------
# Example 3 - two individuals from a single grave, each with one radiocarbon
# date
#------------------------------------------------------------------

library(DiagrammeR)
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
library(rsvg)
library(DiagrammeRsvg)
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example3.png")

#------------------------------------------------------------------
# Example 4 - two individuals and a canine from a single grave, first individual with
# 3 strontium sampled teeth, 1 tooth used for aDNA, 1 femur used
# for radiocarbon data, second individual's tooth samled for carbon and nitrogen,
# canine sampled for faunal data
#------------------------------------------------------------------

library(DiagrammeR)
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
ItemID1 -> Strontium  [dir = both]
ItemID1 -> aDNA [dir = both]
ItemID1 -> GraveIndividuals [dir = both]
ItemID1 -> C14Samples [dir = both]
ItemID1 -> Strontium [dir = both]
ItemID2 -> GraveIndividuals [dir = both]
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
library(rsvg)
library(DiagrammeRsvg)
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example4.png")

#------------------------------------------------------------------
# Example 5 - two individuals from a single grave with one radiocarbon
# date each
#------------------------------------------------------------------

library(DiagrammeR)
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
  ItemID1
  }
subgraph{
{rank = same GraveIndividuals}
Items -> ItemID1
}
subgraph{
GraveIndividuals -> ItemID1 [dir= both]
}
subgraph{
{rank = same Strontium HumanIsotopes Items}
ItemID1 -> Strontium [dir = both]
ItemID1 -> HumanIsotopes [dir = both]
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
library(rsvg)
library(DiagrammeRsvg)
export_svg(item.plot) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.items.example5.png")
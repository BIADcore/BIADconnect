#------------------------------------------------------------------
# Example R script for relationships used in core import
#------------------------------------------------------------------
# Created: 28.07.2022
# Last modified: 28.07.2022

library(DiagrammeR)
core.import <- DiagrammeR::grViz("
digraph {
#data tables
  node [shape = circle
  style = filled,
  color = orange,
  fixedsize = true,
  width = 2.2,
  fontsize = 15]
  Citations; Sites; Phases; PhaseCitation; PhaseType

#list tables
  node [shape = box
  style = filled,
  color = lightblue
  fixedsize = true,
  width = 3.0
  fontsize = 15]

  edge [color = dimgray]  
Citations -> Sites -> Phases
Citations -> PhaseCitation -> Phases
zoptions_Types -> PhaseType -> Phases
zoptions_Countries -> Sites
zoptions_Cultures -> Phases
zoptions_Periods -> Phases
subgraph cluster {
node [shape = circle
  style = filled,
  color = orange,
  fixedsize = true,
  width = 1,
  fontsize = 10]
  DataTable
  node [shape = box
  style = filled,
  color = lightblue
  fixedsize = true,
  width = 1
  fontsize = 10]
  List
}
}
")
export_svg(core.import) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.core.import.png")
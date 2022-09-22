#------------------------------------------------------------------
# Example R script for relationships used in core import
#------------------------------------------------------------------
library(DiagrammeR)
core.import <- DiagrammeR::grViz("
digraph {
 node [shape = record];
{
node [shape = rectangle style = filled fillcolor = white]
Citations Sites
}
{
node [shape = rectangle style = filled fillcolor = white]
Phases PhaseCitation PhaseType
}
subgraph {
{rank=same Sites Citations}
Sites -> Phases [dir = back]
Citations -> PhaseCitation [dir = back];
}
subgraph {
{rank=same PhaseCitation PhaseType Phases}
PhaseCitation -> Phases [dir = back]
}
subgraph {
{rank=same  Phases}
Phases -> PhaseType;
}
}
")
core.import
library(rsvg)
library(DiagrammeRsvg)
export_svg(core.import) %>%
  charToRaw %>%
  rsvg_png("../tools/plots/database.core.import.png")
#------------------------------------------------------------------
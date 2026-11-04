# Engagement loop figure for the "smart digital health app" slide.
# Adapted from ~/github/proj_rudra1/presentations/engagement_loop.qmd, restyled for the deck.
# Run manually; writes images/engagement_loop.png, which usf.qmd includes as a static image.
# Needs Source Sans Pro installed as a system font (the deck's revealjs font; copied from
# /opt/quarto/share/formats/revealjs/reveal/dist/theme/fonts/source-sans-pro to
# ~/.local/share/fonts). Otherwise rsvg silently falls back to another sans font.

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# Deck colors: day blue (model figures), UW red (titles/accents), dark gray (arrows)
day_blue <- "#0072B2"
uw_red <- "#c5050c"
arrow_gray <- "#666666"

node_label <- function(step, detail) {
  paste0(
    "<<FONT FACE='Source Sans Pro Semibold' POINT-SIZE='30'>", step, "</FONT>",
    "<BR/><BR/><FONT FACE='Source Sans Pro' POINT-SIZE='18'>", detail, "</FONT>>"
  )
}

loop_dot <- glue::glue('
digraph loop {{
  graph [layout = neato, splines = curved, outputorder = edgesfirst, bgcolor = "transparent"]
  node  [shape = circle, fixed = true, width = 2.75, style = filled, fillcolor = "white",
         color = "{day_blue}", penwidth = 4, fontcolor = "black"]
  edge  [color = "{arrow_gray}", penwidth = 3, arrowsize = 1.1]

  reflect  [pos = "0,3.1!",  label = {node_label("Reflect", "1-minute<BR/>daily reflection")}]
  discover [pos = "3.1,0!",  label = {node_label("Discover", "recovery health score<BR/>and today&#39;s top factor")}]
  learn    [pos = "0,-3.1!", label = {node_label("Learn", "a brief recovery tool<BR/>matched to today")}]
  reward   [pos = "-3.1,0!", label = {node_label("Reward", "incentives for<BR/>reflecting and learning")}]

  reflect  -> discover
  discover -> learn
  learn    -> reward
  reward   -> reflect
  # ports send the dashed feedback arrow through the loop interior
  discover:w -> reflect:s [style = dashed, color = "{uw_red}"]
  fb_label [pos = "-0.15,0.75!", shape = plaintext, style = "", width = 0,
            label = <<FONT FACE="Source Sans Pro Semibold" POINT-SIZE="22" COLOR="{uw_red}">sharpens<BR/>self-monitoring</FONT>>]
}}
')

svg <- export_svg(grViz(loop_dot))
rsvg_png(charToRaw(svg), here::here("images/engagement_loop.png"), width = 1600)

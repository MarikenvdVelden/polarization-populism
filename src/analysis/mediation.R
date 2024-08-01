library(DiagrammeR)

med <- '
# Mediator
picom_z ~ a*pid + country +gender + age + educ

# Outcome
affpol_z ~ b*picom_index + c*pid + country +gender + age + educ

# Indirect effect
ab := a*b

# Total effect
total := c + ab
'

m4 <- lavaan::sem(med, data=df)

# Extract standardized coefficients
coefs <- lavaan::parameterEstimates(m4)
# Extract standardized coefficients and their errors
paths <- coefs[coefs$op %in% c("~", ":="), c("lhs", "op", "rhs", "est", "se","pvalue")]


# Ensure paths has the correct structure and length
if (nrow(paths) < 3) {
  stop("Not enough paths extracted. Check model and data.")
}

# Format coefficients to two decimal places
formatted_paths <- sapply(seq_len(nrow(paths)), function(i) {
  estimate <- format(round(paths$est[i], 2), nsmall = 2)
  stderr <- format(round(paths$se[i], 2), nsmall = 2)
  pvalue <- paths$pvalue[i]
  significance <- ifelse(pvalue < 0.05, "*", "")  # Mark as significant if p < 0.05
  sprintf("%s (%s)%s", estimate, stderr, significance)
})

# Create the Graphviz dot code for a pyramid layout with Arial Narrow font and formatted effect sizes
graph <- paste(
  'digraph {',
  '  graph [rankdir=TB, splines=polyline, ranksep=1.0, nodesep=0.5, bgcolor=white];',
  '  node [shape=box, style=filled, fillcolor=gray95, fontname="Arial Narrow", fontsize = 10];',
  '  edge [color=black, fontname="Arial Narrow", fontsize = 10];',
  '',
  '  // Define nodes',
  '  pid [label="PC", pos="0,0!"];',
  '  aff_pol [label="Affective Polarization", pos="1,0!"];',
  '  picom_index [label="Mediation: Conspiracy Mentality", pos="0.5,1!"];',
  '',
  '  // Define edges with formatted effect sizes',
  sprintf('  pid -> picom_index [label="%s"];', formatted_paths[1]),
  sprintf('  picom_index -> aff_pol [label="%s"];', formatted_paths[2]),
  sprintf('  pid -> aff_pol [label="%s"];', formatted_paths[3]),
  '',
  '  // Create ranks',
  '  { rank = same; pid; aff_pol; }',
  '  { rank = min; picom_index; }',
  '}',
  sep = "\n"
)
# Plot the graph
grViz(graph)

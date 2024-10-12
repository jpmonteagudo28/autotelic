# Create function to compute prior, likelihood, 
# posterior and print posterior plot
create_posterior_plot <- function(trials, 
                                  successes,
                                  grid_points,
                                  ...,
                                  mtext_label = NULL,
                                  xlab = NULL, 
                                  ylab = NULL){
  
  args <- list(...)
  
  # Define the grid
  p_grid <- seq(0,1, length.out = grid_points)
  
  # Define the prior
  prior <- if ("prior" %in% names(args)) args$prior else rep(1, grid_points)
  
  # Ensure the prior length matches grid_points
  if (length(prior) != grid_points) {
    stop("Length of prior must match grid_points")
  }
  
  # Compute the likelihood for each value in the grid
  likelihood <- dbinom(successes,trials,prob = p_grid)
  
  # Compute unstandardized posterior
  unstd_posterior <- likelihood * prior
  
  # Standardize posterior
  posterior <- unstd_posterior/sum(unstd_posterior)
  
  plot(p_grid, posterior, 
        type = "b",
        bty = "n",
        xlab = xlab,
        ylab = ylab,
        pch = 16, 
        col = "#7c225c",
        family = font_family,
       args)
  
  if(!is.null(mtext_label)){
    mtext(mtext_label, 
          family = font_family)
  }
  
  # Save the R plot
  plot_obj = recordPlot()
  
  
  result <- list(
    figure = plot_obj,
    grid = p_grid,
    prior = prior,
    likelihood = likelihood,
    unstandardized_posterior = unstd_posterior,
    posterior = posterior
  )
  
  # Set the class of the result to enable custom print method
  class(result) <- "posterior_grid"
  
  return(result)
}

print.posterior_grid <- function(x, ...) {
  
  # Display the plot
  print(x$figure)
  
  # Print a message about accessing other components
  cat("Use $grid, $prior, $likelihood, $unstandardized_posterior, or $posterior to access respective components.\n")
  
  # Return invisibly
  invisible(x)
}

# Set Google font for base R and ggplot2 plots
set_plot_font <- function(name,family){
  
  if(missing(family)){
    family = name
  }
  
  required_packages <- c("showtext", "sysfonts")
  missing_packages <- required_packages[!sapply(required_packages, 
                                                requireNamespace, quietly = TRUE)
                                        ]
  
  if (length(missing_packages) > 0) {
    
    stop("The following required package(s) are not installed: ", 
         paste(missing_packages, collapse = ", "), 
         ". Please install them using install.packages().")
  }
  
  # Add the Google font
  tryCatch({
    sysfonts::font_add_google(name, family)
  }, error = function(e) {
    stop("Failed to add Google font. Error: ", e$message)
  })
  
  # Enable custom font for all devices
  showtext::showtext_auto()
  
  # Set the font as the default for base R plots
  par(family = family)
  
  message("Plot font set to '", name, "'. Use 'reset_font' to reset to default.")
  
  invisible(family)
}

reset_plot_font <- function(family){
  
  par(family = family)
  showtext::showtext_auto(enable = FALSE)
  
  message("Plot font set to '", family, "'.")
}

set_plot_theme <- function(
  font_family = "Montserrat",   # if font_family selected, applies to all text in plot
  axis_font = 1,                # 1 - reg, 2 - bold, 3 - italic, 4 - bold-italic, 5- symbol
  label_font = 1,
  title_font = 1,
  point_type = 16,
  box_type = "n",
  color = "#7c225c",
  title_color = "black",
  sub_color = "black",
  bg = "#fffff2",
  plot_dim = c(9, 6),
  axis_line_width = 0.5,
  tick_length = -0.01,
  # as fraction of height of line of text. Use instead of 'tck'
  side_margin = c(5, 4, 4, 2) + 0.1,
  #BLaTteR (bottom, left, top, right)
  margin_line = c(3, 1, 0),
  outer_margin_inches = c(0, 0, 0, 0),
  # BLaTteR (bottom, left, top, right)
  axis_text_size = 0.75,
  axis_label_size = 1,
  title_size = 1.25,
  sub_size = 1
) {
  
  set_plot_font(font_family)
  
  old_par <- par(no.readonly = TRUE)
  
  par(
    family = font_family,
    font.axis = axis_font,
    font.lab = label_font,
    font.main = title_font,
    pch = point_type,
    bty = box_type,
    col = color,
    col.main = title_color,
    col.sub = sub_color,
    bg = bg,
    pin = plot_dim,
    tcl = tick_length,
    mar = side_margin,
    mgp = margin_line,
    omi = outer_margin_inches,
    cex.axis = axis_text_size,
    cex.lab = axis_label_size,
    cex.main = title_size,
    cex.sub = sub_size
  )
  
 invisible(old_par)
 
}

# Reset original plot parameters
reset_plot_theme <- function(old_par = NULL){
  
  if (is.null(old_par)) {
    
    # Reset to R's default graphical parameters
    dev.new()
    default_par <- par(no.readonly = TRUE)
    dev.off()
    
    par(default_par)
  } else {
    
    # Reset to the provided old_par settings
    par(old_par)
  }
  message("Plot parameters reset.")
}

# Create custom axis in base graphics plot
set_axis <- function(sides = c("bottom","left"), 
                     line_width = 0.5,...,
                     at = NULL){
  
  args <- list(...)
  
  stopifnot(is.numeric(line_width),
            is.character(sides)
            )
  
  
  sides <- match.arg(sides,
                     c("bottom","left","top","right"),
                     several.ok = TRUE)
  
  side_map <- c(bottom = 1, left = 2, top = 3, right = 4)
  numeric_sides <- side_map[sides]

  for (side in numeric_sides) {
    do.call(axis, c(list(side = side, 
                         lwd = line_width, 
                         at = at), 
                    args))
  }
}

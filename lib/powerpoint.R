# PowerPoint Export Functions for Ataxia Natural History Analyses
#
# Contains reusable functions for exporting plots to PowerPoint

# FUNCTION: Save plot to PowerPoint ----
#
# Saves a ggplot object to PowerPoint slide using template
# Output file is automatically named based on script name
#
# @param plot_obj ggplot object to save (default: last_plot())
# @param title Character string for slide title
# @param layout PowerPoint layout name (default: "1s")
# @param i Integer; body placeholder index (default: 2)
# @param template Path to PowerPoint template file
# @param master Master slide name (default: "CR")
# @param image Logical; if TRUE, export as raster image instead of vector (default: FALSE)
# @param output_file Path to output file (if NULL, uses script name)
# @return Invisible path to saved file
#
save_plot_to_ppt <- function(plot_obj = NULL,
                              title = "title",
                              layout = "1s",
                              i = 2,
                              template = .ppt.template.file,
                              master = "CR",
                              image = FALSE,
                              output_file = NULL) {

  # Get plot object
  if (is.null(plot_obj)) {
    pp <- last_plot()
  } else {
    pp <- plot_obj
  }

  if (is.null(pp)) {
    stop("No plot found. Please create a plot before saving to PowerPoint.")
  }

  # Fix minus signs
  .fix_plot_minuses <- function(p) {
    p + labs(
      title    = gsub("-", "\u2212", p$labels$title),
      subtitle = gsub("-", "\u2212", p$labels$subtitle),
      caption  = gsub("-", "\u2212", p$labels$caption),
      x        = gsub("-", "\u2212", p$labels$x),
      y        = gsub("-", "\u2212", p$labels$y)
    )
  }

  pp <- .fix_plot_minuses(pp)

  # Create output file path
  if (is.null(output_file)) {
    # Use script name as output file name in same directory
    script_path <- sys.frame(1)$ofile
    if (is.null(script_path)) {
      # Fallback to current script
      script_path <- rstudioapi::getSourceEditorContext()$path
    }
    if (is.null(script_path) || script_path == "") {
      # Last resort fallback
      target_file <- .create_output_filepath(title)
    } else {
      output_file <- sub("\\.R$", ".pptx", script_path)
      target_file <- output_file
    }
  } else {
    target_file <- output_file
  }

  # Create PowerPoint
  ppt <- read_pptx(template) %>%
    add_slide(layout = layout, master = master)

  # Add plot as vector or image
  if (!image) {
    ppt <- ppt %>%
      ph_with(
        dml(print(pp, newpage = FALSE)),
        location = ph_location_type(type = "body", type_idx = i)
      )
  } else {
    ppt <- ppt %>%
      ph_with(
        print(pp, newpage = FALSE),
        location = ph_location_type(type = "body", type_idx = i)
      )
  }

  # Add title and notes
  ppt <- ppt %>%
    ph_with(
      title,
      location = ph_location_type(type = "title")
    ) %>%
    set_notes(
      value = paste("Created at", format(Sys.time(), "%Y-%m-%d %H-%M-%S")),
      location = notes_location_type("body")
    ) %>%
    print(target = target_file)

  message("PowerPoint saved to: ", target_file)
  invisible(target_file)
}

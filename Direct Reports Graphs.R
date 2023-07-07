pkgs = c( "ggridges", "ggplot2", "viridis", "showtext", "ggtext", "hrbrthemes", "dplyr", "arrow" )
sapply( pkgs, require, character.only = TRUE )

dat <- read_parquet("direct_reports.parquet")

dat$`Department Name` = factor( dat$`Department Name`,
                                levels = c(
                                  grep("Company",
                                       sort( unique( dat$`Department Name` ), decreasing = TRUE ),
                                       invert = TRUE,
                                       value = TRUE ),
                                  grep( "Company",
                                        sort( unique( dat$`Department Name` ), decreasing = TRUE), 
                                        value = TRUE) ),
                                ordered = TRUE )

labels_cw <- dat %>% 
  dplyr::group_by( `Department Name` ) %>%
  dplyr::summarise( Mean = round( mean( `Direct Reports`, na.rm = TRUE ), 1 ),
                    N = format( n(), big.mark = ","),
                    .groups = "drop") %>%
  dplyr::mutate( Label = paste0( `Department Name`, " (n=", N, ")") )

labels_cw$Label <- factor( labels_cw$Label, 
                           levels = c( 
                             grep("Company", 
                                  sort( unique( labels_cw$Label ), decreasing = TRUE), 
                                  invert = TRUE, 
                                  value = TRUE), 
                             grep("Company", 
                                  sort( unique( labels_cw$Label ), decreasing = TRUE), 
                                  value = TRUE) ), 
                           ordered = TRUE)

# Fonts
font_add_google("Open Sans", "open")
font_add_google("Acme", "acme")

showtext_auto()

# A color-blind-friendly palette with black:
cbbPalette <- c( rep("#F58216", 14 ), "#0C7BDC")

ggplot( dat, 
        aes( x = `Direct Reports`, 
             y = `Department Name`, 
             fill = `Department Name`, 
             vline_linetype = 3 ) ) +
  geom_density_ridges( alpha = 0.8,
                       scale = 1.2,
                       size = 0.1,
                       bandwidth = 1.0,
                       quantile_lines = TRUE,
                       quantile_fun = function( `Direct Reports`, ... ) mean( `Direct Reports`, na.rm = TRUE ),
                       na.rm = TRUE, show.legend = FALSE ) +
  scale_fill_manual( values = cbbPalette ) +
  coord_cartesian( xlim = c( 0, 30 ) ) +
  scale_x_continuous( breaks = seq( 0, 30, 2 ), position = "bottom" ) +
  scale_y_discrete( labels = NULL ) +
  geom_text( data = labels_cw, 
             aes( x = Mean,  size = 14, label = Mean ),
             show.legend = FALSE,
             nudge_y = 0.2, 
             nudge_x = 0.6,
             family = "acme" ) +
  geom_text(data = labels_cw,
            aes( label = Label, x = 25 ),
            position = position_nudge( y = 0.3, x = 5 ),
            colour = "black", 
            size = 4.5,
            hjust = "right",
            family = "open" ) +
  labs( title = "Supervisor Span of Control in an Organization",
        subtitle = "Number of supervisors displayed in parentheses", 
        y = "",
        x = "Direct Reports Per Supervisor <span style='font-size: 16px;'><strong>(Averages in Bold)</strong></span>" ) + 
  theme_ridges() +
  theme(
    plot.title = element_text( family =  "open", size = 14, margin = margin( 0, 0, 3, 0 ) ),
    plot.subtitle = element_text( family = "open", size = 12, margin = margin( 0, 0, 10, 0 ) ),
    axis.title.x = element_markdown( family = "open", size = 14, hjust = 0.5, margin = margin( 5, 0, 0, 0 ) ),
    axis.text.x = element_text( family = "acme", size = 14 )
  )


ggsave(
  "span-of-control.png",
  plot = last_plot(),
  device = "png",
  scale = 1,
  width = 1152,
  height = 750,
  units = c("px"),
  dpi = 300,
  limitsize = TRUE,
  bg = "white"
)

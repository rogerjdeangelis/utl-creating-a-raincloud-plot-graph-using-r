# utl-creating-a-raincloud-plot-graph-using-r
    %let pgm=utl-creating-a-raincloud-plot-graph-using-r;

    Creating a raincloud plot graph using r

    Output hires plot
    https://tinyurl.com/3u6c7t5m
    https://github.com/rogerjdeangelis/utl-creating-a-raincloud-plot-graph-using-r/blob/main/raincloud.pdf

    github
    https://tinyurl.com/44yscdh6
    https://stackoverflow.com/questions/78096944/trying-to-differentiate-lines-in-a-rain-cloud-plot

       TWO PROGRAMS

            1. r rainbow graph
            2. Test stattransfer

    Related repos on end

    /*               _     _
     _ __  _ __ ___ | |__ | | ___ _ __ ___
    | `_ \| `__/ _ \| `_ \| |/ _ \ `_ ` _ \
    | |_) | | | (_) | |_) | |  __/ | | | | |
    | .__/|_|  \___/|_.__/|_|\___|_| |_| |_|
    |_|
    */

    /**************************************************************************************************************************/
    /*                                                                                                                        */
    /*                      RAINBOW PLOT OF WEIGHT BY AGE                                                                     */
    /*                                                                                                                        */
    /*       +----------------------+------------+------------------------+                                                   */
    /*       |                      USES JITTERING                        |                                                   */
    /*       |                                                            |                                                   */
    /*    50 +          --         FEMALE      MALE                       |                                                   */
    /*       |       -----         WEIGHT      WEIGHT                     |                                                   */
    /*       |       -----    |      F                                    |                                                   */
    /*       |     -------    |      F  \                                 |                                                   */
    /*       |     -------    |       \  \                                |                                                   */
    /* W  40 +    --------   _|_     F-M  \                               +                                                   */
    /* E     |    --------  |   |      F\---M             |   --          |                                                   */
    /* I     |    --------  |   |        \  \             |   -----       |                                                   */
    /* G     |    --------  |   |    F----\---M M         |   -------     |                                                   */
    /* H     |   ---------  |   |    F     \  \/         _|_  --------    |                                                   */
    /* T  35 +   ---------  |   |     \     \ /\M       |   | --------    +                                                   */
    /*       |  ----------  |   |      \     \ /        |   | ---------   |                                                   */
    /* K     |  ----------  |___|    F--\-----/--M      |___| ----------  |                                                   */
    /* G     |     -------  |   |        \ / / \        |   | -------     |                                                   */
    /*       |     -------  |   |      F--\-/---\--M    |   | -----       |                                                   */
    /*    30 +       -----  |   |        / \  M  M      |_ _| ---         +                                                   */
    /*       |       -----  |   |     F-----/-----M       |   ---         |                                                   */
    /*       |         ---  |   |          /  M           |   --          |                                                   */
    /*       |         ---  |   |   F-----/--/--M         |   -           |                                                   */
    /*       |         ---  |   |    F------M             |   -           |                                                   */
    /*    25 +         ---  |   |    F/ /  /                              +                                                   */
    /*       |          --  |_ _|      /  /                               |                                                   */
    /*       |          --    |      F/  /                                |                                                   */
    /*       |           -    |         /                                 |                                                   */
    /*       |           -    |        /                                  |                                                   */
    /*    20 +           -    |       /                                   +                                                   */
    /*       |                       F                                    |                                                   */
    /*       |                     FEMALE      MALE                       |                                                   */
    /*       |                     WEIGHT      WEIGHT                     |                                                   */
    /*       |                                                            |                                                   */
    /*       +----------------------+------------+------------------------+                                                   */
    /*                                                                                                                        */
    /**************************************************************************************************************************/

    /*                    _       _                                           _
    / |  _ __   _ __ __ _(_)_ __ | |__   _____      __   __ _ _ __ __ _ _ __ | |__
    | | | `__| | `__/ _` | | `_ \| `_ \ / _ \ \ /\ / /  / _` | `__/ _` | `_ \| `_ \
    | | | |    | | | (_| | | | | | |_) | (_) \ V  V /  | (_| | | | (_| | |_) | | | |
    |_| |_|    |_|  \__,_|_|_| |_|_.__/ \___/ \_/\_/    \__, |_|  \__,_| .__/|_| |_|
                                                        |___/          |_|
    */

    %utl_rbegin;
    parmcards4;
    library(raincloudplots)
    library(tidyverse)

    mtcars <- data_1x1(
      array_1 = mtcars$mpg,
      array_2 = mtcars$qsec,
      jit_distance = .09,
      jit_seed = 321)

    raincloud_2 <- raincloud_1x1_repmes(
      data = mtcars,
      colors = (c('#F6C337', '#68D2D5')),
      fills = (c('#F6C337', '#68D2D5')),
      line_alpha = .3,
      size = 1,
      alpha = .6,
      align_clouds = FALSE) +
      scale_x_continuous(breaks=c(1,2), labels=c("mpg", "qsec"), limits=c(0, 3)) +
      scale_y_continuous(breaks=c(10,15,20,25), limits=c(10, 25)) +
      ylab("Value") +
      xlab("")+
      theme_classic()

    pdf("d:/pdf/raincloud.pdf");

    raincloud_2$data <- raincloud_2$data %>%
      mutate(Direction = ifelse(last(y_axis) - first(y_axis) > 0, "up", "down"),
                                .by = id)
    raincloud_2$layers[[3]]$aes_params$colour <- NULL
    raincloud_2$layers[[3]]$mapping <- aes(jit, y_axis, group = id,
                                           color = Direction)

    raincloud_2 + scale_color_manual(values = c(up = "gray", down = "black"))
    ;;;;
    %utl_rend;

    /*___    _            _         _        _   _                        __
    |___ \  | |_ ___  ___| |_   ___| |_ __ _| |_| |_ _ __ __ _ _ __  ___ / _| ___ _ __
      __) | | __/ _ \/ __| __| / __| __/ _` | __| __| `__/ _` | `_ \/ __| |_ / _ \ `__|
     / __/  | ||  __/\__ \ |_  \__ \ || (_| | |_| |_| | | (_| | | | \__ \  _|  __/ |
    |_____|  \__\___||___/\__| |___/\__\__,_|\__|\__|_|  \__,_|_| |_|___/_|  \___|_|

    */

    %utl_rbegin;
    parmcards4;
    library(raincloudplots)
    library(tidyverse)
    mtcars <- data_1x1(
      array_1 = mtcars$mpg,
      array_2 = mtcars$qsec,
      jit_distance = .09,
      jit_seed = 321)
    mtcars
    source("c:/temp/fn_tosas9.R")
    fn_tosas9(dataf=mtcars);
    ;;;;
    %utl_rend;
    libame tmp "c:/temp";
    proc print data=tmp.mtcars ;
    run;quit;
    ');

    /**************************************************************************************************************************/
    /*                                                                                                                        */
    /* Obs    ROWNAMES    Y_AXIS    X_AXIS    ID      JIT                                                                     */
    /*                                                                                                                        */
    /*   1        1        21.00       1       1    1.08206                                                                   */
    /*   2        2        21.00       1       2    1.07871                                                                   */
    /*   3        3        22.80       1       3    0.95288                                                                   */
    /*   4        4        21.40       1       4    0.95591                                                                   */
    /*   5        5        18.70       1       5    0.98029                                                                   */
    /* ...                                                                                                                    */
    /*  60       60        16.90       2      28    2.03181                                                                   */
    /*  61       61        14.50       2      29    1.92133                                                                   */
    /*  62       62        15.50       2      30    1.92160                                                                   */
    /*  63       63        14.60       2      31    2.02901                                                                   */
    /*  64       64        18.60       2      32    1.92885                                                                   */
    /*                                                                                                                        */
    /**************************************************************************************************************************/
    /*              _ _           _
     _ __ ___  __ _| | |_ ___  __| |  _ __ ___ _ __   ___  ___
    | `__/ _ \/ _` | | __/ _ \/ _` | | `__/ _ \ `_ \ / _ \/ __|
    | | |  __/ (_| | | ||  __/ (_| | | | |  __/ |_) | (_) \__ \
    |_|  \___|\__,_|_|\__\___|\__,_| |_|  \___| .__/ \___/|___/
                                              |_|
    */

    REPO
    --------------------------------------------------------------------------------------------------------------------------------------
    https://github.com/rogerjdeangelis/utl-Create-a-side-by-side-table-and-graph-using-greplay
    https://github.com/rogerjdeangelis/utl-Graph-with-known-intercept-and-slope
    https://github.com/rogerjdeangelis/utl-Graphics-Surveying-ten-random-locations-in-North-Carolina-using-superinposed-grid
    https://github.com/rogerjdeangelis/utl-R-AI-igraph-list-connections-in-a-non-directed-graph-for-a-subset-of-vertices
    https://github.com/rogerjdeangelis/utl-adding-text-to-an-existing-png-graphic-python-AI
    https://github.com/rogerjdeangelis/utl-changepoint-like-analysis-in-R-and-SAS-elbow-graph
    https://github.com/rogerjdeangelis/utl-classic-sas-and-well-designed-tables-and-ascii-graphics-instead-of-bling
    https://github.com/rogerjdeangelis/utl-color-a-region-under-a-distribution-curve-graph-wps-and-wps-r
    https://github.com/rogerjdeangelis/utl-create-graphs-in-excel-using-excel-chart-templates
    https://github.com/rogerjdeangelis/utl-dygraph-javascript-library-from-MIT
    https://github.com/rogerjdeangelis/utl-excel-report-with-two-side-by-side-graphs-below_python
    https://github.com/rogerjdeangelis/utl-graphics-boxplots-with-jiggled-point-values-alongside
    https://github.com/rogerjdeangelis/utl-how-many-triangles-in-the-polygon-r-igraph-AI
    https://github.com/rogerjdeangelis/utl-identical-side-by-side-text-and-graphics-in-pdf-and-powerpoint
    https://github.com/rogerjdeangelis/utl-identify-linked-and-unliked-paths-r-igraph
    https://github.com/rogerjdeangelis/utl-igraph-find-largest-group-of-unrelated-individuals-in-your-family-reunion
    https://github.com/rogerjdeangelis/utl-quality-graphics-in-R-wps-and-sas
    https://github.com/rogerjdeangelis/utl-r-graphics-vs-wps-base-graphics-layering-in-r-versus-procs-in-wps-base-ggplot2
    https://github.com/rogerjdeangelis/utl-shortest-and-longest-travel-time-from-home-to-work-igraph-AI
    https://github.com/rogerjdeangelis/utl-three-heat-maps-of-bivariate-normal-wps-r-graph-plot
    https://github.com/rogerjdeangelis/utl-under-used-proc-calendar-ascii-graphics
    https://github.com/rogerjdeangelis/utl_R_graphics_polar_plot
    https://github.com/rogerjdeangelis/utl_adding_SAS_graphics_at_an_arbitrary_position_into_existing_excel_sheets
    https://github.com/rogerjdeangelis/utl_classic_sas_graph_greplay_harvard_macro_multiple_plots_per_page
    https://github.com/rogerjdeangelis/utl_classic_sas_graph_three_plots_across_many_methods_long_live
    https://github.com/rogerjdeangelis/utl_custom_graphics_in_R
    https://github.com/rogerjdeangelis/utl_fun_with_line_printer_graphics_editor
    https://github.com/rogerjdeangelis/utl_graph_visualize_crosstab
    https://github.com/rogerjdeangelis/utl_graphics_589_SAS_and_R_graphics_with_code_and_datasets
    https://github.com/rogerjdeangelis/utl_graphics_determine_us_state_from_latitude_and_longitude
    https://github.com/rogerjdeangelis/utl_graphics_fit_a_smooth_line_to_a_scatter_plot_loess
    https://github.com/rogerjdeangelis/utl_graphics_flexibility_of_ascii_bar_charts
    https://github.com/rogerjdeangelis/utl_graphics_plotting_rivers_in_brazil_using_sharpefiles
    https://github.com/rogerjdeangelis/utl_graphics_zipcode_boundary_maps
    https://github.com/rogerjdeangelis/utl_how_to_stack_a_table_and_corresponding_bar_graph
    https://github.com/rogerjdeangelis/utl_javascript_and_classic_map_graphics_with_mouseovers_and_multiple_drilldowns
    https://github.com/rogerjdeangelis/utl_javascript_dygraph_graphics_multipanel_time_series
    https://github.com/rogerjdeangelis/utl_pdf_graphics_top_40_a_sas_ods_graphics_look_at_chicago_public_schools_salaries_by_job
    https://github.com/rogerjdeangelis/utl_polar_graphics_pot_violin_plot
    https://github.com/rogerjdeangelis/utl_proc_gmap_classic_graphics_grid_containing_four_states
    https://github.com/rogerjdeangelis/utl_r_graphics_visualizing_assciation_amoung_many_variables
    https://github.com/rogerjdeangelis/utl_remove_isolated_nodes_from_an_network_r_igraph
    https://github.com/rogerjdeangelis/utl_sas_classic_graphics_15_plots_on_a_page
    https://github.com/rogerjdeangelis/utl_sas_classic_graphics_designing_your_greplay_template
    https://github.com/rogerjdeangelis/utl_sas_classic_graphics_grid_of__proc_univariate_histograms
    https://github.com/rogerjdeangelis/utl_sas_classic_graphs_using_phil_mason_grid_macro_for_layout
    https://github.com/rogerjdeangelis/utl_table_graph_ppt
    https://github.com/rogerjdeangelis/utl_wps_sas_classic_graphics_optimum_minimums_maximums_increments_for-axes



    https://github.com/rogerjdeangelis/utl-AI-computer-vision-fitting-a-circle-to-a-scatter-plot-of-points-wps-r
    https://github.com/rogerjdeangelis/utl-accessing-the-Federal-Reserve-Economic-Data-Base-and-Plotting-the-monthly-unemployment-rate
    https://github.com/rogerjdeangelis/utl-difference-in-difference-trends-plots
    https://github.com/rogerjdeangelis/utl-dropping-down-to-R-from-sas-and-creating-a-box-plot
    https://github.com/rogerjdeangelis/utl-flexible-methods-for-labeling-points-on-a-scatter-plot
    https://github.com/rogerjdeangelis/utl-lattice-plots-six-histograms-in-three-rows-and-two-columns-ggplot-sgpanels-in-sas-wps-and-r
    https://github.com/rogerjdeangelis/utl-meta-analysis-of-a-single-proportion-in-R-with-forest-and-funnel-plots
    https://github.com/rogerjdeangelis/utl-meta-analysis-of-deaths-in-twenty-studies-with-forest-plot
    https://github.com/rogerjdeangelis/utl-optimum-number-of-clusters-elbow-plot
    https://github.com/rogerjdeangelis/utl-overlay-a-small-version-of-a-plot-inside-a-larger-plot-window-within-window-r-ggplot
    https://github.com/rogerjdeangelis/utl-overlaying-histograms-and-scatterplots-in-powerpoint-pdf-and-jpeg
    https://github.com/rogerjdeangelis/utl-plot-eighteen-random-points-within-Nova-Scotia
    https://github.com/rogerjdeangelis/utl-plot-every-column-against-every-other-column-ggplot-ods-wps
    https://github.com/rogerjdeangelis/utl-plot-of-the-rivers-in-brazil-using-public-shapefiles
    https://github.com/rogerjdeangelis/utl-sgplot-labeling-vertical-barcharts-with-percentages
    https://github.com/rogerjdeangelis/utl_convex_hull_maximum-distance-between-two-points-in-a-scatter-plot
    https://github.com/rogerjdeangelis/utl_convex_hull_polygons_encompassing_a_three_dimensional_scatter_plot
    https://github.com/rogerjdeangelis/utl_given_the_latitude_and_longitude_box_plot_a_country
    https://github.com/rogerjdeangelis/utl_meta_analysis_funnel_plot_odds_ratio_vs_standard_error
    https://github.com/rogerjdeangelis/utl_plot_with_two_different_x_axis_for_the_same_variable_in_r
    https://github.com/rogerjdeangelis/utl_simple_convex_hull_polygon_envelop_for_a_scatter_plot
    https://github.com/rogerjdeangelis/utl_ternary_plots_for_US_2016_election
    https://github.com/rogerjdeangelis/utl_visualizing_suspicious_bivariate_outliers_with_2_dimensional_boxplots



    /*              _
      ___ _ __   __| |
     / _ \ `_ \ / _` |
    |  __/ | | | (_| |
     \___|_| |_|\__,_|

    */
creating a raincloud plot graph using r  

#' Word cloud without data splitting
#'
#' @inheritParams common_args
#' @param word_by A character string of the column name to use as the word.
#'  A character column is expected.
#' @param sentence_by A character string of the column name to split the sentence.
#'  A character column is expected.
#'  Either `word_by` or `sentence_by` should be specified.
#' @param count_by A character string of the column name for the count of the word/sentence.
#'  A numeric column is expected.
#'  If NULL, the count of the word/sentence will be used.
#' @param score_by A character string of the column name for the score of the word/sentence.
#'  A numeric column is expected, used for the color of the word cloud.
#'  If NULL, the score will be set to 1.
#' @param count_name A character string to name the legend of count.
#' @param score_name A character string to name the legend of score.
#' @param words_excluded A character vector of words to exclude from the word cloud.
#' @param score_agg A function to aggregate the scores. Default is `mean`.
#' @param minchar A numeric value specifying the minimum number of characters for the word.
#' @param word_size A numeric vector specifying the range of the word size.
#' @param top_words A numeric value specifying the number of top words to show.
#' @param palreverse A logical value to reverse the palette colors.
#' @return A ggplot object
#' @keywords internal
#' @importFrom tidyr unnest
#' @importFrom dplyr reframe slice_max distinct mutate group_by summarise n
WordCloudPlotAtomic <- function(
    data, word_by = NULL, sentence_by = NULL, count_by = NULL, score_by = NULL,
    count_name = NULL, score_name = NULL,
    words_excluded = plotthis::words_excluded, score_agg = mean, minchar = 2,
    word_size = c(2, 8), top_words = 100,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Paired", palcolor = NULL, alpha = 1, palreverse = FALSE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, seed = 8525, ...
) {
    ggplot <- if (getOption("plotthis.gglogger.enabled", FALSE)) {
        gglogger::ggplot
    } else {
        ggplot2::ggplot
    }
    if (sum(is.null(word_by), is.null(sentence_by)) != 1) {
        stop("Either 'word_by' or 'sentence_by' should be specified.")
    }
    if (!is.null(sentence_by) && !is.null(count_by)) {
        stop("Cannot specify 'count_by' when 'sentence_by' is specified.")
    }
    facet_by <- check_columns(data, facet_by, force_factor = TRUE, allow_multi = TRUE)
    count_by <- check_columns(data, count_by)
    score_by <- check_columns(data, score_by)
    if (is.null(score_by)) {
        data$.score <- 1
        score_by <- ".score"
    }
    if (is.null(count_by)) {
        data$.count <- 1
        count_by <- ".count"
    }

    if (!is.null(sentence_by)) {
        sentence_by <- check_columns(data, sentence_by)
        data <- data %>%
            mutate(word = strsplit(tolower(gsub("[[:punct:]]", "", as.character(!!sym(sentence_by)))), "\\b\\s+\\b")) %>%
            unnest(cols = "word")

        if (length(facet_by) == 1) {
            data <- data %>%
                group_by(!!!syms(c("word", facet_by))) %>%
                reframe(
                    word = !!sym("word"),
                    !!sym(facet_by) := !!sym(facet_by),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        } else if (length(facet_by) == 2) {
            data <- data %>%
                group_by(!!!syms(c("word", facet_by))) %>%
                reframe(
                    word = !!sym("word"),
                    !!sym(facet_by[1]) := !!sym(facet_by[1]),
                    !!sym(facet_by[2]) := !!sym(facet_by[2]),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        } else {
            data <- data %>%
                group_by(!!sym("word")) %>%
                reframe(
                    word = !!sym("word"),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        }
    } else {
        word_by <- check_columns(data, word_by)
        data <- data %>% unnest(cols = word_by)

        if (length(facet_by) == 1) {
            data <- data %>%
                group_by(!!!syms(unique(c(word_by, facet_by)))) %>%
                reframe(
                    word = !!sym(word_by),
                    !!sym(facet_by) := !!sym(facet_by),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        } else if (length(facet_by) == 2) {
            data <- data %>%
                group_by(!!!syms(unique(c(word_by, facet_by)))) %>%
                reframe(
                    word = !!sym(word_by),
                    !!sym(facet_by[1]) := !!sym(facet_by[1]),
                    !!sym(facet_by[2]) := !!sym(facet_by[2]),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        } else {
            data <- data %>%
                group_by(!!sym(word_by)) %>%
                reframe(
                    word = !!sym(word_by),
                    count = sum(!!sym(count_by)),
                    score = score_agg(!!sym(score_by)),
                    .groups = "keep"
                )
        }
    }

    data <- data %>%
        filter(!grepl(pattern = "\\[.*\\]", x = !!sym("word"))) %>%
        filter(nchar(!!sym("word")) >= minchar) %>%
        filter(!tolower(!!sym("word")) %in% tolower(words_excluded)) %>%
        distinct() %>%
        slice_max(order_by = !!sym("score"), n = top_words) %>%
        mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) %>%
        as.data.frame()

    colors <- palette_this(data$score, type = "continuous", palette = palette, palcolor = palcolor, matched = FALSE, reverse = palreverse)
    colors_value <- seq(min(data$score, na.rm = TRUE), quantile(data$score, 0.99, na.rm = TRUE) + 0.001, length.out = 100)
    p <- ggplot(data, aes(label = !!sym("word"), size = !!sym("count"), color = !!sym("score"), angle = !!sym("angle"))) +
        ggwordcloud::geom_text_wordcloud(rm_outside = TRUE, eccentricity = 1, shape = "square", show.legend = TRUE, grid_margin = 3) +
        scale_color_gradientn(
            name = score_name %||% "Score", colours = colors, values = scales::rescale(colors_value),
            guide = guide_colorbar(frame.colour = "black", ticks.colour = "black", title.hjust = 0)
        ) +
        scale_size(name = count_name %||% "Count", range = word_size, breaks = ceiling(seq(min(data$count, na.rm = TRUE), max(data$count, na.rm = TRUE), length.out = 3))) +
        guides(size = guide_legend(override.aes = list(colour = "black", label = "G"), order = 1)) +
        labs(title = title, subtitle = subtitle) +
        coord_flip() +
        do.call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    height <- width <- 4.5
    if (legend.position %in% c("right", "left")) {
        width <- width + 1
    } else if (legend.direction == "horizontal") {
        height <- height + 1
    } else {
        height <- height + 2
    }

    attr(p, "height") <- height
    attr(p, "width") <- width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Word Cloud Plot
#'
#' @description Word cloud plot to illustrate the count/frequency of words.
#' @inheritParams common_args
#' @inheritParams WordCloudPlotAtomic
#' @return A ggplot object or wrap_plots object or a list of ggplot objects
#' @export
#' @examples
#' data <- data.frame(
#'    word = c("apple", "banana", "cherry", "date", "elderberry"),
#'    count = c(10, 20, 30, 40, 50),
#'    score = c(1, 2, 3, 4, 5)
#' )
#' WordCloudPlot(data, word_by = "word", count_by = "count", score_by = "score")
WordCloudPlot <- function(
    data, word_by = NULL, sentence_by = NULL, count_by = NULL, score_by = NULL,
    count_name = NULL, score_name = NULL, split_by = NULL, split_by_sep = "_",
    words_excluded = plotthis::words_excluded, score_agg = mean, minchar = 2,
    word_size = c(2, 8), top_words = 100,
    facet_by = NULL, facet_scales = "fixed", facet_ncol = NULL, facet_nrow = NULL, facet_byrow = TRUE,
    theme = "theme_this", theme_args = list(), palette = "Spectral", palcolor = NULL, alpha = 1, palreverse = FALSE,
    aspect.ratio = 1, legend.position = "right", legend.direction = "vertical",
    title = NULL, subtitle = NULL, seed = 8525, combine = TRUE, nrow = NULL, ncol = NULL, byrow = TRUE,
    axes = NULL, axis_titles = axes, guides = NULL, design = NULL, ...
    ) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(data, split_by, force_factor = TRUE, allow_multi = TRUE, concat_multi = TRUE, concat_sep = split_by_sep)

    if (!is.null(split_by)) {
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(legend.direction, names(datas), "legend.direction")
    legend.position <- check_legend(legend.position, names(datas), "legend.position")

    plots <- lapply(
        names(datas), function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) NULL else nm
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            WordCloudPlotAtomic(datas[[nm]],
                word_by = word_by, sentence_by = sentence_by, count_by = count_by, score_by = score_by,
                count_name = count_name, score_name = score_name, words_excluded = words_excluded, score_agg = score_agg, minchar = minchar,
                word_size = word_size, top_words = top_words,
                facet_by = facet_by, facet_scales = facet_scales, facet_ncol = facet_ncol, facet_nrow = facet_nrow, facet_byrow = facet_byrow,
                theme = theme, theme_args = theme_args, palette = palette[[nm]], palcolor = palcolor[[nm]], alpha = alpha, palreverse = palreverse,
                aspect.ratio = aspect.ratio, legend.position = legend.position[[nm]], legend.direction = legend.direction[[nm]],
                title = title, subtitle = subtitle, seed = seed, ...
            )
        }
    )

    combine_plots(plots, combine = combine, nrow = nrow, ncol = ncol, byrow = byrow,
        axes = axes, axis_titles = axis_titles, guides = guides, design = design)
}

#' Word cloud plot (internal)
#'
#' @description
#' Core implementation for drawing a word cloud plot. This is the workhorse
#' behind the exported \code{\link{WordCloudPlot}} function — it takes a
#' **single** data frame (no \code{split_by} support) and returns a
#' \code{ggplot} object rendered via
#' \code{\link[ggwordcloud]{geom_text_wordcloud}}.
#'
#' The function supports two input modes:
#' \itemize{
#'   \item \strong{Pre-tokenized words} (\code{word_by}) — each row contains
#'   a single word (or multiple words in a list column that is unnested).
#'   \item \strong{Sentence splitting} (\code{sentence_by}) — each row
#'   contains a sentence or phrase; the function splits the text into
#'   individual words after removing punctuation and lowercasing.
#' }
#'
#' Words are displayed with \strong{font size} proportional to a count
#' variable (\code{count_by}) and \strong{colour} based on a continuous
#' score variable (\code{score_by}). Text filtering options allow removal of
#' short words, common stop words, and bracketed patterns.
#'
#' @section Architecture:
#' \enumerate{
#'   \item \strong{ggplot dispatch} — selects \code{gglogger::ggplot} or
#'         \code{ggplot2::ggplot} based on
#'         \code{getOption("plotthis.gglogger.enabled")}.
#'   \item \strong{Input validation} — ensures exactly one of \code{word_by}
#'         or \code{sentence_by} is specified. Errors if both are provided
#'         or neither is provided. When \code{sentence_by} is used,
#'         \code{count_by} must be \code{NULL} (counts are derived from
#'         occurrences after splitting).
#'   \item \strong{Column resolution} — \code{facet_by}, \code{count_by},
#'         and \code{score_by} are validated and transformed via
#'         \code{\link{check_columns}}.
#'   \item \strong{Default values} — when \code{score_by} is \code{NULL}, a
#'         synthetic \code{.score} column with value \code{1} is created.
#'         When \code{count_by} is \code{NULL}, a synthetic \code{.count}
#'         column with value \code{1} is created.
#'   \item \strong{Sentence splitting branch} (when \code{sentence_by} is set):
#'         \itemize{
#'           \item The \code{sentence_by} column is validated via
#'                 \code{\link{check_columns}}.
#'           \item Text is lowercased, punctuation is removed, and the
#'                 string is split into individual words on whitespace
#'                 boundaries.
#'           \item Words are unnested into separate rows via
#'                 \code{\link[tidyr]{unnest}}.
#'           \item Data is grouped by word (and \code{facet_by} columns
#'                 when present) and aggregated: \code{sum(count_by)} and
#'                 \code{score_agg(score_by)}. Separate aggregation
#'                 templates handle 0, 1, or 2 \code{facet_by} columns.
#'         }
#'   \item \strong{Word branch} (when \code{word_by} is set):
#'         \itemize{
#'           \item The \code{word_by} column is validated via
#'                 \code{\link{check_columns}}.
#'           \item Column values are unnested into separate rows (supports
#'                 list columns where a row may contain multiple words).
#'           \item Data is grouped by word (and \code{facet_by} columns
#'                 when present) and aggregated similarly to the sentence
#'                 branch.
#'         }
#'   \item \strong{Text filtering pipeline} — the aggregated data is
#'         processed sequentially:
#'         \itemize{
#'           \item Words matching the pattern \verb{[.*]} (bracketed
#'                 content) are removed.
#'           \item Words with fewer than \code{minchar} characters are
#'                 removed.
#'           \item Words listed in \code{words_excluded} are removed
#'                 (case-insensitive matching).
#'           \item Duplicate rows are removed.
#'           \item The top \code{top_words} words by score are retained
#'                 via \code{\link[dplyr]{slice_max}}.
#'         }
#'   \item \strong{Angle assignment} — each word is randomly assigned a
#'         rotation angle of 0 (60\% probability) or 90 degrees (40\%
#'         probability), creating the characteristic word cloud appearance.
#'   \item \strong{Colour scale preparation} —
#'         \code{\link{palette_this}()} with \code{type = "continuous"}
#'         generates a smooth gradient for the \code{score} variable. A
#'         \code{colors_value} sequence is created spanning
#'         \code{min(score)} to \code{quantile(score, 0.99)} for colour
#'         interpolation.
#'   \item \strong{Plot assembly} — the \code{ggplot} object is built with:
#'         \itemize{
#'           \item \code{ggwordcloud::geom_text_wordcloud()} renders the
#'                 words with \code{rm_outside = TRUE}, square shape, and
#'                 eccentricity of 1.
#'           \item \code{scale_color_gradientn()} maps the score to the
#'                 continuous colour gradient, with a colour-bar legend
#'                 (titled by \code{score_name} or \code{"Score"}).
#'           \item \code{scale_size()} maps the count to font size within
#'                 the \code{word_size} range, with a size legend (titled
#'                 by \code{count_name} or \code{"Count"}).
#'           \item \code{coord_flip()} swaps the axes (word cloud
#'                 convention).
#'           \item \code{do_call(theme, theme_args)} applies the selected
#'                 theme; \code{aspect.ratio}, \code{legend.position}, and
#'                 \code{legend.direction} are set directly.
#'         }
#'   \item \strong{Dimension calculation} —
#'         \code{\link{calculate_plot_dimensions}()} computes plot height
#'         and width using \code{base_height = 4.5}, \code{aspect.ratio},
#'         and legend metrics. The resulting \code{height} / \code{width}
#'         attributes are stored on the \code{ggplot} object.
#'   \item \strong{Faceting} — \code{\link{facet_plot}()} wraps the plot
#'         with \code{facet_wrap} or \code{facet_grid} if \code{facet_by}
#'         is provided, up to a maximum of 2 facet columns.
#' }
#'
#' @inheritParams common_args
#' @param word_by A character string specifying the column name containing
#'  pre-tokenized words. A character column is expected. Use this when your
#'  data already has one word per row (or a list of words per row). Mutually
#'  exclusive with \code{sentence_by}.
#' @param sentence_by A character string specifying the column name containing
#'  sentences or phrases to be split into individual words. A character column
#'  is expected. The text is lowercased and punctuation is removed before
#'  splitting on whitespace boundaries. Mutually exclusive with
#'  \code{word_by}.
#' @param count_by A character string specifying the numeric column for the
#'  count or frequency of each word. When \code{NULL} (the default), each
#'  occurrence counts as 1. Must be \code{NULL} when \code{sentence_by} is
#'  used, as counts are derived from the number of occurrences after splitting.
#' @param score_by A character string specifying the numeric column for the
#'  score of each word, mapped to the text colour via a continuous gradient.
#'  When \code{NULL} (the default), all words receive a score of 1 and are
#'  coloured at the low end of the palette.
#' @param count_name A character string for the size legend title. When
#'  \code{NULL} (the default), \code{"Count"} is used.
#' @param score_name A character string for the colour-bar legend title. When
#'  \code{NULL} (the default), \code{"Score"} is used.
#' @param words_excluded A character vector of words to exclude from the word
#'  cloud. Matching is case-insensitive. Defaults to
#'  \code{plotthis::words_excluded}, a built-in set of common English stop
#'  words.
#' @param score_agg A function to aggregate the scores when multiple
#'  observations of the same word exist. Default is \code{mean}. Other
#'  options include \code{sum}, \code{median}, or a custom function.
#' @param minchar A numeric value specifying the minimum number of characters
#'  a word must have to be included. Words with fewer characters are filtered
#'  out. Default: \code{2}.
#' @param word_size A numeric vector of length 2 specifying the range of font
#'  sizes (in mm) for the words. Passed to \code{scale_size(range =
#'  word_size)}. Default: \code{c(2, 8)}.
#' @param top_words A numeric value specifying the maximum number of words to
#'  display, selected by highest score. Default: \code{100}.
#' @return A \code{ggplot} object with \code{height} and \code{width}
#'  attributes (in inches) attached.
#' @keywords internal
#' @importFrom tidyr unnest
#' @importFrom dplyr all_of reframe slice_max distinct mutate group_by summarise n
WordCloudPlotAtomic <- function(
    data,
    word_by = NULL,
    sentence_by = NULL,
    count_by = NULL,
    score_by = NULL,
    count_name = NULL,
    score_name = NULL,
    words_excluded = plotthis::words_excluded,
    score_agg = mean,
    minchar = 2,
    word_size = c(2, 8),
    top_words = 100,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Paired",
    palcolor = NULL,
    alpha = 1,
    palreverse = FALSE,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    seed = 8525,
    ...
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
    facet_by <- check_columns(
        data,
        facet_by,
        force_factor = TRUE,
        allow_multi = TRUE
    )
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
            mutate(
                word = strsplit(
                    tolower(gsub(
                        "[[:punct:]]",
                        "",
                        as.character(!!sym(sentence_by))
                    )),
                    "\\b\\s+\\b"
                )
            ) %>%
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
        data <- data %>% unnest(cols = all_of(word_by))

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
        mutate(
            angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))
        ) %>%
        as.data.frame()

    colors <- palette_this(
        data$score,
        type = "continuous",
        palette = palette,
        palcolor = palcolor,
        matched = FALSE,
        reverse = palreverse
    )
    colors_value <- seq(
        min(data$score, na.rm = TRUE),
        quantile(data$score, 0.99, na.rm = TRUE) + 0.001,
        length.out = 100
    )
    p <- ggplot(
        data,
        aes(
            label = !!sym("word"),
            size = !!sym("count"),
            color = !!sym("score"),
            angle = !!sym("angle")
        )
    ) +
        ggwordcloud::geom_text_wordcloud(
            rm_outside = TRUE,
            eccentricity = 1,
            shape = "square",
            show.legend = TRUE,
            grid_margin = 3
        ) +
        scale_color_gradientn(
            name = score_name %||% "Score",
            colours = colors,
            values = scales::rescale(colors_value),
            guide = guide_colorbar(
                frame.colour = "black",
                ticks.colour = "black",
                title.hjust = 0
            )
        ) +
        scale_size(
            name = count_name %||% "Count",
            range = word_size,
            breaks = ceiling(seq(
                min(data$count, na.rm = TRUE),
                max(data$count, na.rm = TRUE),
                length.out = 3
            ))
        ) +
        guides(
            size = guide_legend(
                override.aes = list(colour = "black", label = "G"),
                order = 1
            )
        ) +
        labs(title = title, subtitle = subtitle) +
        coord_flip() +
        do_call(theme, theme_args) +
        ggplot2::theme(
            aspect.ratio = aspect.ratio,
            legend.position = legend.position,
            legend.direction = legend.direction
        )

    dims <- calculate_plot_dimensions(
        base_height = 4.5,
        aspect.ratio = aspect.ratio,
        legend.position = legend.position,
        legend.direction = legend.direction
    )

    attr(p, "height") <- dims$height
    attr(p, "width") <- dims$width

    facet_plot(p, facet_by, facet_scales, facet_nrow, facet_ncol, facet_byrow)
}

#' Word cloud plot
#'
#' @description
#' Draws a word cloud plot that visualises word frequency and importance.
#' Words are displayed with font size proportional to a count variable and
#' colour based on a continuous score variable, using
#' \code{\link[ggwordcloud]{geom_text_wordcloud}} for rendering.
#'
#' The function supports \strong{pre-tokenised words} (via \code{word_by})
#' and \strong{sentence splitting} (via \code{sentence_by}). Sentences are
#' automatically lowercased, stripped of punctuation, and split into
#' individual words before aggregation. Common stop words can be excluded
#' via \code{words_excluded}, and the number of displayed words is
#' controlled by \code{top_words}.
#'
#' The word cloud can be \strong{faceted} (via \code{facet_by}) or
#' \strong{split} into separate sub-plots via \code{split_by}. When
#' \code{split_by} is used, each split level receives its own word cloud,
#' and the results are combined into a single layout via
#' \code{\link{combine_plots}}.
#'
#' @section split_by workflow:
#' When \code{split_by} is provided:
#' \enumerate{
#'   \item \code{\link{validate_common_args}()} validates the \code{seed}
#'         and \code{facet_by} constraints (maximum 2 facet columns).
#'   \item The \code{theme} argument is resolved via
#'         \code{\link{process_theme}()}.
#'   \item The \code{split_by} column(s) are validated and transformed via
#'         \code{\link{check_columns}()} with \code{force_factor = TRUE}
#'         and \code{concat_multi = TRUE}.
#'   \item The data frame is split by \code{split_by} (preserving factor
#'         level order). If \code{split_by} is \code{NULL}, the data is
#'         wrapped in a single-element list with name \code{"..."}.
#'   \item Per-split \code{palette}, \code{palcolor},
#'         \code{legend.position}, and \code{legend.direction} are resolved
#'         via \code{\link{check_palette}()}, \code{\link{check_palcolor}()},
#'         and \code{\link{check_legend}()}.
#'   \item \code{\link{WordCloudPlotAtomic}()} is called for each split. If
#'         \code{title} is a function, it receives the split level name and
#'         can generate dynamic titles per sub-plot.
#'   \item Results are combined via \code{\link{combine_plots}()} (when
#'         \code{combine = TRUE}) or returned as a named list (when
#'         \code{combine = FALSE}).
#' }
#'
#' @inheritParams common_args
#' @inheritParams WordCloudPlotAtomic
#' @return A \code{ggplot} object (when no \code{split_by} is used), a
#'  \code{patchwork} object (when \code{combine = TRUE} and
#'  \code{split_by} is used), or a named list of \code{ggplot} objects
#'  (when \code{combine = FALSE}), each with \code{height} and
#'  \code{width} attributes in inches.
#' @export
#' @examples
#' \donttest{
#' set.seed(8525)
#' data <- data.frame(
#'     word = c("apple", "banana", "cherry", "date", "elderberry",
#'              "fig", "grape", "honeydew", "kiwi", "lemon"),
#'     count = c(10, 20, 30, 40, 50, 15, 25, 35, 45, 55),
#'     score = c(1, 2, 3, 4, 5, 1.5, 2.5, 3.5, 4.5, 5.5),
#'     facet = rep(c("Group1", "Group2"), each = 5),
#'     split = rep(c("A", "B"), 5)
#' )
#'
#' # Basic word cloud with word, count, and score columns
#' WordCloudPlot(data, word_by = "word",
#'               count_by = "count", score_by = "score")
#'
#' # Word cloud using sentence_by (sentences split into words)
#' data_sent <- data.frame(
#'     sentence = c("The quick brown fox jumps over the lazy dog",
#'                  "A quick brown dog jumps over a lazy fox"),
#'     score = c(10, 5)
#' )
#' WordCloudPlot(data_sent, sentence_by = "sentence", score_by = "score")
#'
#' # Word cloud with faceting
#' WordCloudPlot(data, word_by = "word",
#'               count_by = "count", score_by = "score",
#'               facet_by = "facet")
#'
#' # Word cloud split by a grouping variable
#' WordCloudPlot(data, word_by = "word",
#'               count_by = "count", score_by = "score",
#'               split_by = "split")
#' }
WordCloudPlot <- function(
    data,
    word_by = NULL,
    sentence_by = NULL,
    count_by = NULL,
    score_by = NULL,
    count_name = NULL,
    score_name = NULL,
    split_by = NULL,
    split_by_sep = "_",
    words_excluded = plotthis::words_excluded,
    score_agg = mean,
    minchar = 2,
    word_size = c(2, 8),
    top_words = 100,
    facet_by = NULL,
    facet_scales = "fixed",
    facet_ncol = NULL,
    facet_nrow = NULL,
    facet_byrow = TRUE,
    theme = "theme_this",
    theme_args = list(),
    palette = "Spectral",
    palcolor = NULL,
    alpha = 1,
    palreverse = FALSE,
    aspect.ratio = 1,
    legend.position = "right",
    legend.direction = "vertical",
    title = NULL,
    subtitle = NULL,
    seed = 8525,
    combine = TRUE,
    nrow = NULL,
    ncol = NULL,
    byrow = TRUE,
    axes = NULL,
    axis_titles = axes,
    guides = NULL,
    design = NULL,
    ...
) {
    validate_common_args(seed, facet_by = facet_by)
    theme <- process_theme(theme)
    split_by <- check_columns(
        data,
        split_by,
        force_factor = TRUE,
        allow_multi = TRUE,
        concat_multi = TRUE,
        concat_sep = split_by_sep
    )

    if (!is.null(split_by)) {
        data[[split_by]] <- droplevels(data[[split_by]])
        datas <- split(data, data[[split_by]])
        # keep the order of levels
        datas <- datas[levels(data[[split_by]])]
    } else {
        datas <- list(data)
        split_by <- names(datas) <- "..."
    }
    palette <- check_palette(palette, names(datas))
    palcolor <- check_palcolor(palcolor, names(datas))
    legend.direction <- check_legend(
        legend.direction,
        names(datas),
        "legend.direction"
    )
    legend.position <- check_legend(
        legend.position,
        names(datas),
        "legend.position"
    )

    plots <- lapply(
        names(datas),
        function(nm) {
            default_title <- if (length(datas) == 1 && identical(nm, "...")) {
                NULL
            } else {
                nm
            }
            if (is.function(title)) {
                title <- title(default_title)
            } else {
                title <- title %||% default_title
            }
            WordCloudPlotAtomic(
                datas[[nm]],
                word_by = word_by,
                sentence_by = sentence_by,
                count_by = count_by,
                score_by = score_by,
                count_name = count_name,
                score_name = score_name,
                words_excluded = words_excluded,
                score_agg = score_agg,
                minchar = minchar,
                word_size = word_size,
                top_words = top_words,
                facet_by = facet_by,
                facet_scales = facet_scales,
                facet_ncol = facet_ncol,
                facet_nrow = facet_nrow,
                facet_byrow = facet_byrow,
                theme = theme,
                theme_args = theme_args,
                palette = palette[[nm]],
                palcolor = palcolor[[nm]],
                alpha = alpha,
                palreverse = palreverse,
                aspect.ratio = aspect.ratio,
                legend.position = legend.position[[nm]],
                legend.direction = legend.direction[[nm]],
                title = title,
                subtitle = subtitle,
                seed = seed,
                ...
            )
        }
    )

    names(plots) <- names(datas)

    combine_plots(
        plots,
        combine = combine,
        split_by = split_by,
        nrow = nrow,
        ncol = ncol,
        byrow = byrow,
        axes = axes,
        axis_titles = axis_titles,
        guides = guides,
        design = design
    )
}

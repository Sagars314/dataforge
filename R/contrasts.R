#' Add a contrast coding column to a data frame
#'
#' Adds one or more contrast-coded numeric columns for a factor, based on a
#' chosen coding scheme.
#'
#' @param data A data frame.
#' @param col Character. Name of the factor column to contrast-code.
#' @param contrasts Character. One of `"treatment"`, `"sum"`, `"helmert"`,
#'   `"difference"`, `"poly"`, `"anova"`. Default `"treatment"`.
#' @param base Integer or character. Base/reference level (used by treatment
#'   coding). Default `1`.
#' @param labels Character vector of new column name(s). Auto-generated if
#'   `NULL`.
#' @param remove_intercept Logical. Remove the intercept column? (default
#'   `FALSE`)
#' @return The input data frame with additional numeric contrast column(s).
#' @export
#' @examples
#' d <- data.frame(group = factor(c("A","B","C","A","B","C")),
#'                 y = rnorm(6))
#' add_contrast(d, "group", contrasts = "treatment")
#' add_contrast(d, "group", contrasts = "sum")
add_contrast <- function(data, col, contrasts = "treatment",
                         base = 1, labels = NULL,
                         remove_intercept = FALSE) {
  f <- data[[col]]
  if (!is.factor(f)) f <- factor(f)
  levs <- levels(f)
  k    <- length(levs)

  cm <- switch(contrasts,
    treatment  = contr_code_treatment(levs, base),
    sum        = contr_code_sum(levs, base),
    helmert    = contr_code_helmert(levs),
    difference = contr_code_difference(levs),
    poly       = contr_code_poly(levs),
    anova      = contr_code_anova(levs, base),
    stop("Unknown contrast scheme: ", contrasts, call. = FALSE)
  )

  if (is.null(labels)) labels <- colnames(cm)
  coded <- model.matrix(~ f - 1, data = data.frame(f = f)) %*% cm
  colnames(coded) <- labels

  cbind(data, as.data.frame(coded))
}

# ---- Contrast matrix constructors ------------------------------------------

#' Treatment (dummy) coding
#' @param levels Character vector of factor levels.
#' @param base Reference level (integer index or level name). Default `1`.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_treatment(c("A","B","C"))
contr_code_treatment <- function(levels, base = 1) {
  k   <- length(levels)
  if (is.character(base)) base <- match(base, levels)
  cm  <- diag(k)[-base, , drop = FALSE]
  cm  <- t(cm)
  colnames(cm) <- paste0(levels[-base], ".vs.", levels[base])
  rownames(cm) <- levels
  cm
}

#' Sum (deviation) coding
#' @param levels Character vector of factor levels.
#' @param base Reference level. Default `length(levels)`.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_sum(c("A","B","C"))
contr_code_sum <- function(levels, base = length(levels)) {
  k  <- length(levels)
  if (is.character(base)) base <- match(base, levels)
  cm <- contr.sum(k)
  if (base != k) {
    ord <- c(setdiff(seq_len(k), base), base)
    cm  <- cm[ord, , drop = FALSE]
  }
  rownames(cm) <- levels
  colnames(cm) <- paste0(levels[-base], ".sum")
  cm
}

#' Helmert coding
#' @param levels Character vector of factor levels.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_helmert(c("A","B","C","D"))
contr_code_helmert <- function(levels) {
  k  <- length(levels)
  cm <- contr.helmert(k)
  # Normalise so coefficients compare each level to mean of previous
  for (j in seq_len(ncol(cm))) {
    cm[, j] <- cm[, j] / (j + 1)
  }
  rownames(cm) <- levels
  colnames(cm) <- paste0(levels[-1], ".vs.prev")
  cm
}

#' Difference (successive) coding
#' @param levels Character vector of factor levels.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_difference(c("A","B","C","D"))
contr_code_difference <- function(levels) {
  k  <- length(levels)
  cm <- matrix(0, k, k - 1)
  for (j in seq_len(k - 1)) {
    cm[seq_len(j), j]       <- -1 / k
    cm[seq(j+1, k), j]      <-  (k - j) / k / j
  }
  # Use simpler successive differences
  cm <- matrix(0, k, k - 1)
  for (j in seq_len(k - 1)) {
    cm[1:j, j]       <- -1
    cm[(j+1):k, j]   <-  j / (k - j)
  }
  # Cleaner: each column contrasts level j+1 vs level j
  cm <- matrix(0, k, k - 1)
  for (j in seq_len(k - 1)) {
    cm[j, j]     <- -1
    cm[j + 1, j] <-  1
  }
  rownames(cm) <- levels
  colnames(cm) <- paste0(levels[-1], ".vs.", levels[-k])
  cm
}

#' ANOVA (deviation from grand mean) coding
#' @param levels Character vector of factor levels.
#' @param base Reference level (omitted). Default `length(levels)`.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_anova(c("A","B","C"))
contr_code_anova <- function(levels, base = length(levels)) {
  contr_code_sum(levels, base = base)
}

#' Polynomial (orthogonal) coding
#' @param levels Character vector of factor levels.
#' @return A contrast matrix.
#' @export
#' @examples
#' contr_code_poly(c("low","med","high","vhigh"))
contr_code_poly <- function(levels) {
  k  <- length(levels)
  cm <- contr.poly(k)
  rownames(cm) <- levels
  colnames(cm) <- paste0(".L", seq_len(k - 1))
  # Rename to conventional labels
  poly_nms <- c(".L", ".Q", ".C", paste0("^", seq(4, k - 1)))
  colnames(cm) <- poly_nms[seq_len(k - 1)]
  cm
}

#' Add Contrast Codes to a Data Frame
#'
#' @description
#' Generate and attach contrast codes for a factor column. Supports treatment,
#' sum, Helmert, polynomial, and difference coding — matching the schemes
#' used in `faux::add_contrast()`.
#'
#' @param data A data frame.
#' @param col Character. Name of the factor column to code.
#' @param contrast Character. One of `"treatment"`, `"sum"`, `"anova"`,
#'   `"helmert"`, `"poly"`, `"difference"`.
#' @param reference Integer. Index of the reference level (treatment coding
#'   only). Default 1.
#' @param .add_cols Logical. If `TRUE` (default), add contrast columns to
#'   `data`. If `FALSE`, return the contrast matrix only.
#'
#' @return The data frame with contrast columns added, or the contrast matrix
#'   if `.add_cols = FALSE`.
#'
#' @examples
#' df <- data.frame(group = factor(rep(c("A","B","C"), each = 10)))
#' add_contrast(df, "group", contrast = "helmert")
#'
#' @export
add_contrast <- function(data, col, contrast = "treatment",
                         reference = 1, .add_cols = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (!(col %in% names(data))) stop(sprintf("Column '%s' not found.", col), call. = FALSE)

  f   <- factor(data[[col]])
  lvs <- levels(f)
  k   <- length(lvs)

  cm <- switch(contrast,
    treatment  = contr_code_treatment(k, reference),
    anova      = ,
    sum        = contr_code_sum(k),
    helmert    = contr_code_helmert(k),
    poly       = contr_code_poly(k),
    difference = contr_code_difference(k),
    stop(sprintf("Unknown contrast type '%s'.", contrast), call. = FALSE)
  )
  rownames(cm) <- lvs

  if (!.add_cols) return(cm)

  # Attach contrast columns named col.level (dropping reference for treatment)
  col_nms <- colnames(cm)
  for (cn in col_nms) {
    data[[paste0(col, ".", cn)]] <- cm[as.character(f), cn]
  }
  data
}

#' Treatment Contrast Matrix
#' @param k Integer. Number of levels.
#' @param reference Integer. Reference level index.
#' @return A `k x (k-1)` matrix.
#' @export
contr_code_treatment <- function(k, reference = 1) {
  cm <- stats::contr.treatment(k, base = reference)
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  cm
}

#' Sum (Deviation) Contrast Matrix
#' @param k Integer. Number of levels.
#' @return A `k x (k-1)` matrix.
#' @export
contr_code_sum <- function(k) {
  cm <- stats::contr.sum(k)
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  cm
}

#' ANOVA (sum) Contrast Matrix — alias for contr_code_sum
#' @inheritParams contr_code_sum
#' @export
contr_code_anova <- contr_code_sum

#' Helmert Contrast Matrix
#' @param k Integer. Number of levels.
#' @return A `k x (k-1)` matrix.
#' @export
contr_code_helmert <- function(k) {
  cm <- stats::contr.helmert(k)
  # Scale to [-1, 1] range
  cm <- apply(cm, 2, function(col) col / max(abs(col)))
  colnames(cm) <- paste0("c", seq_len(ncol(cm)))
  cm
}

#' Polynomial Contrast Matrix
#' @param k Integer. Number of levels.
#' @return A `k x (k-1)` matrix.
#' @export
contr_code_poly <- function(k) {
  cm <- stats::contr.poly(k)
  colnames(cm) <- c(".L", ".Q", ".C", paste0("^", 4:k))[seq_len(k - 1)]
  cm
}

#' Difference (Sequential) Contrast Matrix
#'
#' @description
#' Each column contrasts adjacent level pairs: level i+1 vs level i.
#'
#' @param k Integer. Number of levels.
#' @return A `k x (k-1)` matrix.
#' @export
contr_code_difference <- function(k) {
  cm <- matrix(0, nrow = k, ncol = k - 1)
  for (j in seq_len(k - 1)) {
    cm[j, j]       <- -1
    cm[j + 1, j]   <-  1
  }
  colnames(cm) <- paste0("c", seq_len(k - 1))
  cm
}

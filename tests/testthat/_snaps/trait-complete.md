# complete() description and diagnosis are as expected

    Code
      obj_inspect_type(c(1L, 2L, 3L), t)
    Output
      Object `c(1L, 2L, 3L)` has the expected type.
      v `c(1L, 2L, 3L)` is a vctrs style vector.
      v `c(1L, 2L, 3L)` contains no missing values.

---

    Code
      obj_inspect_type(c(1L, NA_integer_, 3L, NA_integer_), t)
    Output
      Object `c(1L, NA_integer_, 3L, NA_integer_)` does not have the expected type.
      v `c(1L, NA_integer_, 3L, NA_integer_)` is a vctrs style vector.
      i `c(1L, NA_integer_, 3L, NA_integer_)` must not contain missing elements.
      x `c(1L, NA_integer_, 3L, NA_integer_)` is NA at locations `c(2, 4)`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.


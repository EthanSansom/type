# unduplicated() description and diagnosis are as expected

    Code
      obj_inspect_type(c(1L, 2L, 3L), t)
    Output
      Object `c(1L, 2L, 3L)` has the expected type.
      v `c(1L, 2L, 3L)` is a vctrs style vector.
      v `c(1L, 2L, 3L)` contains no duplicated values.

---

    Code
      obj_inspect_type(c(1L, 1L, 2L, 3L, 3L), t)
    Output
      Object `c(1L, 1L, 2L, 3L, 3L)` does not have the expected type.
      v `c(1L, 1L, 2L, 3L, 3L)` is a vctrs style vector.
      i `c(1L, 1L, 2L, 3L, 3L)` must not contain duplicate elements.
      x `c(1L, 1L, 2L, 3L, 3L)` contains duplicate elements at locations `c(1, 2, 4,
        5)`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.


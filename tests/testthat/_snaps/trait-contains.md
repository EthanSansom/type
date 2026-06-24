# contains() description and diagnosis are as expected

    Code
      obj_inspect_type(c("a", "b", "c", "d"), t)
    Output
      Object `c("a", "b", "c", "d")` has the expected type.
      v `c("a", "b", "c", "d")` is a vctrs style vector.
      v `c("a", "b", "c", "d")` contains elements: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(c("a", "c"), t)
    Output
      Object `c("a", "c")` does not have the expected type.
      v `c("a", "c")` is a vctrs style vector.
      i `c("a", "c")` must contain elements: `c("a", "b", "c")`.
      x `c("a", "c")` is missing 1 element: `"b"`.

---

    Code
      obj_inspect_type(character(), t)
    Output
      Object `character()` does not have the expected type.
      v `character()` is a vctrs style vector.
      i `character()` must contain elements: `c("a", "b", "c")`.
      x `character()` is missing 3 elements: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.


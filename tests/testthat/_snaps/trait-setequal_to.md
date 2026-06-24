# setequal_to() description and diagnosis are as expected

    Code
      obj_inspect_type(c("c", "b", "a"), t)
    Output
      Object `c("c", "b", "a")` has the expected type.
      v `c("c", "b", "a")` is a vctrs style vector.
      v `c("c", "b", "a")` is setequal to: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(c("a", "b"), t)
    Output
      Object `c("a", "b")` does not have the expected type.
      v `c("a", "b")` is a vctrs style vector.
      i `c("a", "b")` must contain exactly: `c("a", "b", "c")`.
      x `c("a", "b")` is missing 1 element: `"c"`.

---

    Code
      obj_inspect_type(c("a", "b", "c", "d"), t)
    Output
      Object `c("a", "b", "c", "d")` does not have the expected type.
      v `c("a", "b", "c", "d")` is a vctrs style vector.
      i `c("a", "b", "c", "d")` must contain exactly: `c("a", "b", "c")`.
      x `c("a", "b", "c", "d")` contains 1 unexpected element: `"d"`.

---

    Code
      obj_inspect_type(c("a", "b", "d"), t)
    Output
      Object `c("a", "b", "d")` does not have the expected type.
      v `c("a", "b", "d")` is a vctrs style vector.
      i `c("a", "b", "d")` must contain exactly: `c("a", "b", "c")`.
      x `c("a", "b", "d")` is missing 1 element: `"c"`.
      x `c("a", "b", "d")` contains 1 unexpected element: `"d"`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.


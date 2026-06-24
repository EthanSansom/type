# disjoint_to() description and diagnosis are as expected

    Code
      obj_inspect_type(c("d", "e"), t)
    Output
      Object `c("d", "e")` has the expected type.
      v `c("d", "e")` is a vctrs style vector.
      v `c("d", "e")` contains none of: `c("a", "b", "c")`.

---

    Code
      obj_inspect_type(c("a"), t)
    Output
      Object `c("a")` does not have the expected type.
      v `c("a")` is a vctrs style vector.
      i `c("a")` must not contain any of: `c("a", "b", "c")`.
      x `c("a")` contains 1 unexpected element: `"a"`.

---

    Code
      obj_inspect_type(c("b", "c", "d"), t)
    Output
      Object `c("b", "c", "d")` does not have the expected type.
      v `c("b", "c", "d")` is a vctrs style vector.
      i `c("b", "c", "d")` must not contain any of: `c("a", "b", "c")`.
      x `c("b", "c", "d")` contains 2 unexpected elements: `c("b", "c")`.

---

    Code
      obj_inspect_type(mean, t)
    Output
      Object `mean` does not have the expected type.
      x `mean` must be a vctrs style vector, not a function.


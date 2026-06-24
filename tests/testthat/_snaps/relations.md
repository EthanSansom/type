# exclusive() description and diagnosis are as expected

    Code
      obj_inspect_type(list(x = 1L, y = NULL), t)
    Output
      Object `list(x = 1L, y = NULL)` has the expected type.
      v Exactly one of `list(x = 1L, y = NULL)[["x"]]` and `list(x = 1L, y =
        NULL)[["y"]]` are non-NULL.

---

    Code
      obj_inspect_type(list(x = NULL, y = NULL), t)
    Output
      Object `list(x = NULL, y = NULL)` does not have the expected type.
      i Exactly one of `list(x = NULL, y = NULL)[["x"]]` and `list(x = NULL, y =
        NULL)[["y"]]` must be non-NULL.
      x Every element is NULL.

---

    Code
      obj_inspect_type(list(x = 1L, y = 1L), t)
    Output
      Object `list(x = 1L, y = 1L)` does not have the expected type.
      i Exactly one of `list(x = 1L, y = 1L)[["x"]]` and `list(x = 1L, y =
        1L)[["y"]]` must be non-NULL.
      x `list(x = 1L, y = 1L)[["x"]]` and `list(x = 1L, y = 1L)[["y"]]` are all
        non-NULL.


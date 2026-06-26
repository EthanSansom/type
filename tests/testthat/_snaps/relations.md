# recyclable() description and diagnosis are as expected

    Code
      obj_inspect_type(list(x = 1L, y = 1:3), t)
    Output
      Object `list(x = 1L, y = 1:3)` has the expected type.
      v `list(x = 1L, y = 1:3)[["x"]]` and `list(x = 1L, y = 1:3)[["y"]]` are
        recyclable.

---

    Code
      obj_inspect_type(list(x = 1:2, y = 1:3), t)
    Output
      Object `list(x = 1:2, y = 1:3)` does not have the expected type.
      i `list(x = 1:2, y = 1:3)[["x"]]` and `list(x = 1:2, y = 1:3)[["y"]]` must be
        recyclable.
      x `list(x = 1:2, y = 1:3)[["x"]]` (size 2) and `list(x = 1:2, y = 1:3)[["y"]]`
        (size 3) have incompatible sizes.

---

    Code
      obj_inspect_type(list(x = mean, y = 1:3), t)
    Output
      Object `list(x = mean, y = 1:3)` does not have the expected type.
      x `list(x = mean, y = 1:3)[["x"]]` must be a vector, not a function.

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


# bounded() description and diagnosis are as expected

    Code
      obj_inspect_type(5L, t_closed)
    Output
      Object `5L` has the expected type.
      v `5L` is a vctrs style vector.
      v `5L` is bounded by [0, 10].

---

    Code
      obj_inspect_type(-1L, t_closed)
    Output
      Object `-1L` does not have the expected type.
      v `-1L` is a vctrs style vector.
      i `-1L` must be bounded by [0, 10].
      x `-1L` is out of bounds at location `1`.

---

    Code
      obj_inspect_type(11L, t_closed)
    Output
      Object `11L` does not have the expected type.
      v `11L` is a vctrs style vector.
      i `11L` must be bounded by [0, 10].
      x `11L` is out of bounds at location `1`.

---

    Code
      obj_inspect_type(10L, t_half_open)
    Output
      Object `10L` does not have the expected type.
      v `10L` is a vctrs style vector.
      i `10L` must be bounded by [0, 10).
      x `10L` is out of bounds at location `1`.

---

    Code
      obj_inspect_type(0L, t_open)
    Output
      Object `0L` does not have the expected type.
      v `0L` is a vctrs style vector.
      i `0L` must be bounded by (0, 10).
      x `0L` is out of bounds at location `1`.

---

    Code
      obj_inspect_type(-c(10:15), t_left)
    Output
      Object `-c(10:15)` does not have the expected type.
      v `-c(10:15)` is a vctrs style vector.
      i `-c(10:15)` must be equal to or above 0.
      x `-c(10:15)` is out of bounds at locations `c(1, 2, 3, 4, ..., 6)` and 1 more.

---

    Code
      obj_inspect_type(11:100, t_right)
    Output
      Object `11:100` does not have the expected type.
      v `11:100` is a vctrs style vector.
      i `11:100` must be equal to or below 10.
      x `11:100` is out of bounds at locations `c(1, 2, 3, 4, ..., 90)` and 85 more.


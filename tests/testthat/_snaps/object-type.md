# obj_inspect_type() works as expected

    Code
      obj_inspect_type(1L, t_int)
    Output
      Object `1L` has the expected type.
      v `1L` is a bare <integer>.

---

    Code
      obj_inspect_type("a", t_int)
    Output
      Object `"a"` does not have the expected type.
      x `"a"` must be a bare <integer>, not a bare <character>.

---

    Code
      obj_inspect_type(1L, sized(t_int, 2L))
    Output
      Object `1L` does not have the expected type.
      v `1L` is a bare <integer>.
      x `1L` must be size 2, not size 1.


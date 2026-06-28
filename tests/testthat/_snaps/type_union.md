# type_union() description and diagnosis are as expected

    Code
      obj_inspect_type(1L, t)
    Output
      Object `1L` has the expected type.
      v `1L` is a bare <integer>.

---

    Code
      obj_inspect_type("a", t)
    Output
      Object `"a"` has the expected type.
      v `"a"` is a bare <character>.
      v `"a"` is size 1.

---

    Code
      obj_inspect_type(1.5, t)
    Output
      Object `1.5` does not have the expected type.
      * Type option 1 of 2:
      x `1.5` must be a bare <integer>, not a bare <double>.
      * Type option 2 of 2:
      x `1.5` must be a bare <character>, not a bare <double>.


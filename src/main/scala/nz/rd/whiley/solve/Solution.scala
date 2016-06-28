package nz.rd.whiley.solve

final case class Solution[+C,+A](context: C, value: A)

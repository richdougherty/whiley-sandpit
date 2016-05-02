package nz.rd.whiley

sealed trait Kind
final case object KNull extends Kind
final case object KInt extends Kind
final case object KUnion extends Kind
final case object KFunction extends Kind
final case object KIntersection extends Kind
final case object KTuple extends Kind
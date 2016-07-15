package nz.rd.whiley

import org.scalatest._

class DNFSpec extends FreeSpec with Matchers {

//  import DNF.Implicits._

  "Terms" - {
    "should support negation" in {
      !DNF.Term(true, DNF.RootVal, DNF.Kind.Int) should be(DNF.Term(false, DNF.RootVal, DNF.Kind.Int))
    }
  }

  val rootIsNull = DNF.Term.isKind(DNF.RootVal, DNF.Kind.Null)
  val rootIsInt = DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int)
  val rootIsProduct = DNF.Term.isKind(DNF.RootVal, DNF.Kind.Product(2))
  val root0IsInt = DNF.Term.isKind(DNF.RootVal(0), DNF.Kind.Int)
  val root1IsNull = DNF.Term.isKind(DNF.RootVal(1), DNF.Kind.Null)

  "Conjunction forms" - {
    "constants" - {
      "should have valid constant for conj(true)" in {
        DNF.Conj.True should be(new DNF.Conj(TTrue, Set.empty))
      }
      "should have valid constant for conj(unknown)" in {
        DNF.Conj.Unknown should be(new DNF.Conj(TUnknown, Set.empty))
      }
      "should have valid constant for conj(false)" in {
        DNF.Conj.False should be(new DNF.Conj(TFalse, Set.empty))
      }
      "empty constant should be conj(true)" in {
        DNF.Conj.Empty should be(DNF.Conj.True)
      }
    }

    "conjunction (&) operator" - {

      "truth table identities" - {
        "with simple ternary values" - {
          "conj(true) & true --> conj(true)" in {
            (DNF.Conj.True & TTrue) should be(DNF.Conj.True)
          }
          "conj(true) & unknown --> conj(unknown)" in {
            (DNF.Conj.True & TUnknown) should be(DNF.Conj.Unknown)
          }
          "conj(true) & false --> conj(false)" in {
            (DNF.Conj.True & TFalse) should be(DNF.Conj.False)
          }
          "conj(unknown) & true --> conj(unknown)" in {
            (DNF.Conj.Unknown & TTrue) should be(DNF.Conj.Unknown)
          }
          "conj(unknown) & unknown --> conj(unknown)" in {
            (DNF.Conj.Unknown & TUnknown) should be(DNF.Conj.Unknown)
          }
          "conj(unknown) & false --> conj(false)" in {
            (DNF.Conj.Unknown & TFalse) should be(DNF.Conj.False)
          }
          "conj(false) & true --> conj(false)" in {
            (DNF.Conj.False & TTrue) should be(DNF.Conj.False)
          }
          "conj(false) & unknown --> conj(false)" in {
            (DNF.Conj.False & TUnknown) should be(DNF.Conj.False)
          }
          "conj(false) & false --> conj(false)" in {
            (DNF.Conj.False & TFalse) should be(DNF.Conj.False)
          }
        }
        "with ternary values in other conjunctions" - {
          "conj(true) & conj(true) --> conj(true)" in {
            (DNF.Conj.True & DNF.Conj.True) should be(DNF.Conj.True)
          }
          "conj(true) & conj(unknown) --> conj(unknown)" in {
            (DNF.Conj.True & DNF.Conj.Unknown) should be(DNF.Conj.Unknown)
          }
          "conj(true) & conj(false) --> conj(false)" in {
            (DNF.Conj.True & DNF.Conj.False) should be(DNF.Conj.False)
          }
          "conj(unknown) & conj(true) --> conj(unknown)" in {
            (DNF.Conj.Unknown & DNF.Conj.True) should be(DNF.Conj.Unknown)
          }
          "conj(unknown) & conj(unknown) --> conj(unknown)" in {
            (DNF.Conj.Unknown & DNF.Conj.Unknown) should be(DNF.Conj.Unknown)
          }
          "conj(unknown) & conj(false) --> conj(false)" in {
            (DNF.Conj.Unknown & DNF.Conj.False) should be(DNF.Conj.False)
          }
          "conj(false) & conj(true) --> conj(false)" in {
            (DNF.Conj.False & DNF.Conj.True) should be(DNF.Conj.False)
          }
          "conj(false) & conj(unknown) --> conj(false)" in {
            (DNF.Conj.False & DNF.Conj.Unknown) should be(DNF.Conj.False)
          }
          "conj(false) & conj(false) --> conj(false)" in {
            (DNF.Conj.False & DNF.Conj.False) should be(DNF.Conj.False)
          }
        }
      }

      "single terms" - {
        "conj(true) & root:Int --> conj(true & root:Int)" in {
          (DNF.Conj.True & rootIsInt) should be(new DNF.Conj(TTrue, Set(rootIsInt)))
        }
        "conj(unknown) & root:Int --> conj(unknown & root:Int)" in {
          (DNF.Conj.Unknown & rootIsInt) should be(new DNF.Conj(TUnknown, Set(rootIsInt)))
        }
        "conj(false) & root:Int --> conj(false)" in {
          (DNF.Conj.False & rootIsInt) should be(DNF.Conj.False)
        }
      }

      "equality and inequality contradict" - {
        "root values" - {
          "conj(true & root:Int) & !root:Int --> conj(false)" in {
            val t = DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int)
            val c = DNF.Conj.Empty & t
            c & !t should be(DNF.Conj.False)
          }
          "conj(true & !root:Int) & root:Int --> conj(false)" in {
            val t = !DNF.Term.isKind(DNF.RootVal, DNF.Kind.Int)
            val c = DNF.Conj.Empty & t
            c & !t should be(DNF.Conj.False)
          }
        }
        "indexed values" - {
          "conj(true & root[0]:Int) & !root[0]:Int --> conj(false)" in {
            (DNF.Conj.Empty & root0IsInt) & !root0IsInt should be(DNF.Conj.False)
          }
          "conj(true & !root[0]:Int) & root[0]:Int --> conj(false)" in {
            (DNF.Conj.Empty & !root0IsInt) & root0IsInt should be(DNF.Conj.False)
          }
        }
      }

      "equality and inequality to different kinds" - {
        "conj(true & root:Null) & !root:Int --> conj(true & root:Null)" in {
          (DNF.Conj.Empty & rootIsNull) & !rootIsInt should be(DNF.Conj(rootIsNull))
        }
      }

      "equality to different kinds contradict" - {
        "conj(true & root:Null) & root:Int --> conj(false)" in {
          (DNF.Conj.Empty & rootIsNull) & rootIsInt should be(DNF.Conj.False)
        }
      }

      "supports multiple terms" - {
        "conj(true) & unknown & root:Product[2] & root[0]:Int & root[1]:Null --> conj(unknown & root:Product[2] & root[0]:Int & root[1]:Null)" in {
          (DNF.Conj.Empty & TUnknown & rootIsProduct & root0IsInt & root1IsNull) should be(
            new DNF.Conj(TUnknown, Set(rootIsProduct, root0IsInt, root1IsNull)))
        }

        "conj(true) & root:Product[1] & root[0]:Int & !root[0]:Int --> conj(unknown & root:Product[2] & root[0]:Int & root[1]:Null)" in {
          (DNF.Conj.Empty & TUnknown & rootIsProduct & root0IsInt & root1IsNull) should be(
            new DNF.Conj(TUnknown, Set(rootIsProduct, root0IsInt, root1IsNull)))
        }
      }
    }

    "negation (!) operator" - {
      "truth table" - {
        "!conj(true) --> disj(false)" in {
          !DNF.Conj.True should be (DNF.Disj.False)
        }
        "!conj(unknown) --> disj(unknown)" in {
          !DNF.Conj.Unknown should be (DNF.Disj.Unknown)
        }
        "!conj(false) --> disj(true)" in {
          !DNF.Conj.False should be (DNF.Disj.True)
        }
      }
      "single terms" - {
        "!conj(true & root:Int) --> disj(false | conj(true & !root:int))" in {
          !(DNF.Conj.True & rootIsInt) should be(new DNF.Disj(TFalse, Set(DNF.Conj(!rootIsInt))))
        }
        "!conj(unknown & root:Int) --> disj(unknown | conj(true & !root:Int)" in {
          !(DNF.Conj.Unknown & rootIsInt) should be(new DNF.Disj(TUnknown, Set(DNF.Conj(!rootIsInt))))
        }
      }

    }
  }

  "Disjunction forms" - {
    "constants" - {
      "should have valid constant for disj(true)" in {
        DNF.Disj.True should be(new DNF.Disj(TTrue, Set.empty))
      }
      "should have valid constant for disj(unknown)" in {
        DNF.Disj.Unknown should be(new DNF.Disj(TUnknown, Set.empty))
      }
      "should have valid constant for disj(false)" in {
        DNF.Disj.False should be(new DNF.Disj(TFalse, Set.empty))
      }
      "empty constant should be disj(false)" in {
        DNF.Disj.Empty should be(DNF.Disj.False)
      }
    }

    "disjunction (|) operator" - {
      "truth table identities" - {
        "with simple ternary values" - {
          "disj(true) | true --> disj(true)" in {
            (DNF.Disj.True | TTrue) should be(DNF.Disj.True)
          }
          "disj(true) | unknown --> disj(true)" in {
            (DNF.Disj.True | TUnknown) should be(DNF.Disj.True)
          }
          "disj(true) | false --> disj(true)" in {
            (DNF.Disj.True | TFalse) should be(DNF.Disj.True)
          }
          "disj(unknown) | true --> disj(true)" in {
            (DNF.Disj.Unknown | TTrue) should be(DNF.Disj.True)
          }
          "disj(unknown) | unknown --> disj(unknown)" in {
            (DNF.Disj.Unknown | TUnknown) should be(DNF.Disj.Unknown)
          }
          "disj(unknown) | false --> disj(unknown)" in {
            (DNF.Disj.Unknown | TFalse) should be(DNF.Disj.Unknown)
          }
          "disj(false) | true --> disj(true)" in {
            (DNF.Disj.False | TTrue) should be(DNF.Disj.True)
          }
          "disj(false) | unknown --> disj(unknown)" in {
            (DNF.Disj.False | TUnknown) should be(DNF.Disj.Unknown)
          }
          "disj(false) | false --> disj(false)" in {
            (DNF.Disj.False | TFalse) should be(DNF.Disj.False)
          }
        }
        "with ternary values in other disjunctions" - {
          "disj(true) | disj(true) --> disj(true)" in {
            (DNF.Disj.True | DNF.Disj.True) should be(DNF.Disj.True)
          }
          "disj(true) | disj(unknown) --> disj(true)" in {
            (DNF.Disj.True | DNF.Disj.Unknown) should be(DNF.Disj.True)
          }
          "disj(true) | disj(false) --> disj(true)" in {
            (DNF.Disj.True | DNF.Disj.False) should be(DNF.Disj.True)
          }
          "disj(unknown) | disj(true) --> disj(true)" in {
            (DNF.Disj.Unknown | DNF.Disj.True) should be(DNF.Disj.True)
          }
          "disj(unknown) | disj(unknown) --> disj(unknown)" in {
            (DNF.Disj.Unknown | DNF.Disj.Unknown) should be(DNF.Disj.Unknown)
          }
          "disj(unknown) | disj(false) --> disj(unknown)" in {
            (DNF.Disj.Unknown | DNF.Disj.False) should be(DNF.Disj.Unknown)
          }
          "disj(false) | disj(true) --> disj(true)" in {
            (DNF.Disj.False | DNF.Disj.True) should be(DNF.Disj.True)
          }
          "disj(false) | disj(unknown) --> disj(unknown)" in {
            (DNF.Disj.False | DNF.Disj.Unknown) should be(DNF.Disj.Unknown)
          }
          "disj(false) | disj(false) --> disj(false)" in {
            (DNF.Disj.False | DNF.Disj.False) should be(DNF.Disj.False)
          }
        }
      }
      "single conjunctions" - {
        "disj(true) | root:Int --> disj(true)" in {
          (DNF.Disj.True | rootIsInt) should be (new DNF.Disj(TTrue, Set.empty))
        }
        "disj(unknown) | root:Int --> disj(unknown | root:Int)" in {
          (DNF.Disj.Unknown | rootIsInt) should be (new DNF.Disj(TUnknown, Set(DNF.Conj(rootIsInt))))
        }
        "disj(false) | root:Int --> disj(false | root:Int)" in {
          (DNF.Disj.False | rootIsInt) should be (new DNF.Disj(TFalse, Set(DNF.Conj(rootIsInt))))
        }
      }
      "multiple conjunctions" - {
        "disj() | !root:Int | !root:Null --> disj(false | !root:Int | !root:Null)" in {
          (DNF.Disj.Empty | !rootIsInt | !rootIsNull) should be (new DNF.Disj(TFalse, Set(DNF.Conj(!rootIsInt), DNF.Conj(!rootIsNull))))
        }
      }
      "multiple disjunctions" - {
        "disj(true) & disj(conj(root:Int)) --> disj(conj(root:int))" in {
          val a = DNF.Disj(TTrue)
          val b = DNF.Disj(rootIsInt)
          println("Calculating...")
          val c = a & b
          println("Done")
          (DNF.Disj(TTrue) & DNF.Disj(rootIsInt)) should be(DNF.Disj(rootIsInt))
        }
      }
    }
    "conjunction (&) operator" - {
      "truth table identities" - {
        "disj(true) & true --> disj(true)" in {
          (DNF.Disj.True & TTrue) should be(DNF.Disj.True)
        }
        "disj(true) & unknown --> disj(unknown)" in {
          (DNF.Disj.True & TUnknown) should be(DNF.Disj.Unknown)
        }
        "disj(true) & false --> disj(false)" in {
          (DNF.Disj.True & TFalse) should be(DNF.Disj.False)
        }
        "disj(unknown) & true --> disj(unknown)" in {
          (DNF.Disj.Unknown & TTrue) should be(DNF.Disj.Unknown)
        }
        "disj(unknown) & unknown --> disj(unknown)" in {
          (DNF.Disj.Unknown & TUnknown) should be(DNF.Disj.Unknown)
        }
        "disj(unknown) & false --> disj(false)" in {
          (DNF.Disj.Unknown & TFalse) should be(DNF.Disj.False)
        }
        "disj(false) & true --> disj(false)" in {
          (DNF.Disj.False & TTrue) should be(DNF.Disj.False)
        }
        "disj(false) & unknown --> disj(false)" in {
          (DNF.Disj.False & TUnknown) should be(DNF.Disj.False)
        }
        "disj(false) & false --> disj(false)" in {
          (DNF.Disj.False & TFalse) should be(DNF.Disj.False)
        }
      }
      "single terms" - {
        "disj(true) & root:Int --> disj(true)" in {
          (DNF.Disj.True & rootIsInt) should be (DNF.Disj(DNF.Conj(rootIsInt)))
        }
        "disj(unknown) & root:Int --> disj(false | conj(unknown & root:Int))" in {
          (DNF.Disj.Unknown & rootIsInt) should be (new DNF.Disj(TFalse, Set(new DNF.Conj(TUnknown, Set(rootIsInt)))))
        }
        "disj(false) & root:Int --> disj(false)" in {
          (DNF.Disj.False & rootIsInt) should be (DNF.Disj.False)
        }
      }
      "single conjunction with multiple terms" - {
        "disj(false) & !root:Int & !root:Null --> disj(false | conj(true & root:Int & !root:Null))" in {
          (DNF.Disj(DNF.Conj(!rootIsInt)) & !rootIsNull) should be (new DNF.Disj(TFalse, Set(DNF.Conj(!rootIsInt, !rootIsNull))))
        }
      }
      "term applied to disjunction with multiple conjunctions" - {
        "disj(conj(!root:Int) | conj(!root:Null)) & root[0]:Int --> disj(conj(!root:Int & root[0]:Int) | conj(!root:Null & root[0]:Int))" in {
          (!rootIsInt | !rootIsNull) & root0IsInt should be ((!rootIsInt & root0IsInt) | (!rootIsNull & root0IsInt))
        }
      }
    }
    "negation (!) operator" - {
      "truth table" - {
        "!disj(true) --> disj(false)" in {
          !DNF.Disj.True should be (DNF.Disj.False)
        }
        "!disj(unknown) --> disj(unknown)" in {
          !DNF.Disj.Unknown should be (DNF.Disj.Unknown)
        }
        "!disj(false) --> disj(true)" in {
          (!DNF.Disj.False) should be (DNF.Disj.True)
        }
      }
      "single conjs" - {
        "!disj(conj(root:Int)) --> disj(conj(!root:int))" in {
          !(DNF.Disj(DNF.Conj(rootIsInt))) should be(DNF.Disj(DNF.Conj(!rootIsInt)))
        }
        "!disj(unknown | conj(root:Int)) --> disj(conj(unknown & !root:int))" in {
          !(DNF.Disj.Unknown | rootIsInt) should be(DNF.Disj(DNF.Conj.Unknown & !rootIsInt))
        }
      }
    }
  }
}

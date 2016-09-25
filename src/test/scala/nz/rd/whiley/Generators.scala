package nz.rd.whiley

import org.scalacheck.{Arbitrary, Gen}

object Generators {

  val valueGen: Gen[Value] = Gen.sized { size =>
    def genValue(remaining: Int): Gen[Value] = Gen.lzy {
      Gen.frequency(
        1 -> Gen.const(Value.Null),
        1 -> Arbitrary.arbitrary[Int].map(Value.Int(_)),
        1 -> {
          for {
            sizes <- genSizes(remaining - 1)
            children <- Gen.sequence[List[Value], Value](sizes.map(genValue))
          } yield Value.Product(children)
        }
      )
    }
    genValue(size)
  }

  def genSizes(remaining: Int): Gen[List[Int]] = {
    if (remaining <= 0) Gen.const(Nil) else {
      for {
        nodeSize <- Gen.choose(1, remaining)
        followingSizes <- genSizes(remaining - nodeSize)
      } yield nodeSize :: followingSizes
    }
  }

  val treeGen: Gen[Tree] = Gen.sized { sizeMinusOne =>

    def genTree(remaining: Int, boundNamesCount: Int, lastConcreteBinding: Int): Gen[Tree] = Gen.lzy {
      val freq = {
        Seq[(Int, Gen[Tree])](
          1 -> Tree.Any,
          1 -> Tree.Void,
          1 -> Tree.Null,
          1 -> Tree.Int,
          1 -> {
            for {
              sizes <- genSizes(remaining - 2)
              children <- Gen.sequence[List[Tree], Tree](sizes.map(genTree(_, boundNamesCount, boundNamesCount)))
            } yield Tree.Union(children)
          },
          1 -> {
            for {
              sizes <- genSizes(remaining - 2)
              children <- Gen.sequence[List[Tree], Tree](sizes.map(genTree(_, boundNamesCount, boundNamesCount)))
            } yield Tree.Intersection(children)
          },
          1 -> {
            for {
              sizes <- genSizes(remaining - 2)
              children <- Gen.sequence[List[Tree], Tree](sizes.map(genTree(_, boundNamesCount, boundNamesCount)))
            } yield Tree.Product(children)
          }
        )
      } ++ {
        if (lastConcreteBinding == 0) Seq.empty else {
          Seq[(Int, Gen[Tree])](
            1 -> Gen.choose(0, lastConcreteBinding - 1).map(n => Tree.Variable("X" + n))
          )
        }
      } ++ {
        if (remaining < 2) Seq.empty else {
          Seq[(Int, Gen[Tree])](
            1 -> genTree(remaining-1, boundNamesCount+1, lastConcreteBinding).map(Tree.Recursive("X"+boundNamesCount, _)),
            1 -> genTree(remaining-1, boundNamesCount, boundNamesCount).map(Tree.Negation)
          )
        }
      }

      Gen.frequency[Tree](freq: _*)
    }

    genTree(sizeMinusOne+1, 0, 0)

  }

  val graphGen: Gen[Graph] = Gen.sized { sizeMinusOne =>

    import Graph.Node

    def genNodeSizes(remaining: Int): Gen[List[Int]] = {
      if (remaining <= 0) Gen.const(Nil) else {
        for {
          nodeSize <- Gen.choose(1, remaining)
          followingSizes <- genNodeSizes(remaining - nodeSize)
        } yield nodeSize :: followingSizes
      }
    }

    def genNode(nodeCount: Int, nodeSize: Int): Gen[Node] = {
      require(nodeSize >= 1)
      val idGen: Gen[Graph.Id] = Gen.choose(0, nodeCount-1)
      Gen.frequency(
        1 -> Node.Any,
        1 -> Node.Void,
        1 -> Node.Null,
        1 -> Node.Int,
        1 -> idGen.map(Node.Negation(_)), // Actually size 2, not 1, but doesn't increase complexity
        1 -> Gen.listOfN(nodeSize - 1, idGen).map(Node.Union(_)),
        1 -> Gen.listOfN(nodeSize - 1, idGen).map(Node.Intersection(_))
      )
    }

    for {
      nodeSizes <- genSizes(sizeMinusOne + 1)
      nodeCount: Int = nodeSizes.size - 1
      root <- Gen.choose(0, nodeCount)
      nodes <- Gen.sequence[List[Node],Node](nodeSizes.map(genNode(nodeCount, _)))
    } yield {
      val g = Graph.empty
      g.root = root
      for (n <- nodes) { g += n }
      g
    }
  }
}

package steelmillslab

import org.xcsp.common.IVar.Var
import org.xcsp.modeler.ProblemAPI

/**
  * Created by vion on 19/06/17.
  */
class SteelMillSlab extends ProblemAPI {
  var nbOrders: Int = _
  var nbColours: Int = _
  var sizes: Array[Int] = _
  var ordSize: Array[Int] = _
  var ordCol: Array[Int] = _

  def model(): Unit = {


    val nbSlabs = nbOrders

    // freesPreComp(i) is best free space for a slab of load i
    val freesPreComp = Array.tabulate(sizes.max + 1) { l =>
      sizes.map(_ - l).filter(_ >= 0).min
    }

    // Computed free space (deducted from loads)
    val frees = array("frees", size(nbSlabs), dom(range(freesPreComp.max + 1)))

    // assign(i) == j iff order i is assigned to slab j
    val assign = array("assign", size(nbOrders), dom(range(nbSlabs)))

    // loads(i) is load of slab i
    val loads = array("loads", size(nbSlabs), dom(range(sizes.max + 1)))

    // Color constraint (at most 2 colors per slab)
    forall(range(nbSlabs), { i =>
      val presence = Seq.tabulate(nbColours) { c =>
        val alternatives = (0 until nbOrders).filter(o => ordCol(o) == c + 1).map(o => eq(assign(o), Int.box(i)))
        alternatives match {
          case Seq(a) => a
          case e => or(e: _*)
        }
      }
      lessEqual(add(presence: _*), 2)
    })

    // Packing constraint
    binPacking(loads, assign, ordSize)

    // Compute and minimize free space
    forall(range(nbSlabs), { i => element(freesPreComp, loads(i), frees(i)) })
    minimize(ProblemAPI.SUM, frees: _*)

    // Symmetry breaking
    forall(range(nbOrders).range(nbOrders), { (i, j) =>
      if (j > i && ordSize(i) == ordSize(j) && ordCol(i) == ordCol(j)) {
        lessEqual(assign(i), assign(j))
      }
    })

  }

  def binPacking(load: Array[Var], bin: Array[Var], w: Array[Int]): Unit = {
    sum(load, ProblemAPI.EQ, w.sum)
    // assignment(i, j) == 1 if bin(i) == j
    val assignments = array("assignments", size(load.length, bin.length), dom(0, 1))
    forall(range(load.length), { b =>
      channel(assignments(b), bin(b))
      sum(assignments(b), w, ProblemAPI.EQ, load(b))
    })
  }
}

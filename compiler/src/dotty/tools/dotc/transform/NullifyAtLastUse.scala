package dotty.tools.dotc
package transform

import core.*
import ast.tpd.*
import Contexts.*
import MegaPhase.*
import Annotations.*
import Symbols.defn
import Constants.*
import Types.*
import Decorators.*
import Flags.*
import dotty.tools.dotc.cc.CaptureSet.Var

object NullifyAtLastUse:
  val name: String = "nullifyAtLastUse"
  val description: String = "descr TODO"

  type TreeHash = Int //might get conflicts, try with just Trees
  case class NullifyPoints(removeAt: Set[TreeHash])//I guess I will add stuff. otherwise just inline it to the Map
  type VariableNullPoints = Map[Symbol, NullifyPoints]//rename to LastRefs  or smth
  //output: inverse the Map:Map[TreeHash->Set[Symbol]]

  // sym -> Set[Tree]

  // Tree -> Set[Symbol]

  /*
  if c then
    println(x~{x})
    (x: @lastUse)~{x}
  else {
    ()~{x}
    1
  }~{x}
  */


  extension (np: VariableNullPoints){
    def withSymbolAt(sym: Symbol, t: Tree): VariableNullPoints = 
      np.updated(sym->t.hashCode())
  }

  def startPoints: VariableNullPoints = Map.empty

  val nullPointsProperty = Property.StickyKey[VariableNullPoints]

/** 
 *  
 */
class NullifyAtLastUse extends MiniPhase {
  import  NullifyAtLastUse.*


  override def phaseName: String = NullifyAtLastUse.name
  override def description: String = NullifyAtLastUse.description

  override def prepareForDefDef(tree: DefDef)(using Context): Context = ctx

  //visit defdefs  children first
  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    
  }

  private class LastUseFinder(method: DefDef) extends TreeAccumulator[VariableNullPoints]{

    //all enclosed variables used in method
    private val enclosedVars = scala.collection.mutable.Set.empty[Symbol]

    override def apply(x: VariableNullPoints, tree: Tree)(using Context): VariableNullPoints = traverse(tree, x)

    def findAndAnnotate = 
      val nullPoints = traverse(method, startPoints)
      println(s"found null points for ${method.symbol}: $nullPoints")
      method.putAttachment(nullPointsProperty, nullPoints)

    private def traverse(tree: Tree, points: VariableNullPoints)(using Context): VariableNullPoints = {
      val sym = tree.symbol

      tree match
        case vd: ValDef => 
          val next = points.withSymbolAt(sym, tree)
          foldOver(next, vd)
        

        case Ident(name) => //
          if points.contains(sym) then points.withSymbolAt(sym, tree)//local symbol!!
            else if sym.isTerm then
              println(s"${method.symbol} reading $sym, seen as enclosing val")
              points.withSymbolAt(sym, tree)
            else
              println(s"${method.symbol} reading $sym, seen as something else")

        case If(cond, thenp, elsep) => 
          val tmp = traverse(cond, points)

          val b1 = traverse(thenp, tmp)
          val b2 = traverse(elsep, tmp)

          merge(b1, b2)


    }
  }
}


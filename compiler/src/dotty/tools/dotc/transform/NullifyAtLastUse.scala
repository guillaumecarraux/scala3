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

import dotty.tools.dotc.util.Property
import scala.collection.mutable
import Symbols.*


object NullifyAtLastUse:
  val name: String = "nullifyAtLastUse"
  val description: String = "descr TODO"

  enum RelativePosition:
    case Before
    case After


  case class PreciseTreeHash(hash: Int, relativePosition: RelativePosition)
  
  type VariableLastAccess = Map[Symbol, Set[PreciseTreeHash]]//rename to LastRefs or smth
  type TreeLastVariables = Map[PreciseTreeHash, Set[Symbol]]//slightly slower since I will traverse it twice, but ok for now, I never have soo many local vars


  import RelativePosition.*


  extension (base: VariableLastAccess){
    def withSymbolAt(sym: Symbol, t: Tree): VariableLastAccess = 
      base.updated(sym, Set(PreciseTreeHash(t.hashCode(), After)))//wanna remove after loading it



    def mergeIf(b1: VariableLastAccess, b1Start: PreciseTreeHash, b2: VariableLastAccess, b2Start: PreciseTreeHash): VariableLastAccess = {
      val solvedIf = base.map((sym, base_set) => 
          val b1Used = b1(sym) != base_set
          val b2Used = b2(sym) != base_set

          //if a variable is used in both branches: just keep both values
          //if it was used in one of the two, mark the other branch as able to remove it (in the provided start)
          //if none is using it: we can simply do nothing, the variable is removed before.

          (b1Used, b2Used) match //maybe better with ifs
            case (true, true) => (sym, b1(sym) ++ b2(sym))
            case (true, false) => (sym, b1(sym) + b2Start)
            case (false, true) => (sym, b2(sym) + b1Start)
            case (false, false) => (sym, base_set)
      )
      b1 ++ b2 ++ solvedIf //such, the modified base will overwrite all changes to other vars.
      //so variable local to each branch are added, and the enclosing one are in the solvedIf (always)
      //TODO make sure this works

      //TODO maybe just enter the branch with an empty Map. lower memory+perf
    }



    def exitLoop(afterLoop: VariableLastAccess, loopTree: PreciseTreeHash): VariableLastAccess = {
      val valsUsedInLoop = base.toSet.diff(afterLoop.toSet).toMap.keySet//remove variables that have the same hash

      //using overwriting behaviour: 
        //afterLoop has all variables after the loop, including the ones that never changed
        //valsUsedInLoops has all variables that changed in the loop
      afterLoop ++ valsUsedInLoop.map(sym => sym -> Set(loopTree)).toMap
    }

  }

  def startPoints: VariableLastAccess = Map.empty

  def startwithMethodParams(m:DefDef)(using Context): VariableLastAccess =
    val syms = m.paramss.flatten.flatMap(p => p match
      case v: ValDef  if !v.tpe.typeSymbol.isPrimitiveValueClass => Some(v.symbol)
      case _ => None
    )
    syms.map(sym => (sym -> Set(PreciseTreeHash(m.rhs.hashCode(), Before)))).toMap

  val varsToNullifyProperty = Property.StickyKey[Map[PreciseTreeHash, Set[Symbol]]]


/** 
 *  
 */
class NullifyAtLastUse extends MiniPhase {
  import  NullifyAtLastUse.*


  override def phaseName: String = NullifyAtLastUse.name
  override def description: String = NullifyAtLastUse.description


  /*
  When entering a function, I want to know local variables from enclosing methods. Can this work ? 
  mmmh hard, unless I attach it in posttyper, but I'm not a fan of it.

  other solution: when seeing a ident that I didnt valdef in f, mark the function as using it (and which is not a function! def or closure).

  
  */


  // override def prepareForDefDef(tree: DefDef)(using Context): Context = ctx

  //visit defdefs  children first
  override def transformDefDef(tree: DefDef)(using Context): Tree = {

    if ctx.settings.Yautoremove.value then
      val luf = LastUseFinder(tree)

      luf.findAndAnnotate


    tree
  }

  private class LastUseFinder(method: DefDef) extends TreeAccumulator[VariableLastAccess]{


    override def apply(x: VariableLastAccess, tree: Tree)(using Context): VariableLastAccess = traverse(tree, x)

    def invertMap(points: VariableLastAccess): TreeLastVariables = {
      // println(s"invertMap on $points")
      val res =
        points
          .toSeq
          .flatMap((sym, pts) => pts.map(p => (p -> sym)))
          .groupMap(_._1)(_._2)
          .view.mapValues(_.toSet)
          .toMap

      // println(s"yields  $res")

      res
    }


    def findAndAnnotate(using Context): Unit = 
      // println(s"[TRAVERSING METHOD ${method.name}]")
      //I dontt want startPoints. My startpoints are actually the method parameters  being removed at the beginning of the method.



      // val nullPoints = traverse(method, startwithMethodParams(method))
      val  nullPoints = startwithMethodParams(method)
      // println(s"found null points for ${method.symbol}: $nullPoints")
      method.putAttachment(varsToNullifyProperty, invertMap(nullPoints))

    private def traverse(tree: Tree, points: VariableLastAccess)(using Context): VariableLastAccess = {
      val sym = tree.symbol

      import RelativePosition.{Before, After}

      tree match
        case vd: ValDef if !vd.tpe.typeSymbol.isPrimitiveValueClass  => //TODO filter better and test filtering
          val next = points.withSymbolAt(sym, tree)
          foldOver(next, vd)
        

        case Ident(_) if points.contains(sym) => // tracked
            points.withSymbolAt(sym, tree)


        case If(cond, thenp, elsep) =>
          // println("caught an if")
          val afterCond = traverse(cond, points)
          val b1 = traverse(thenp, afterCond)
          val b2 = traverse(elsep, afterCond)
          //if not in branch, nullify at start of branch (before branch body)
          afterCond.mergeIf(b1, PreciseTreeHash(thenp.hashCode(),Before), b2, PreciseTreeHash(elsep.hashCode(), Before))

        case WhileDo(cond, body) => 
          val afterCond = traverse(cond, points)
          val afterBody = traverse(body, afterCond)

          //simply transfer variables created inside.
          //for variables updated in the loop, mark 
          points.exitLoop(afterBody, PreciseTreeHash(tree.hashCode(), After))
          
        case _ => foldOver(points, tree)
    }
  }
}


package com.yuiwai.dotmaster

import scala.util.chaining._
import com.yuiwai.dotmaster.Implicits._
import com.yuiwai.yachiyo.core.{Block, Pos, RGB}
import org.scalajs.dom
import org.scalajs.dom.ext.KeyCode
import org.scalajs.dom.raw.{CanvasRenderingContext2D, HTMLCanvasElement}

import scala.collection.immutable.Queue

object Main {
  def main(args: Array[String]): Unit = {
    val block = Block.fill(50, 50, RGB.Blue)
    val canvas = dom.document.createElement("canvas").asInstanceOf[HTMLCanvasElement]
    val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    canvas.setAttribute("width", "600")
    canvas.setAttribute("height", "600")
    dom.document.getElementById("stage").appendChild(canvas)

    var system: DrawSystem = DrawingMode(DrawState(Seq(block), 0), RGB.Red, drawing = false)

    canvas.onmousedown = { e =>
      val pos = Pos(e.clientX.toInt / 12, e.clientY.toInt / 12)
      system.behave(TouchStart(pos)) match {
        case (_, NoEvent) =>
        case (s, DotFilled(p, color)) =>
          system = s
          draw(p, color, ctx)
        case (s, Filled(ps, color)) =>
          system = s
          ps.foreach(p => draw(p, color, ctx))
        case _ =>
      }
    }
    canvas.onmousemove = { e =>
      val pos = Pos(e.clientX.toInt / 12, e.clientY.toInt / 12)
      system.behave(Moving(pos)) match {
        case (_, NoEvent) =>
        case (s, DotFilled(p, color)) =>
          system = s
          draw(p, color, ctx)
        case _ =>
      }
    }
    canvas.onmouseup = { e =>
      val pos = Pos(e.clientX.toInt / 12, e.clientY.toInt / 12)
      system.behave(TouchEnd(pos)) match {
        case (_, NoEvent) =>
        case (s, DotFilled(p, color)) =>
          system = s
          draw(p, color, ctx)
        case (s, DrawEnded) => system = s
        case _ =>
      }
    }
    // canvas.onmouseleave = { _ => canvas.onmousemove = null }
    dom.document.onkeyup = { e =>
      e.keyCode match {
        case KeyCode.D => system.behave(ChangeMode(ModeTypeDrawing)) match {
          case (s, ModeChanged(_)) => system = s
          case _ =>
        }
        case KeyCode.F => system.behave(ChangeMode(ModeTypeFill)) match {
          case (s, ModeChanged(_)) => system = s
          case _ =>
        }
        case KeyCode.S => system.behave(ChangeMode(ModeTypeSelect)) match {
          case (s, ModeChanged(_)) => system = s
          case _ =>
        }
        case _ =>
      }
    }

    draw(block, ctx)
  }
  def draw(block: Block[RGB], ctx: CanvasRenderingContext2D): Unit = {
    block.valuesWithPos.foreach { case (p, c) =>
      ctx.fillStyle = "#" + c.toHexString
      ctx.fillRect(p.x * 12, p.y * 12, 11, 11)
    }
  }
  def draw(block: Block[RGB], pos: P, ctx: CanvasRenderingContext2D): Unit = {
    block.posToIndex(pos).foreach { i =>
      ctx.fillStyle = "#" + block.values(i).toHexString
      ctx.fillRect(pos.x * 12, pos.y * 12, 11, 11)
    }
  }
  def draw(pos: P, color: RGB, ctx: CanvasRenderingContext2D): Unit = {
    ctx.fillStyle = "#" + color.toHexString
    ctx.fillRect(pos.x * 12, pos.y * 12, 11, 11)
  }
}

sealed trait Action
final case class ChangeMode(modeType: ModeType) extends Action
final case class TouchStart(pos: P) extends Action
final case class TouchEnd(pos: P) extends Action
final case class Moving(pos: P) extends Action
case object Clear extends Action
case class ChangeLayer(index: Int) extends Action

sealed trait Event
case object ModeNoChanged extends Event
final case class ModeChanged(newMode: ModeType) extends Event
case object NoEvent extends Event
final case class DotFilled(pos: P, color: RGB) extends Event
final case class Filled(ps: Set[P], color: RGB) extends Event
case object DrawEnded extends Event

final case class DrawState(layers: Seq[Block[RGB]], currentLayer: Int) {
  private def block: Option[Block[RGB]] = layers.lift(currentLayer)
  def land(pos: P): Set[P] =
    block.flatMap { b =>
      b.posToIndex(pos)
        .map(i => b.values(i))
        .map(landImpl(_, Set(pos), Queue(pos)))
    }.getOrElse(Set.empty)
  private def landImpl(color: RGB, visited: Set[P], queue: Queue[P]): Set[P] = {
    queue.dequeueOption match {
      case Some((p, q)) =>
        val b = block
        val ps = Set(p.left, p.right, p.up, p.down)
          .filterNot(visited)
          .filter(x => b.flatMap(y => y.posToIndex(x).map(i => y.values(i))).contains(color))
        landImpl(color, visited ++ ps, q.enqueueAll(ps))
      case None => visited
    }
  }
  private[dotmaster] def modified(f: Block[RGB] => Block[RGB]): Option[DrawState] =
    block.map(f).map(b => copy(layers.updated(currentLayer, b)))
  def draw(pos: P, color: RGB): (DrawState, Event) =
    block.map(_.updated(pos, color))
      .fold[(DrawState, Event)](this -> NoEvent) {
      b => copy(layers.updated(currentLayer, b)) -> DotFilled(pos, color)
    }
}

sealed trait DrawSystem {
  val state: DrawState
  val behave: PartialFunction[Action, (DrawSystem, Event)]
  val defaultBehavior: PartialFunction[Action, (DrawSystem, Event)] = {
    case ChangeMode(modeType) =>
      changeMode(modeType).fold[(DrawSystem, Event)](this -> ModeNoChanged)(_ -> ModeChanged(modeType))
    case _ => this -> NoEvent
  }
  def changeMode(modeType: ModeType): Option[DrawSystem] = modeType match {
    case ModeTypeDrawing => Some(DrawingMode(state, RGB.Red, drawing = false))
    case ModeTypeFill => Some(FillMode(state, RGB.Green))
    case ModeTypeSelect => Some(SelectMode(state, None))
  }
}

sealed trait ModeType
case object ModeTypeDrawing extends ModeType
case object ModeTypeFill extends ModeType
case object ModeTypeSelect extends ModeType

final case class DrawingMode(state: DrawState, currentColor: RGB, drawing: Boolean) extends DrawSystem {
  override val behave: PartialFunction[Action, (DrawSystem, Event)] = {
    case TouchStart(pos) => draw(pos)
    case TouchEnd(_) => copy(drawing = false) -> DrawEnded
    case Moving(pos) => if (drawing) draw(pos) else this -> NoEvent
    case other => defaultBehavior(other)
  }
  private def draw(pos: P): (DrawingMode, Event) = state.draw(pos, currentColor) match {
    case (s, e) => copy(state = s, drawing = true) -> e
  }
}

final case class FillMode(state: DrawState, fillColor: RGB) extends DrawSystem {
  override val behave: PartialFunction[Action, (DrawSystem, Event)] = {
    case TouchStart(pos) => fill(pos) match {
      case (s, ps) => copy(state = s) -> Filled(ps, fillColor)
    }
    case other => defaultBehavior(other)
  }
  def fill(pos: P): (DrawState, Set[P]) = {
    state.land(pos).pipe { ps =>
      state.modified(b => Block.withValues(b.width, b.mapValuesWithPos {
        case (x, r) => if (ps(x)) fillColor else r
      }))
        .getOrElse(state) -> ps
    }
  }
}

final case class SelectMode(state: DrawState, startPos: Option[P]) extends DrawSystem {
  override val behave: PartialFunction[Action, (DrawSystem, Event)] = {
    case other => defaultBehavior(other)
  }
}

final case class LineMode(state: DrawState) extends DrawSystem {
  override val behave: PartialFunction[Action, (DrawSystem, Event)] = {
    case other => defaultBehavior(other)
  }
}

object Implicits {
  type P = Pos[Int]
  type R = (DrawSystem, Event)

  implicit class RichP(p: P) {
    def left: P = p.copy(x = p.x - 1)
    def right: P = p.copy(x = p.x + 1)
    def up: P = p.copy(y = p.y - 1)
    def down: P = p.copy(y = p.y + 1)
  }
}

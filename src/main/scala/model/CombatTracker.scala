package me.mtrupkin.game.model

import me.mtrupkin.console.{Colors, RGB, ScreenChar, Screen}
import me.mtrupkin.core.{StateMachine, Size, Point}
import me.mtrupkin.pathfinding.{Dijkstra, AStar}

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer


/**
 * Created by mtrupkin on 12/19/2014.
 * World controller
 */
class CombatTracker(val world: World) extends StateMachine  {
  type StateType = ActionState
  def initialState: StateType = new InputAction()

  trait ActionState extends State {
    def update(elapsed: Int): Unit
    def render(screen: Screen): Unit
    def target(p: Point): Unit = {}
    def handleComplete(sender: ActionState) = nextAction()
  }

  def complete(sender: ActionState) = currentState.handleComplete(sender)

  def nextAction(): Unit = currentState.changeState(nextActionState())

  def passTurn() = {
    world.player.ap = 0
    nextAction()
  }

  def end: Boolean = {
    (agents == Nil) || (player.hp < 0)
  }

  var round: Int = 0

  var mouse: Option[Point] = None

  val pathFinder = new Dijkstra(world.tileMap)

  def agents: Seq[Agent] = world.agents.filter(_.hp >= 0)
  def player = world.player

  def update(elapsed: Int) = {
    world.update(elapsed)
    currentState.update(elapsed)
  }

  def render(screen: Screen) = {
    world.render(screen)
    renderAgents(screen)
    currentState.render(screen)
  }

  def renderAgents(screen: Screen) = {
    for (a <- agents) {
      // TODO: display range
      if (lineOfSight(a.position, world.player.position) != Nil)
        world.renderAgent(screen, a)
      else
        screen.write(a.position, a.sc.copy(fg = RGB(123, 123, 123)))
    }
  }

  def target(p: Point): Unit = currentState.target(p)

  def nextActionState(): ActionState = {
    if (player.ap > 0) new InputAction
    else {
      enemyAction()
    }
  }

  def endRound(): Unit = {
    val entities = player :: agents.toList
    entities.foreach(_.endRound())

    round += 1
  }

  def enemyAction(): ActionState = {
    val actions = for {
      agent <- agents
    } yield agent.getAction(this)

    new CompositeActionState(actions.flatten.map(getActionState(_)))
  }

  def attack(attacker: Entity, defender: Entity, resolution: (Combat, Int) => Int = Combat.attack): Boolean = {
    val lineOfSight = this.lineOfSight(attacker.position, defender.position)
    if (lineOfSight != Nil) {
      Combat.attack(attacker.melee, defender, resolution)
      true
    } else false
  }

  // uses neighbors in addition to current tile
  def lineOfSight(p: Point, p0: Point): Seq[Point] = {
    for( n <- p :: world.tileMap.size.neighbors(p, 1).toList ) {
      val line = lineOfSightSingle(n, p0)
      if (line != Nil) return line
    }
    Nil
  }

  // uses current tile only
  protected def lineOfSightSingle(p: Point, p0: Point): Seq[Point] = {
    // TODO: optimization candidate
    val line = CombatTracker.bresenham(p, p0).toSeq
    if (line.forall(world.tileMap(_).move)) line else Nil
  }

  val actionOptions = List(
    new BurstAttackOption(this),
    new AimAttackOption(this),
    new AttackOption(this),
    new MoveOption(this))

  def getAction(p: Point): Option[Action] = {
    def getAction(target: Point, actions: List[ActionOption]): Option[Action] = {
      actions match {
        case actionOption :: xs => {
          if (actionOption.selected)
            actionOption.getAction(target) match {
              case None => getAction(target, xs)
              case action => action
            }
          else getAction(target, xs)
        }
        case Nil => None
      }
    }
    getAction(p, actionOptions)
  }

  def getActionState(action: Action): ActionState = {
    action match {
      case move: MoveAction => new MoveState(move)
      case attack: AttackAction => new AttackState(attack)
      case _ => ???
    }
  }

  class InputAction extends ActionState {
    import InputAction._

    override def target(target: Point): Unit = {
      for(action <- getAction(target)) {
        changeState(getActionState(action))
      }
    }

    def update(elapsed: Int): Unit = {}

    def render(screen: Screen): Unit = {
      renderValidMoves(screen)
      renderValidPath(screen)
    }

    def renderValidPath(screen: Screen): Unit = {
      for (m <- mouse) {
        getAction(m) match {
          case Some(move: MoveAction) => renderPath(screen, m)
          case _ =>
        }
      }
    }

    def renderValidMoves(screen: Screen): Unit = {
      val move = world.player.move
      val p0 = world.player.position

      for {
        x <- -move to move
        y <- -move to move
        p = p0 + (x, y)
        moves = pathFinder.moveCount(p, p0, move)
        if ((moves > 0) && (moves <= move))
        if agents.forall(_.position != p)
      } screen(p) = moveChar
    }

    def renderPath(screen: Screen, target: Point): Unit = {
      val move = world.player.move
      val p0 = world.player.position

      val moves = pathFinder.moveCount(target, p0, move)
      if ((moves > 0) && (moves <= move)) {
        val path = pathFinder.path(target, p0)
        val smoothPoints = smoothPathPoints(p0, path, Nil)

        smoothPoints.foreach(screen(_) = pathChar)
      }
    }
  }

  def hasLineOfMovement(p: Point, p0: Point): Boolean = {
    CombatTracker.bresenham(p, p0).forall(world.tileMap(_).move)
  }

  // returns smooth path up to point p0
  // finds the line from p0 to a point pn previousPath that is not blocked
  // returns the line and the rest of the path
  // param next is the last point that had a line of sight to p0
  def smoothPreviousPath(p0: Point, next: Point, previousPath: List[Point]): List[Point] = {
    previousPath match {
      case p :: ps => if (hasLineOfMovement(p, p0))
        smoothPreviousPath(p0, p, ps)
      else
        p0 :: (CombatTracker.bresenham(next, p0) ++ previousPath).toList
      case Nil => p0 :: CombatTracker.bresenham(next, p0).toList
    }
  }

  def smoothPathPoints(p0: Point, path: Seq[Point], smoothed: List[Point]): Seq[Point] = {
    path match {
      case p :: ps => smoothPathPoints(p, ps, smoothPreviousPath(p, p0, smoothed))
      // XXX: try to remove reverse and tail call
      case Nil => smoothed.reverse.tail
    }
  }

  class MoveState(val action: MoveAction) extends ActionState {
    val p0 = action.entity.position
    val path = pathFinder.path(action.target, p0)
    var smoothPath = smoothPathPoints(p0, path, Nil)

    def update(elapsed: Int): Unit = {
      smoothPath match {
        case x::xs =>
          action.entity.position = x
          smoothPath = xs
        case Nil => {
          action.complete()
          complete(this)
        }
      }
    }

    def render(screen: Screen): Unit = {}
  }

  class AttackState(val action: AttackAction) extends ActionState {
    var path = lineOfSight(action.defender.position, action.attacker.position).toList

    var time = 0
    val rate = 300
    def update(elapsed: Int): Unit = {
      time += elapsed

      path match {
        case p::ps => if (time > rate) {
          time -= rate
          path = ps
        }
        case Nil => {
          action.complete()
          complete(this)
        }
      }
    }

    def render(screen: Screen): Unit = {
      path match {
        case p::ps => screen.write(p, 'Q')
        case Nil =>
      }
    }
  }

  class CompositeActionState(val actions: Seq[ActionState]) extends ActionState {
    val actionsBuffer = actions.to[ListBuffer]

    def update(elapsed: Int): Unit = {
      actionsBuffer.foreach(_.update(elapsed))
      if (actionsBuffer == Nil) {
        endRound()
        nextAction()
      }
    }
    def render(screen: Screen): Unit = actionsBuffer.foreach(_.render(screen))

    override def handleComplete(state: ActionState): Unit = {
      actionsBuffer -= state
    }
  }
}

object InputAction {
  import Colors._

  val moveChar = ScreenChar('.', RGB(61, 61, 61), Black)
  val pathChar = ScreenChar('.', White, Black)
}



object Line {
  /**
   * Uses the Bresenham Algorithm to calculate all points on a line from p0 to p1.
   * The iterator returns all points in the interval [start, end].
   * @return the iterator containing all points on the line
   */
  protected def bresenham(p1: Point, p0: Point): Iterator[Point] = {
    import scala.math.abs
    val d = Point(abs(p1.x - p0.x), abs(p1.y - p0.y))

    val sx = if (p0.x < p1.x) 1 else -1
    val sy = if (p0.y < p1.y) 1 else -1

    new Iterator[Point] {
      var p = p0
      var err = d.x - d.y

      def next = {
        val e2 = 2 * err
        if (e2 > -d.y) {
          err -= d.y
          p = p.copy(x = p.x + sx)
        }
        if (e2 < d.x) {
          err += d.x
          p = p.copy(y = p.y + sy)
        }
        p
      }
      def hasNext = !(p == p1)
    }
  }
}
object CombatTracker {

  /**
   * Uses the Bresenham Algorithm to calculate all points on a line from p0 to p1.
   * The iterator returns all points in the interval [start, end).
   * @return the iterator containing all points on the line
   */
  protected def bresenham(p1: Point, p0: Point): Iterator[Point] = {
    import scala.math.abs
    val d = Point(abs(p1.x - p0.x), abs(p1.y - p0.y))

    val sx = if (p0.x < p1.x) 1 else -1
    val sy = if (p0.y < p1.y) 1 else -1

    new Iterator[Point] {
      var p = p0
      var err = d.x - d.y

      def next = {
        val e2 = 2 * err
        if (e2 > -d.y) {
          err -= d.y
          p = p.copy(x = p.x + sx)
        }
        if (e2 < d.x) {
          err += d.x
          p = p.copy(y = p.y + sy)
        }
        p
      }
      def hasNext = !(p == p1)
    }
  }
}



package me.mtrupkin.game.model



import me.mtrupkin.core.Dice

/**
 * Created by mtrupkin on 12/19/2014.
 */
trait Combat {
  def attack: Int
  def damage: Int
}

object Combat {
  def roll: Int = Dice(3, 6) // 3d6

  def apply(f: => Int): Combat = new Combat {
    def attack: Int = f
    def damage: Int = f
  }

  def playerAttack(attacker: Combat, defense: Int): Int = {
    val attack = attacker.attack + roll

    val effect = Math.max(0, attack - (defense + 11))
    val damage = attacker.damage + effect

    damage
  }

  def attack(attacker: Combat, defense: Int): Int = {
    val attack = attacker.attack + roll

    val effect = attack - (defense + 11)
    if (effect > 0) attacker.damage + effect else 0
  }

  def attack(attackType: Combat, defender: Entity, resolution: (Combat, Int) => Int = attack): Unit = {
    val damage = resolution(attackType, defender.defense)
    defender.hp -= damage
  }
}

object Simulator {

  def simulate(attackers: List[Combat], maxHP: Int, defense: Int): (Int, Seq[Int]) = {
    var hp = maxHP
    var round = 0
    var results: List[Int] = Nil
    do {
      val r1 = for (attacker <- attackers) yield {
        val damage = Combat.attack(attacker, defense)
        hp -= damage
        damage
      }
      results = results ::: r1
      round += 1
    } while (hp >= 0)

    round -> results
  }

  def simulate(attackers: List[Combat] = List(Combat(0), Combat(0), Combat(0))): Unit = {
    val encounters = for(i <- 0 until 10000) yield simulate(attackers, 10, 0)
    val rounds = encounters.map(_._1)
    val attacks = encounters.flatMap(_._2)

    import Statistics._
    output("Number of rounds", rounds)
    println(f"Average damage taken per attack: ${mean(attacks)}%3.3g")

  }
}


object Statistics {
  def mean(xs: Seq[Int]): Double = xs match {
    case Nil => 0.0
    case ys => ys.sum / ys.size.toDouble
  }

  def stddev(xs: Seq[Int], avg: Double): Double = xs match {
    case Nil => 0.0
    case ys => math.sqrt((0.0 /: ys) {
      (a,e) => a + math.pow(e - avg, 2.0)
    } / xs.size)
  }

  def output(s: String, xs: Seq[Int]): Unit = {
    println(s)
    val avg = mean(xs)
    val std = stddev(xs, avg)
    val max = xs.max
    val min = xs.min

    println(f"size: ${xs.size}")
    println(f" avg: $avg%3.3g")
    println(f" std: $std%3.3g")
    println(f" max(count): $max%4d (${xs.count(_==max)})")
    println(f" min(count): $min%4d (${xs.count(_==min)})")
  }
}
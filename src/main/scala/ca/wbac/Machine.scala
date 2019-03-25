package ca.wbac

import ca.wbac.State.{get, modify, sequence}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    sequence(inputs.map(input => modify(machine => nextState(machine, input))))
      .flatMap(_ => get.map(state => (state.candies, state.coins)))

  private def nextState(machine: Machine, input: Input): Machine = (input, machine) match {
    case (Coin, Machine(true, candies, _)) if candies > 0 => Machine(locked = false, candies, machine.coins + 1)
    case (Turn, Machine(false, candies, _)) if candies > 0 => Machine(locked = true, candies - 1, machine.coins)
    case _ => machine
  }

}

package vm

abstract class Action

class Function(val rule: Rule, val action: Vector[Action]) {
}

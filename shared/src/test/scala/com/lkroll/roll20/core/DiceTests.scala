package com.lkroll.roll20.core

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class DiceTests extends AnyFunSuite with Matchers {

  import CoreImplicitsExplicitLabels._;

  val testField = TestField("test_field");

  test("basic dice") {
    Dice.d20.render shouldBe "1d20"
    Dice.d8.copy(n = 42).render shouldBe "42d8"
    5.d(10).render shouldBe "5d10"
  }

  test("criticals") {
    (Dice.d20.cs() `=` 18).render shouldBe "1d20cs18"
    (Dice.d20.cf() `=` 2).render shouldBe "1d20cf2"
    (5.d(10).cs() `=` 2).render shouldBe "5d10cs2"

    (Dice.d20.cs() > 18).render shouldBe "1d20cs>18"
    (Dice.d20.cf() < 2).render shouldBe "1d20cf<2"

    (Dice.d20.cs() > testField).render shouldBe "1d20cs>@{test_field}"
    (Dice.d20.cs() `=` testField).render shouldBe "1d20cs@{test_field}"
    (Dice.d20.cs()(testField)).render shouldBe "1d20cs@{test_field}"
  }
}

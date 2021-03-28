package com.lkroll.roll20.core

import org.scalatest._
import org.scalatest.funsuite._
import org.scalatest.matchers.should.Matchers

class LabelledRenderingTest extends AnyFunSuite with Matchers {
  val testField = TestField("test_field");

  test("CoreImplicitsLabelFields should render fields labelled") {
    import CoreImplicitsLabelFields._;
    val expr: AutocalcExpression[Int] = testField;
    expr.render should be("@{test_field}[test field]");
  }

  test("CoreImplicitsExplicitLabels should render fields unlabelled") {
    import CoreImplicitsExplicitLabels._;
    val expr: AutocalcExpression[Int] = testField;
    expr.render should be("@{test_field}");
  }
}

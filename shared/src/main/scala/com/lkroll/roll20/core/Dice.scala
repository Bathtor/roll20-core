/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2017 Lars Kroll <bathtor@googlemail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package com.lkroll.roll20.core

sealed trait DiceExpression extends Renderable {
  import ComparePoints._;
  import RollModifiers._;
  import DiceExprs._;
  import RollExprs.{Dice => RDice, _};
  import Arith.{RollArith => RArith, _};
  import AccessTransformation.AccessTransformer;

  def ++(mod: RollModifier): IntRollExpression = RollExprs.WithMods(this, mod);

  def <(target: Int): IntRollExpression = this ++ TargetRoll(LeqCP(Left(target)));
  def >(target: Int): IntRollExpression = this ++ TargetRoll(GeqCP(Left(target)));
  def `=`(target: Int): IntRollExpression = this ++ TargetRoll(EqualCP(Left(target)));
  def cs(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CriticalSuccess(cp));
  def cf(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CriticalFailure(cp));
  def f(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ FailuresRoll(cp));
  def !(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ ExplodingRoll(cp));
  def !!(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ CompoundingRoll(cp));
  def `!p`(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ PenetratingRoll(cp));
  def r(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ RerollRoll(cp));
  def ro(): ModifierLackingCP[IntRollExpression] = ELCP(cp => this ++ RerollOnceRoll(cp));
  def kh(target: Int): IntRollExpression = this ++ KeepHighestRoll(target);
  def kl(target: Int): IntRollExpression = this ++ KeepLowestRoll(target);
  def dh(target: Int): IntRollExpression = this ++ DropHighestRoll(target);
  def dl(target: Int): IntRollExpression = this ++ DropLowestRoll(target);
  def s(): IntRollExpression = this ++ SortAscRoll;
  def sa(): IntRollExpression = this ++ SortAscRoll;
  def sd(): IntRollExpression = this ++ SortDescRoll;

  override def toString(): String = this.render;

  def arith: ArithmeticExpression[Int] = RArith(RDice(this));

  def +(other: ArithmeticExpression[Int]): RollExpression[Int] = Math(PlusExpr(this.arith, other));
  def -(other: ArithmeticExpression[Int]): RollExpression[Int] = Math(MinusExpr(this.arith, other));
  def /(other: ArithmeticExpression[Int]): RollExpression[Int] = Math(DivExpr(this.arith, other));
  def *(other: ArithmeticExpression[Int]): RollExpression[Int] = Math(MultExpr(this.arith, other));
  def %(other: ArithmeticExpression[Int]): RollExpression[Int] = Math(ModExpr(this.arith, other));

  def as[O: Numeric] = this.arith.as[O];

  // special version for common case
  def +(other: FieldLike[Int])(implicit labelFields: LabelFields): RollExpression[Int] =
    Math(PlusExpr(this.arith, other.arith));
  def -(other: FieldLike[Int])(implicit labelFields: LabelFields): RollExpression[Int] =
    Math(MinusExpr(this.arith, other.arith));
  def /(other: FieldLike[Int])(implicit labelFields: LabelFields): RollExpression[Int] =
    Math(DivExpr(this.arith, other.arith));
  def *(other: FieldLike[Int])(implicit labelFields: LabelFields): RollExpression[Int] =
    Math(MultExpr(this.arith, other.arith));
  def %(other: FieldLike[Int])(implicit labelFields: LabelFields): RollExpression[Int] =
    Math(ModExpr(this.arith, other.arith));

  def transformForAccess(f: AccessTransformer): DiceExpression;
}

object DiceExprs {
  import RollModifiers._
  import AccessTransformation.AccessTransformer;

  object ELCP {
    def apply(completer: ComparePoint => IntRollExpression): ModifierLackingCP[IntRollExpression] =
      new ModifierLackingCP[IntRollExpression](completer);
  }

  case class BasicRoll(n: DiceParameter, x: DiceParameter) extends DiceExpression {
    override def render: String = {
      val numberOfDice = n match {
        case DiceParams.ArithmeticParameter(Arith.Literal(num)) => num.toString
        case _                                                  => s"(${n.render})"
      }
      val dieSize = x match {
        case DiceParams.ArithmeticParameter(Arith.Literal(num)) => num.toString
        case _                                                  => s"(${x.render})"
      }
      s"${numberOfDice}d${dieSize}"
    }

    override def transformForAccess(f: AccessTransformer): DiceExpression =
      this.copy(n = n.transformForAccess(f), x = x.transformForAccess(f));
  }

  case class FateRoll(n: DiceParameter) extends DiceExpression {
    override def render: String = s"(${n.render})dF";

    override def transformForAccess(f: AccessTransformer): DiceExpression =
      this.copy(n = n.transformForAccess(f));
  }

  case class DiceQuery(query: SelectQuery[DiceExpression]) extends DiceExpression {
    override def render: String = query.render;

    override def transformForAccess(f: AccessTransformer): DiceExpression = this;
  }

}

sealed trait DiceParameter extends Renderable {
  import AccessTransformation.AccessTransformer;

  def transformForAccess(f: AccessTransformer): DiceParameter;
}

object DiceParams {
  import AccessTransformation.AccessTransformer;

  case class AutocalcParameter(n: AutocalcExpression[Int]) extends DiceParameter {
    lazy val nNoLabel = n.transformForAccess(AccessTransformation.Delabel);

    override def render: String = nNoLabel.render;

    override def transformForAccess(f: AccessTransformer): DiceParameter = this.copy(n.transformForAccess(f));
  }

  case class ArithmeticParameter(n: ArithmeticExpression[Int]) extends DiceParameter {
    lazy val nNoLabel = n.transformForAccess(AccessTransformation.Delabel);

    override def render: String = nNoLabel.render;

    override def transformForAccess(f: AccessTransformer): DiceParameter = this.copy(n.transformForAccess(f));
  }

  case class QueryParameter(query: RollQuery[Int]) extends DiceParameter {
    override def render: String = query.render;

    override def transformForAccess(f: AccessTransformer): DiceParameter = this;
  }

}

object Dice {
  import CoreImplicits._;
  import DiceExprs._

  val unit = BasicRoll(1, 0);
  val d4 = BasicRoll(1, 4);
  val d6 = BasicRoll(1, 6);
  val d8 = BasicRoll(1, 8);
  val d10 = BasicRoll(1, 10);
  val d12 = BasicRoll(1, 12);
  val d20 = BasicRoll(1, 20);
  val d100 = BasicRoll(1, 100);

}

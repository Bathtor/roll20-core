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

import scala.collection.Seq

sealed trait RollExpression[T] extends Renderable {
  import RollExprs._;
  import Arith.{ RollArith => Arith };
  import AccessTransformation._;

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) + other;
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) - other;
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) / other;
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) * other;
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = Arith(this) % other;
  def as[O: Numeric](implicit n: Numeric[T]) = Arith(this).as[O];
  def arith(implicit n: Numeric[T]): ArithmeticExpression[T] = Arith(this);

  // special version for common case
  def +(other: FieldLike[T])(implicit n: Numeric[T], labelFields: LabelFields) = Arith(this) + other.arith();
  def -(other: FieldLike[T])(implicit n: Numeric[T], labelFields: LabelFields) = Arith(this) - other.arith();
  def /(other: FieldLike[T])(implicit n: Numeric[T], labelFields: LabelFields) = Arith(this) / other.arith();
  def *(other: FieldLike[T])(implicit n: Numeric[T], labelFields: LabelFields) = Arith(this) * other.arith();
  def %(other: FieldLike[T])(implicit n: Numeric[T], labelFields: LabelFields) = Arith(this) % other.arith();

  def &(option: RollOption) = WithOption(this, option);
  def label(s: String) = LabelledRoll(this, s);

  def transformForAccess(f: AccessTransformer): RollExpression[T];
  def forCharacter(characterName: String): RollExpression[T] = transformForAccess(Character(characterName));
  def forSelected(): RollExpression[T] = transformForAccess(Selected);
  def forTarget(): RollExpression[T] = transformForAccess(Targeted);
  def forTarget(targetName: String): RollExpression[T] = transformForAccess(Target(targetName));

  def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[T];
}

sealed trait IntRollExpression extends RollExpression[Int] {
  import ComparePoints._;
  import RollModifiers._;
  import DiceExprs._;
  import AccessTransformation.AccessTransformer;

  def ++(mod: RollModifier): IntRollExpression;
  def <(target: Int): IntRollExpression = this ++ TargetRoll(LeqCP(target));
  def >(target: Int): IntRollExpression = this ++ TargetRoll(GeqCP(target));
  def `=`(target: Int): IntRollExpression = this ++ TargetRoll(EqualCP(target));
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
  override def transformForAccess(f: AccessTransformer): RollExpression[Int] = this.transformForAccessIRE(f);
  protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression;
  override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[Int] = {
    replacement.value match {
      case i: Int => this.replaceQueryIRE(replacement.asInstanceOf[QueryReplacer[Int]])
      case _      => this
    }
  }
  protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression;
}

object RollExprs {
  import AccessTransformation.AccessTransformer;

  case class Dice(dice: DiceExpression) extends IntRollExpression {
    override def render: String = dice.render
    override def ++(mod: RollModifier): IntRollExpression = WithMods(dice, mod);
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression = this;
    override protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression = this;
  }

  case class WithMods(dice: DiceExpression, mod: RollModifier) extends IntRollExpression {
    override def render: String = s"${dice.render}${mod.render}";
    override def ++(mod: RollModifier): IntRollExpression = WithMods(dice, this.mod ++ mod);
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression = this;
    override protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression = this;
  }

  case class Group(exprs: Seq[IntRollExpression]) extends IntRollExpression {
    override def render: String = exprs.map(_.render).mkString("{", ",", "}");
    def ++(mod: RollModifier): IntRollExpression = GroupWithMods(exprs, mod);
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression =
      Group(exprs.map(_.transformForAccessIRE(f)));
    override protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression =
      Group(exprs.map(_.replaceQueryIRE(replacement)));
  }

  case class GroupWithMods(exprs: Seq[IntRollExpression], mod: RollModifier) extends IntRollExpression {
    override def render: String = exprs.map(_.render).mkString("{", ",", "}") + mod.render;
    override def ++(mod: RollModifier): IntRollExpression = GroupWithMods(exprs, this.mod ++ mod);
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression =
      GroupWithMods(exprs.map(_.transformForAccessIRE(f)), mod);
    override protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression =
      Group(exprs.map(_.replaceQueryIRE(replacement)));
  }

  case class Inline(roll: Rolls.InlineRoll[Int]) extends IntRollExpression {
    override def render: String = roll.render;
    override def ++(mod: RollModifier): IntRollExpression = GroupWithMods(Seq(this), mod);
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression = Inline(roll.transformForAccess(f));
    override protected[core] def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression = Inline(roll.replaceQuery(replacement));
  }

  case class Math[T: Numeric](expr: ArithmeticExpression[T]) extends RollExpression[T] {
    override def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): RollExpression[T] = Math(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[T] = Math(expr.replaceQuery(replacement));
  }

  case class AsIRE(expr: RollExpression[Int]) extends IntRollExpression {
    override def render: String = expr.render;
    override def ++(mod: RollModifier): IntRollExpression = GroupWithMods(Seq(this), mod);
    override def transformForAccessIRE(f: AccessTransformer): IntRollExpression = AsIRE(expr.transformForAccess(f));
    override def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression = AsIRE(expr.replaceQuery(replacement));
  }

  case class WithIntQuery(query: RollQuery[Int]) extends IntRollExpression {
    override def render: String = query.render;
    override def ++(mod: RollModifier): IntRollExpression = ???; // not allowed here
    override protected[core] def transformForAccessIRE(f: AccessTransformer): IntRollExpression = this;
    override def replaceQueryIRE(replacement: QueryReplacer[Int]): IntRollExpression = {
      if (query.name.equals(replacement.name)) {
        AsIRE(Math(Arith.Literal(replacement.value)))
      } else {
        this
      }
    }
  }

  case class WithOption[T](expr: RollExpression[T], option: RollOption) extends RollExpression[T] {
    override def render: String = s"${expr.render} ${option.render}";
    override def transformForAccess(f: AccessTransformer): RollExpression[T] = WithOption(expr.transformForAccess(f), option);
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[T] = WithOption(expr.replaceQuery(replacement), option);
  }

  case class LabelledRoll[T](expr: RollExpression[T], label: String) extends RollExpression[T] {
    override def render: String = s"${expr.render}[$label]";
    override def transformForAccess(f: AccessTransformer): RollExpression[T] = LabelledRoll(expr.transformForAccess(f), label);
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[T] = LabelledRoll(expr.replaceQuery(replacement), label);
  }

  case class Native[T](expr: String) extends RollExpression[T] {
    override def render: String = expr;
    override def transformForAccess(f: AccessTransformer): RollExpression[T] = ???; // Can't inspect native rolls
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollExpression[T] = ???; // Can't inspect native rolls
  }
}

sealed trait RollOption extends Renderable;

object RollOptions {
  case object NoError extends RollOption {
    override def render: String = "&{noerror}";
  }

  case object Tracker extends RollOption {
    override def render: String = "&{tracker}";
  }

  case object TrackerPlus extends RollOption {
    override def render: String = "&{tracker:+}";
  }

  case object TrackerMinus extends RollOption {
    override def render: String = "&{tracker:-}";
  }
}

sealed trait Roll extends Renderable {
  import AccessTransformation._;

  type RollType <: Roll;

  def transformForAccess(f: AccessTransformer): RollType;
  def forCharacter(characterName: String): RollType = transformForAccess(Character(characterName));
  def forSelected(): RollType = transformForAccess(Selected);
  def forTarget(): RollType = transformForAccess(Targeted);
  def forTarget(targetName: String): RollType = transformForAccess(Target(targetName));

  def replaceQuery[QT](replacement: QueryReplacer[QT]): RollType;
}

object Rolls {
  import AccessTransformation.AccessTransformer;

  case class SimpleRoll[T](expr: RollExpression[T]) extends Roll {
    override type RollType = SimpleRoll[T];

    override def render: String = s"/roll ${expr.render}";
    override def transformForAccess(f: AccessTransformer): RollType = SimpleRoll(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollType = SimpleRoll(expr.replaceQuery(replacement));
  }

  case class InlineRoll[T](expr: RollExpression[T]) extends Roll {
    override type RollType = InlineRoll[T];

    override def render: String = s"[[${expr.render}]]";
    override def transformForAccess(f: AccessTransformer): RollType = InlineRoll(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollType = InlineRoll(expr.replaceQuery(replacement));
  }

  case class TemplateRoll(chat: ChatCommand, template: TemplateApplication) extends Roll {
    override type RollType = TemplateRoll;

    override def render: String = s"${chat.render}${template.render}";
    override def transformForAccess(f: AccessTransformer): RollType = this; // TODO maybe carry this through to TemplateApplication
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollType = this; // TODO maybe carry this through to TemplateApplication
  }

  case class APIRoll(command: String, args: List[(String, Renderable)]) extends Roll {
    override type RollType = APIRoll;

    lazy val apiCmd = Chat.API(command, args.map(t => s"--${t._1} ${t._2.render}").mkString(" "));

    override def render: String = apiCmd.render;
    override def transformForAccess(f: AccessTransformer): RollType = this;
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): RollType = this;
  }
}

trait TemplateApplication extends Renderable {

}

sealed trait ComparePoint extends Renderable {
}

object ComparePoints {

  case object DefaultCP extends ComparePoint {
    override def render: String = "";
  }
  case class EqualCP(target: Int) extends ComparePoint {
    override def render: String = s"$target"; // =-sign is optional
  }
  case class GeqCP(target: Int) extends ComparePoint {
    override def render: String = s">$target";
  }
  case class LeqCP(target: Int) extends ComparePoint {
    override def render: String = s"<$target";
  }
}

// TODO roll templates

sealed trait RollModifier extends Renderable {
  import ComparePoints._
  import RollModifiers._

  def ++(mod: RollModifier): RollModifier = ModifierSeq(Seq(this, mod));
  def <(target: Int): RollModifier = this ++ TargetRoll(LeqCP(target));
  def >(target: Int): RollModifier = this ++ TargetRoll(GeqCP(target));
  def `=`(target: Int): RollModifier = this ++ TargetRoll(EqualCP(target));
  def cs(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CriticalSuccess(cp));
  def cf(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CriticalFailure(cp));
  def f(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ FailuresRoll(cp));
  def !(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ ExplodingRoll(cp));
  def !!(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ CompoundingRoll(cp));
  def `!p`(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ PenetratingRoll(cp));
  def r(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ RerollRoll(cp));
  def ro(): ModifierLackingCP[RollModifier] = MLCP(cp => this ++ RerollOnceRoll(cp));
  def kh(target: Int): RollModifier = this ++ KeepHighestRoll(target);
  def kl(target: Int): RollModifier = this ++ KeepLowestRoll(target);
  def dh(target: Int): RollModifier = this ++ DropHighestRoll(target);
  def dl(target: Int): RollModifier = this ++ DropLowestRoll(target);
  def s(): RollModifier = this ++ SortAscRoll;
  def sa(): RollModifier = this ++ SortAscRoll;
  def sd(): RollModifier = this ++ SortDescRoll;
}

object RollModifiers {

  import ComparePoints._

  class ModifierLackingCP[Out](val completer: ComparePoint => Out) {

    def <(target: Int): Out = completer(LeqCP(target));
    def >(target: Int): Out = completer(GeqCP(target));
    def `=`(target: Int): Out = completer(EqualCP(target));
    def apply(target: Int): Out = completer(EqualCP(target));
    def apply(cp: ComparePoint): Out = completer(cp);
  }

  object MLCP {
    def apply(completer: ComparePoint => RollModifier): ModifierLackingCP[RollModifier] = new ModifierLackingCP[RollModifier](completer);
  }

  case class TargetRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"${cp.render}";
  }

  case class CriticalSuccess(cp: ComparePoint) extends RollModifier {
    override def render: String = s"cs${cp.render}";
  }

  case class CriticalFailure(cp: ComparePoint) extends RollModifier {
    override def render: String = s"cf${cp.render}";
  }

  case class FailuresRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"f${cp.render}";
  }

  case class ExplodingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!${cp.render}";
  }

  case class CompoundingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!!${cp.render}";
  }

  case class PenetratingRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"!p${cp.render}";
  }

  case class KeepHighestRoll(n: Int) extends RollModifier {
    override def render: String = s"kh$n";
  }

  case class KeepLowestRoll(n: Int) extends RollModifier {
    override def render: String = s"kl$n";
  }

  case class DropHighestRoll(n: Int) extends RollModifier {
    override def render: String = s"dh$n";
  }

  case class DropLowestRoll(n: Int) extends RollModifier {
    override def render: String = s"dl$n";
  }

  case class RerollRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"r${cp.render}";
  }

  case class RerollOnceRoll(cp: ComparePoint) extends RollModifier {
    override def render: String = s"ro${cp.render}";
  }

  case object SortAscRoll extends RollModifier {
    override def render: String = s"sa";
  }

  case object SortDescRoll extends RollModifier {
    override def render: String = s"sd";
  }

  case class ModifierSeq(mods: Seq[RollModifier]) extends RollModifier {
    override def render: String = s"${mods.map(_.render).mkString}";
    override def ++(mod: RollModifier): RollModifier = ModifierSeq(mods :+ mod);
  }

}

case class RollField[T: Numeric](ctx: RenderingContext, attr: String, formula: RollExpression[T]) extends FieldLike[RollExpression[T]] {
  override def editable(): Boolean = false; // for now...maybe change later
  override def name: String = s"attr_${attr}";
  override def initialValue: String = formula.render;
  override def reader: Readable[RollExpression[T]] = ???; // TODO this is tricky...

  def inline: Roll = Rolls.InlineRoll(formula);
  def roll: Roll = Rolls.SimpleRoll(formula);
}

case class Button(ctx: RenderingContext, attr: String, roll: Roll) extends FieldLike[Roll] {
  override def editable(): Boolean = false; // not really relevant
  override def name: String = s"roll_${qualifiedAttr}";
  override def initialValue: String = roll.render;
  override def reader: Readable[Roll] = ???; // TODO this is tricky...
}

case class CommandButton(label: String, button: Button) extends Renderable {
  override def render: String = s"[$label](~${button.accessor})";
}

case class APIButton(label: String, command: Renderable) extends Renderable {
  override def render: String = s"[$label](!${command.render})";
}

object APIButton {
  def from(label: String, formula: RollExpression[Int]): APIButton = {
    APIButton(label, RollAsAPI(formula))
  }
}

/**
 * Note that this doesn't really work at the moment, as the template doesn't unescape things again, but fails if they are not escaped -.-
 */
case class RollAsAPI(formula: RollExpression[Int]) extends Renderable {
  override def render: String = s"&#13;/roll ${escapedFormula}";

  lazy val escapedFormula: String = {
    val fS = formula.render;
    val sb = new StringBuilder();
    fS.foreach { c =>
      c match {
        case '%' => sb.append("&#37;")
        case ')' => sb.append("&#41;")
        case '?' => sb.append("&#63;")
        case '@' => sb.append("&#64;")
        case '[' => sb.append("&#91;")
        case ']' => sb.append("&#93;")
        case _   => sb.append(c)
      }
    }
    sb.toString;
  }
}

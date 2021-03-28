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

sealed trait AutocalcExpression[T] extends Renderable {

  import AutocalcExprs._;
  import AccessTransformation._;
  import Arith.AutoArith;

  def +(other: AutocalcExpression[T])(implicit n: Numeric[T]) = AutoArith(this) + AutoArith(other);
  def -(other: AutocalcExpression[T])(implicit n: Numeric[T]) = AutoArith(this) - AutoArith(other);
  def /(other: AutocalcExpression[T])(implicit n: Numeric[T]) = AutoArith(this) / AutoArith(other);
  def *(other: AutocalcExpression[T])(implicit n: Numeric[T]) = AutoArith(this) * AutoArith(other);
  def %(other: AutocalcExpression[T])(implicit n: Numeric[T]) = AutoArith(this) % AutoArith(other);

  def +(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = AutoArith(this) + other;
  def -(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = AutoArith(this) - other;
  def /(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = AutoArith(this) / other;
  def *(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = AutoArith(this) * other;
  def %(other: ArithmeticExpression[T])(implicit n: Numeric[T]) = AutoArith(this) % other;

  def as[C]: AutocalcExpression[C] = Cast[T, C](this);

  def transformForAccess(f: AccessTransformer): AutocalcExpression[T];
  def forCharacter(characterName: String): AutocalcExpression[T] = transformForAccess(Character(characterName));
  def forSelected(): AutocalcExpression[T] = transformForAccess(Selected);
  def forTarget(): AutocalcExpression[T] = transformForAccess(Targeted);
  def forTarget(targetName: String): AutocalcExpression[T] = transformForAccess(Target(targetName));

  def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T];
}

object AutocalcExprs {
  import AccessTransformation.AccessTransformer;
  import Arith.AutoArith;

  sealed trait AccessContext {
    def access[T](field: FieldLike[T]): String;
  }

  case object LocalAccess extends AccessContext {
    override def access[T](field: FieldLike[T]): String = field.qualifiedAttr;
  }

  case object GlobalAccess extends AccessContext {
    override def access[T](field: FieldLike[T]): String = field.accessor;
  }

  case class RowAccess(rowId: String) extends AccessContext {
    override def access[T](field: FieldLike[T]): String = field.accessor(rowId);
  }

  sealed trait FieldAccessVariant[T] extends AutocalcExpression[T] {
    def labelled: Boolean;
    def field: FieldLike[T];
    def ctx: AccessContext;
    def replaceContext(newCtx: AccessContext): FieldAccessVariant[T];
    def access: String = {
      val raw = ctx.access(field);
      if (field.isMax) {
        raw.replace("_max", "|max");
      } else {
        raw
      }
    }
    def labelExtension: String =
      if (labelled) {
        s"[${field.attr.replace("_", " ")}]"
      } else {
        ""
      };
    def withLabelled(b: Boolean): FieldAccessVariant[T];

    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] =
      this; // field access have no subexpressions
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = f(this);
  }

  case class FieldAccess[T](field: FieldLike[T], labelled: Boolean, ctx: AccessContext = LocalAccess)
      extends FieldAccessVariant[T] {
    override def render: String = s"@{$access}" + labelExtension;
    override def replaceContext(newCtx: AccessContext): FieldAccessVariant[T] = FieldAccess(field, labelled, newCtx);
    override def withLabelled(b: Boolean): FieldAccessVariant[T] = FieldAccess(field, b, ctx);
    def selected = SelectedAttributeAccess(field, labelled);
    def target = TargetedAttributeAccess(field, None, labelled);
    def target(t: String) = TargetedAttributeAccess(field, Some(t), labelled);
    def character(characterName: String) = CharacterAttributeAccess(field, characterName, labelled);
  }

  case class TargetedAttributeAccess[T](field: FieldLike[T],
                                        target: Option[String],
                                        labelled: Boolean,
                                        ctx: AccessContext = LocalAccess
  ) extends FieldAccessVariant[T] {
    override def render: String = target match {
      case Some(t) => s"@{target|${t}|$access}" + labelExtension
      case None    => s"@{target|$access}" + labelExtension
    }
    override def replaceContext(newCtx: AccessContext): FieldAccessVariant[T] =
      TargetedAttributeAccess(field, target, labelled, newCtx);
    override def withLabelled(b: Boolean): FieldAccessVariant[T] = TargetedAttributeAccess(field, target, b, ctx);
  }

  case class SelectedAttributeAccess[T](field: FieldLike[T], labelled: Boolean, ctx: AccessContext = LocalAccess)
      extends FieldAccessVariant[T] {
    override def render: String = s"@{selected|$access}" + labelExtension;
    override def replaceContext(newCtx: AccessContext): FieldAccessVariant[T] =
      SelectedAttributeAccess(field, labelled, newCtx);
    override def withLabelled(b: Boolean): FieldAccessVariant[T] = SelectedAttributeAccess(field, b, ctx);
  }

  case class CharacterAttributeAccess[T](field: FieldLike[T],
                                         characterName: String,
                                         labelled: Boolean,
                                         ctx: AccessContext = LocalAccess
  ) extends FieldAccessVariant[T] {
    override def render: String = s"@{${characterName}|$access}" + labelExtension;
    override def replaceContext(newCtx: AccessContext): FieldAccessVariant[T] =
      CharacterAttributeAccess(field, characterName, labelled, newCtx);
    override def withLabelled(b: Boolean): FieldAccessVariant[T] =
      CharacterAttributeAccess(field, characterName, b, ctx);
  }

  sealed trait AbilityAccessVariant[T] extends AutocalcExpression[T] {
    def name: String;
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] =
      this; // ability access have no subexpressions
  }

  case class Ability[T](name: String) extends AbilityAccessVariant[T] {
    override def render: String = s"%{${name}}";
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = f(this);
    def selected = SelectedAbilityAccess(name);
    def target = TargetedAbilityAccess(name, None);
    def target(t: String) = TargetedAbilityAccess(name, Some(t));
  }

  case class TargetedAbilityAccess[T](name: String, target: Option[String]) extends AbilityAccessVariant[T] {
    override def render: String = target match {
      case Some(t) => s"%{target|${t}|${name}}"
      case None    => s"%{target|${name}}"
    }
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = f(Ability[T](name));
  }

  case class SelectedAbilityAccess[T](name: String) extends AbilityAccessVariant[T] {
    override def render: String = s"%{selected|${name}}";
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = f(Ability[T](name));
  }

  case class CharacterAbilityAccess[T](name: String, characterName: String) extends AbilityAccessVariant[T] {
    override def render: String = s"%{${characterName}|${name}}";
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = f(Ability[T](name));
  }

  case class Literal[T](t: T) extends AutocalcExpression[T] {
    override def render: String = t.toString();
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = this;
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] = this;
  }

  case class Cast[I, O](expr: AutocalcExpression[I]) extends AutocalcExpression[O] {
    override def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[O] =
      Cast[I, O](expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[O] =
      Cast[I, O](expr.replaceQuery(replacement));
  }

  case class Arithmetic[T](expr: ArithmeticExpression[T]) extends AutocalcExpression[T] {
    override def render: String = expr.render;
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] =
      Arithmetic(expr.transformForAccess(f));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] =
      Arithmetic(expr.replaceQuery(replacement));
  }

  case class Macro[T](name: String) extends AutocalcExpression[T] {
    override def render: String = s"#{${name}}";
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] = this;
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] = this;
  }

  case class NativeExpr[T](expr: String) extends AutocalcExpression[T] {
    override def render: String = expr;
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] =
      ???; // Can't guarantee that this will work
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] =
      ???; // Can't guarantee that this will work
  }

  def native[T](s: String) = NativeExpr[T](s);

  case class SeqExpr[T](exprs: Seq[AutocalcExpression[T]]) extends AutocalcExpression[T] {
    override def render: String = exprs.map(_.render).mkString;
    override def transformForAccess(f: AccessTransformer): AutocalcExpression[T] =
      SeqExpr(exprs.map(_.transformForAccess(f)));
    override def replaceQuery[QT](replacement: QueryReplacer[QT]): AutocalcExpression[T] =
      SeqExpr(exprs.map(_.replaceQuery(replacement)));
  }

  def exprs[T](expressions: AutocalcExpression[T]*) = SeqExpr(expressions);

  def ceil[T: Numeric](expr: AutocalcExpression[T]) = Arith.ceil(AutoArith(expr));
  def floor[T: Numeric](expr: AutocalcExpression[T]) = Arith.floor(AutoArith(expr));
  def round[T: Numeric](expr: AutocalcExpression[T]) = Arith.round(AutoArith(expr));

  def abs[T: Numeric](expr: AutocalcExpression[T]) = Arith.abs(AutoArith(expr));
}

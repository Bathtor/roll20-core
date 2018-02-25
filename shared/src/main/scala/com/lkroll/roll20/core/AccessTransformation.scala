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

object AccessTransformation {
  import AutocalcExprs._

  sealed trait AccessTransformer {
    type FieldOut[T] <: FieldAccessVariant[T];
    type AbilityOut[T] <: AbilityAccessVariant[T];
    def apply[T](field: FieldAccessVariant[T]): FieldOut[T];
    def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T];
  }

  object Identity extends AccessTransformer {
    override type FieldOut[T] = FieldAccessVariant[T];
    override type AbilityOut[T] = AbilityAccessVariant[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = field;
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = ability;
  }

  object Simple extends AccessTransformer {
    override type FieldOut[T] = FieldAccess[T];
    override type AbilityOut[T] = Ability[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = FieldAccess(field.field, field.labelled)
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = Ability(ability.name);
  }

  case class Character(characterName: String) extends AccessTransformer {
    override type FieldOut[T] = CharacterAttributeAccess[T];
    override type AbilityOut[T] = CharacterAbilityAccess[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = CharacterAttributeAccess(field.field, characterName, field.labelled);
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = CharacterAbilityAccess(ability.name, characterName);
  }

  object Selected extends AccessTransformer {
    override type FieldOut[T] = SelectedAttributeAccess[T];
    override type AbilityOut[T] = SelectedAbilityAccess[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = SelectedAttributeAccess(field.field, field.labelled);
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = SelectedAbilityAccess(ability.name);
  }

  object Targeted extends AccessTransformer {
    override type FieldOut[T] = TargetedAttributeAccess[T];
    override type AbilityOut[T] = TargetedAbilityAccess[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = TargetedAttributeAccess(field.field, None, field.labelled);
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = TargetedAbilityAccess(ability.name, None);
  }

  case class Target(targetName: String) extends AccessTransformer {
    override type FieldOut[T] = TargetedAttributeAccess[T];
    override type AbilityOut[T] = TargetedAbilityAccess[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = TargetedAttributeAccess(field.field, Some(targetName), field.labelled);
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = TargetedAbilityAccess(ability.name, Some(targetName));
  }

  case class AtRow(sectionSelect: RenderingContext => Boolean, rowId: String) extends AccessTransformer {
    override type FieldOut[T] = FieldAccessVariant[T];
    override type AbilityOut[T] = AbilityAccessVariant[T];
    override def apply[T](field: FieldAccessVariant[T]): FieldOut[T] = if (sectionSelect(field.field.ctx)) {
      field.replaceContext(RowAccess(rowId))
    } else {
      field
    }
    override def apply[T](ability: AbilityAccessVariant[T]): AbilityOut[T] = ability;
  }
}
